#' Compute propensity scores and IPT weights
#'
#' Takes D4_StudyCohort_Covariates and metadata from psmodel_variables,
#' fits a propensity score model (with two-round fitting if needed),
#' and returns propensity scores,
#' stabilized IPT weights, and model diagnostics.
#'
#' @param d4_covariates data.table -- D4_StudyCohort_Covariates
#' @param ps_variables data.table -- psmodel_variables.csv metadata
#' @param ps_trim_bounds numeric vector c(lower, upper) or NULL (default).
#'   If supplied, individuals with p_score outside bounds are flagged via
#'   include_after_trimming = FALSE and excluded from weight calculation.
#' @param se_threshold numeric -- coefficients with SE above this value in
#'   Round 1 are flagged
#'
#' @return A named list with three elements:
#'   - D4_StudyCohort_iptw: person-level propensity scores and weights for the
#'                          matched cohort
#'   - D5_PS_Coef: list of point estimates and standard errors of coefficients
#'                 for the final propensity score model.
#'   - D5_PS_ModelBuilding: information on the model building procedure

compute_d4_studycohort_iptw <- function(
  d4_covariates,
  ps_variables,
  ps_trim_bounds = NULL,
  se_threshold = 10
) {
  # ---------------------------------------------------------------
  # Step 1: Data quality checks
  # ---------------------------------------------------------------

  # Select columns from metadata
  # Use force_out from metadata if present, default to FALSE
  model_building <- data.table::copy(ps_variables[, .(
    variable_id = var,
    var_type = vartype,
    force_in,
    force_out = if ("force_out" %in% names(ps_variables)) force_out else FALSE
  )])

  # Percentage of missing values
  model_building[,
    missing_pct := vapply(
      variable_id,
      function(v) {
        100 * sum(is.na(d4_covariates[[v]])) / nrow(d4_covariates)
      },
      numeric(1)
    )
  ]

  # Non-zero variance
  model_building[,
    nonzero_variance := vapply(
      variable_id,
      function(v) {
        length(unique(d4_covariates[[v]][!is.na(d4_covariates[[v]])])) >= 2
      },
      logical(1)
    )
  ]

  logger::log_info(
    "{msg}",
    msg = paste(
      "PS model candidates:",
      nrow(model_building),
      "variables.",
      sum(!model_building$nonzero_variance),
      "with zero variance.",
      "Missing % range:",
      round(min(model_building$missing_pct), 1),
      "-",
      round(max(model_building$missing_pct), 1)
    )
  )

  # ---------------------------------------------------------------
  # Step 2: Prepare data (variable type etc.)
  # ---------------------------------------------------------------

  model_data <- data.table::copy(d4_covariates)

  for (i in seq_len(nrow(ps_variables))) {
    v <- ps_variables$var[i]
    vtype <- ps_variables$vartype[i]
    refcat <- as.character(ps_variables$reference_category[i])

    if (vtype %in% c("CAT", "TF")) {
      data.table::set(model_data, j = v, value = as.factor(model_data[[v]]))
      if (!is.na(refcat) && refcat %in% levels(model_data[[v]])) {
        data.table::set(
          model_data,
          j = v,
          value = relevel(model_data[[v]], ref = refcat)
        )
      }
    }
  }

  # binary group indicator for ps model
  model_data[, group_binary := data.table::fifelse(group == "exposed", 1L, 0L)]

  # ---------------------------------------------------------------
  # Step 3: Fit Round 1 (initial PS model)
  # ---------------------------------------------------------------

  eligible_vars <- model_building[
    nonzero_variance == TRUE & force_out == FALSE,
    variable_id
  ]

  logger::log_info(paste(
    "Round 1: fitting PS model with",
    length(eligible_vars),
    "predictors"
  ))

  model_r1 <- fit_ps_model(
    predictors = eligible_vars,
    cohort_covariates = model_data,
    use_speedglm = FALSE
  )

  if (is.null(model_r1)) {
    stop("Round 1 PS model failed to fit. Check data and predictors.")
  }

  # ---------------------------------------------------------------
  # Step 4: Check if the model has fit correctly (Round 1)
  # ---------------------------------------------------------------

  # include_r1: was this variable included in the Round 1 model?
  model_building[, include_r1 := variable_id %in% eligible_vars]
  model_building[, reason_excluded := NA_character_]

  # reason for variables to not be in Round 1
  model_building[nonzero_variance == FALSE, reason_excluded := "zero_variance"]
  model_building[force_out == TRUE, reason_excluded := "force_out"]

  # Check for fit issues among R1 variables
  if (!model_r1$converged) {
    logger::log_warn("Round 1 model did not converge")
    model_building[include_r1 == TRUE, reason_excluded := "non_convergence"]
  }

  # Save information from round 1 model fit
  coef_summary_r1 <- as.data.frame(summary(model_r1)$coefficients)

  # flag variables from round 1 that have suspicious large SE
  for (v in eligible_vars) {
    coef_rows <- grep(paste0("^", v), rownames(coef_summary_r1))
    if (length(coef_rows) > 0) {
      max_se <- max(coef_summary_r1[coef_rows, "Std. Error"])
      if (max_se > se_threshold) {
        model_building[variable_id == v, reason_excluded := "large_se"]
        logger::log_warn(paste0(
          "Variable '",
          v,
          "' has large SE (",
          round(max_se, 2),
          ") -- possible perfect prediction"
        ))
      }
    }
  }

  # ---------------------------------------------------------------
  # Step 5: Round 2 -- refit if needed
  # ---------------------------------------------------------------

  # Variables with a reason_excluded after round 1
  problematic_vars <- model_building[
    include_r1 == TRUE & !is.na(reason_excluded),
    variable_id
  ]

  if (length(problematic_vars) > 0) {
    logger::log_info(paste(
      "Round 2: removing",
      length(problematic_vars),
      "problematic variable(s):",
      paste(problematic_vars, collapse = ", ")
    ))

    r2_predictors <- setdiff(eligible_vars, problematic_vars)

    if (length(r2_predictors) == 0) {
      stop("All variables excluded after Round 1. Cannot fit PS model.")
    }

    model_final <- fit_ps_model(
      predictors = r2_predictors,
      cohort_covariates = model_data,
      use_speedglm = FALSE
    )

    if (is.null(model_final)) {
      stop("Round 2 PS model failed to fit.")
    }

    model_building[, included_r2 := variable_id %in% r2_predictors]
  } else {
    logger::log_info("Round 1 fit clean -- no Round 2 needed")
    model_final <- model_r1

    model_building[, included_r2 := include_r1]
  }

  # ---------------------------------------------------------------
  # Step 6: Extract coefficients to D5_PS_Coef
  # ---------------------------------------------------------------

  coef_summary <- as.data.frame(summary(model_final)$coefficients)
  d5_ps_coef <- data.table::data.table(
    coefficient = rownames(coef_summary),
    estimate = coef_summary[, 1],
    std_error = coef_summary[, 2],
    z_score = coef_summary[, 3],
    p_value = coef_summary[, 4],
    exp_estimate = exp(coef_summary[, 1])
  )

  logger::log_info(paste(
    "D5_PS_Coef:",
    nrow(d5_ps_coef),
    "coefficients extracted"
  ))

  # ---------------------------------------------------------------
  # Step 7: Extract propensity scores
  # ---------------------------------------------------------------

  p_score <- predict(model_final, newdata = model_data, type = "response")

  n_na_ps <- sum(is.na(p_score))
  if (n_na_ps > 0) {
    logger::log_warn(paste(
      n_na_ps,
      "NA propensity scores due to missing covariate values.",
      "These individuals will be excluded from weight calculation."
    ))
  }

  # ---------------------------------------------------------------
  # Step 8: output + trimming
  # ---------------------------------------------------------------

  d4_iptw <- data.table::data.table(
    person_id = model_data$person_id,
    T0 = model_data$T0,
    group = model_data$group,
    group_binary = model_data$group_binary,
    p_score = p_score
  )

  if (!is.null(ps_trim_bounds)) {
    stopifnot(
      length(ps_trim_bounds) == 2,
      ps_trim_bounds[1] < ps_trim_bounds[2]
    )
    d4_iptw[,
      include_after_trimming := (!is.na(p_score) &
        p_score >= ps_trim_bounds[1] &
        p_score <= ps_trim_bounds[2])
    ]
    n_trimmed <- sum(!d4_iptw$include_after_trimming)
    logger::log_info(paste(
      "PS trimming applied: bounds [",
      ps_trim_bounds[1],
      ",",
      ps_trim_bounds[2],
      "].",
      n_trimmed,
      "individuals trimmed out of",
      nrow(d4_iptw)
    ))
  } else {
    d4_iptw[, include_after_trimming := !is.na(p_score)]
  }
  # ---------------------------------------------------------------
  # Step 9: Calculate ATT weights and stabilised weights on selected individuals
  # ---------------------------------------------------------------
  # Stabilization rescales raw weights
  # (which can be large) so they center around 1, reducing variance
  # without changing the causal estimate.
  # Formula: stabilized_iptw = P(A) / P(A|X)
  #   Exposed: P(A=1) / p_score
  #   Control: P(A=0) / (1 - p_score)
  selected <- d4_iptw[include_after_trimming == TRUE]
  n_selected <- nrow(selected)
  n_exposed <- sum(selected$group_binary == 1L)
  n_control <- n_selected - n_exposed
  p_a1 <- n_exposed / n_selected
  p_a0 <- 1 - p_a1

  logger::log_info(paste(
    "Stabilization: N =",
    n_selected,
    "(exposed:",
    n_exposed,
    "/ control:",
    n_control,
    ")",
    "| P(exposed) =",
    round(p_a1, 4)
  ))


  # ATT weights:
  #   Exposed: 1
  #   Control: PS / (1 - PS)
  d4_iptw[, raw_iptw := NA_real_]

  d4_iptw[
    include_after_trimming == TRUE,
    raw_iptw := data.table::fifelse(
      group_binary == 1L,
      1,
      p_score / (1 - p_score)
    )
  ]

  # Marginal probability of treatment actually received
  # P(A=1) for exposed, P(A=0) for control
  d4_iptw[, marginal_prob := NA_real_]
  d4_iptw[
    include_after_trimming == TRUE,
    marginal_prob := data.table::fifelse(
      group_binary == 1L,
      p_a1,
      p_a0
    )
  ]

  # Stabilized weights = marginal_prob * raw_iptw
  d4_iptw[, stabilized_iptw := NA_real_]
  d4_iptw[
    include_after_trimming == TRUE,
    stabilized_iptw := marginal_prob * raw_iptw
  ]

  # marginal_prob intermediate column
  d4_iptw[, marginal_prob := NULL]
  # ---------------------------------------------------------------
  # Step 10: Apply trunction of the stabilised weights at 1st and 99th percentile
  # ---------------------------------------------------------------
  d4_iptw[, trunc_iptw := NA_real_]

  for (grp in c("exposed", "control")) {
    idx <- d4_iptw$group == grp & d4_iptw$include_after_trimming == TRUE
    if (sum(idx) == 0) {
      next
    }

    q <- quantile(d4_iptw[idx, stabilized_iptw], probs = c(0.01, 0.99))
    d4_iptw[
      idx,
      # If IPTW less than 1st percentile set to 1st percentile
      trunc_iptw := data.table::fifelse(
        stabilized_iptw < q[1],
        q[1],
        # on the other end set to 99th percentile if larger than 99th
        data.table::fifelse(stabilized_iptw > q[2], q[2], stabilized_iptw)
      )
    ]

    logger::log_info(paste(
      "Truncation",
      grp,
      "-- weights truncated to [",
      round(q[1], 4),
      ",",
      round(q[2], 4),
      "]",
      "(P1-P99)"
    ))
  }

  # ---------------------------------------------------------------
  # Step 11: Select final columns
  # ---------------------------------------------------------------

  d4_iptw <- d4_iptw[, .(
    person_id,
    T0,
    group,
    group_binary,
    p_score,
    include_after_trimming,
    raw_iptw,
    stabilized_iptw,
    trunc_iptw
  )]

  # ---------------------------------------------------------------
  # Step 12: Finalize D5_PS_ModelBuilding
  # ---------------------------------------------------------------

  d5_model_building <- model_building[, .(
    variable_id,
    var_type,
    force_in,
    force_out,
    missing_pct,
    nonzero_variance,
    include_r1,
    included_r2,
    reason_excluded
  )]

  logger::log_info(paste(
    "D4_StudyCohort_IPTW:",
    nrow(d4_iptw),
    "rows |",
    "D5_PS_Coef:",
    nrow(d5_ps_coef),
    "rows |",
    "D5_PS_ModelBuilding:",
    nrow(d5_model_building),
    "rows"
  ))

  return(list(
    D4_StudyCohort_IPTW = d4_iptw,
    D5_PS_Coef = d5_ps_coef,
    D5_PS_ModelBuilding = d5_model_building
  ))
}
