######################################################
### Common functionality for outcome analysis
######################################################

### Some are copied from Nik (cs 2) and some are new
### The ones copied from cs2 have been modified to fit the current analysis 
### For example, no IPTW in the current analysis, so the weighted argument is removed from some functions
### Or expanded to be able to produce estimated on the matched and unmatched datasets (for table 5, 8.2)

## Format confidence intervals
.fmt_ci <- function(est, lb, ub, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  out <- sprintf(paste0(fmt, " (", fmt, "-", fmt, ")"), est, lb, ub)
  out[is.na(est) | is.na(lb) | is.na(ub)] <- NA_character_
  out
}

## Format confidence intervals in weeks
.fmt_weeks <- function(est, lb, ub) {
  .fmt_ci(est / 7, lb / 7, ub / 7)
}

# ## A function that estimates the hazard ratio for a given dataset, follow-up variable, and event variable
# cox_hr <- function(dt, fup_days, event_count) {

#   na_out <- c(est = NA_real_, lb = NA_real_, ub = NA_real_)

#   if (data.table::uniqueN(dt$group) < 2L) {

#     logger::log_warn(
#       "Cannot estimate HR: only one treatment arm present (n={nrow(dt)})"
#     )

#     return(na_out)
#   }

#   events_by_group <- dt[
#     ,
#     .(n_events = sum(get(event_count), na.rm = TRUE)),
#     by = group
#   ]

#   if (any(events_by_group$n_events == 0)) {

#     logger::log_warn(
#       paste0(
#         "Cannot estimate HR: zero events in at least one treatment arm. ",
#         paste(
#           events_by_group[, paste0(group, "=", n_events)],
#           collapse = ", "
#         )
#       )
#     )

#     return(na_out)
#   }

#   fit <- tryCatch(
#     survival::coxph(
#       survival::Surv(
#         dt[[fup_days]],
#         dt[[event_count]]
#       ) ~ group,
#       data = dt,
#       weights = dt$ip_weights,
#       cluster = person_id,
#       robust = TRUE
#     ),
#     error = function(e) {

#       logger::log_warn(
#         "Cox model fitting failed: {conditionMessage(e)}"
#       )

#       NULL
#     }
#   )

#   if (is.null(fit)) {
#     return(na_out)
#   }

#   coefs <- summary(fit)$coefficients

#   log_hr <- coefs[1, "coef"]
#   se     <- coefs[1, "robust se"]

#   if (!is.finite(log_hr) || !is.finite(se)) {

#     logger::log_warn(
#       "Cox model returned non-finite estimates"
#     )

#     return(na_out)
#   }

#   hr <- exp(log_hr)

#   logger::log_info(
#     "Estimated HR={round(hr, 3)} (n={nrow(dt)})"
#   )

#   c(
#     est = hr,
#     lb  = exp(log_hr - 1.96 * se),
#     ub  = exp(log_hr + 1.96 * se)
#   )
# }


## A function that estimates the hazard ratio for a given dataset,
## follow-up variable, event variable, and optional weights
cox_hr <- function(dt, fup_days, event_count, weights = NULL) {

  na_out <- c(
    est = NA_real_,
    lb  = NA_real_,
    ub  = NA_real_
  )

  #----------------------------------------------------------
  # Check treatment arms
  #----------------------------------------------------------

  if (data.table::uniqueN(dt$group) < 2L) {

    logger::log_warn(
      "Cannot estimate HR: only one treatment arm present (n={nrow(dt)})"
    )

    return(na_out)
  }

  # Ensure control is the reference group
  dt[, group := factor(group, levels = c("control", "exposed"))]

  #----------------------------------------------------------
  # Check weights if supplied
  #----------------------------------------------------------

  if (!is.null(weights) && !weights %in% names(dt)) {

    logger::log_warn(
      "Weight column '{weights}' not found in dataset"
    )

    return(na_out)
  }

  #----------------------------------------------------------
  # Check events
  #----------------------------------------------------------

  events_by_group <- dt[
    ,
    .(n_events = sum(get(event_count), na.rm = TRUE)),
    by = group
  ]

  if (any(events_by_group$n_events == 0)) {

    logger::log_warn(
      paste0(
        "Cannot estimate HR: zero events in at least one treatment arm. ",
        paste(
          events_by_group[, paste0(group, "=", n_events)],
          collapse = ", "
        )
      )
    )

    return(na_out)
  }

  #----------------------------------------------------------
  # Fit Cox model
  #----------------------------------------------------------

  fit_args <- list(
    formula = survival::Surv(
      dt[[fup_days]],
      dt[[event_count]]
    ) ~ group,
    data = dt,
    cluster = dt$person_id,
    robust = TRUE
  )

  # Add weights only if supplied
  if (!is.null(weights)) {
    fit_args$weights <- dt[[weights]]
  }

  fit <- tryCatch(
    do.call(survival::coxph, fit_args),
    error = function(e) {

      logger::log_warn(
        "Cox model fitting failed: {conditionMessage(e)}"
      )

      NULL
    }
  )

  if (is.null(fit)) {
    return(na_out)
  }

  #----------------------------------------------------------
  # Extract coefficient and robust SE
  #----------------------------------------------------------

  coefs <- summary(fit)$coefficients

  log_hr <- coefs[1, "coef"]
  se     <- coefs[1, "robust se"]

  if (!is.finite(log_hr) || !is.finite(se)) {

    logger::log_warn(
      "Cox model returned non-finite estimates"
    )

    return(na_out)
  }

  #----------------------------------------------------------
  # Calculate HR and 95% CI
  #----------------------------------------------------------

  hr <- exp(log_hr)

  logger::log_info(
    "Estimated HR={round(hr, 3)} (n={nrow(dt)})"
  )

  c(
    est = hr,
    lb  = exp(log_hr - 1.96 * se),
    ub  = exp(log_hr + 1.96 * se)
  )
}

## A function that creates a table with the hazard ratios for the two treatment groups for Estimand 2 (table 8.1)
HR_table <- function(dt_matched,
                     dt_unmatched,
                     fup_days,
                     event_count,
                     row_name = "Hazard ratio (95% CI)") {

  hr_crude <- cox_hr(dt_unmatched, fup_days, event_count, weights = NULL)
  hr_adjusted <- cox_hr(dt_matched, fup_days, event_count, weights = "ip_weight")

  data.table::data.table(
    Comparison = "CapOx-B vs CapOx",
    Outcome = row_name,
    "HR (95% CI) - unmatched and unweighted" = .fmt_ci(hr_crude["est"], hr_crude["lb"], hr_crude["ub"]),
    "HR (95% CI) - matched and weighted" = .fmt_ci(hr_adjusted["est"], hr_adjusted["lb"], hr_adjusted["ub"])

  )
}

## A function that creates a table with crude, matched-unweighted, and matched-weighted hazard ratios
HR_table_with_unweighted <- function(dt_matched,
                                     dt_unmatched,
                                     dt_unw,
                                     fup_days,
                                     event_count,
                                     row_name = "Hazard ratio (95% CI)") {

  hr_crude <- cox_hr(dt_unmatched, fup_days, event_count, weights = NULL)
  hr_unw <- cox_hr(dt_unw, fup_days, event_count, weights = NULL)
  hr_adjusted <- cox_hr(dt_matched, fup_days, event_count, weights = "ip_weight")

  data.table::data.table(
    Comparison = "CapOx-B vs CapOx",
    Outcome = row_name,
    "HR (95% CI) - unmatched and unweighted" = .fmt_ci(hr_crude["est"], hr_crude["lb"], hr_crude["ub"]),
    "HR (95% CI) - matched and unweighted" = .fmt_ci(hr_unw["est"], hr_unw["lb"], hr_unw["ub"]),
    "HR (95% CI) - matched and weighted" = .fmt_ci(hr_adjusted["est"], hr_adjusted["lb"], hr_adjusted["ub"])

  )
}

## A function that creates a table with the hazard ratios for a given subgroup variable and its levels (Table 6.2)
HR_table_subgroup <- function(dt,
                              fup_days,
                              event_count,
                              row_name,
                              sub_group_var,
                              sub_group_lev) {

  data.table::rbindlist(

    lapply(sub_group_lev, function(i) {

      idx <- dt[[sub_group_var]] == i
      dt_sub <- dt[idx]

      logger::log_info(
        "Running subgroup analysis: subgroup={i}, n={nrow(dt_sub)}"
      )

      hr <- cox_hr(
        dt_sub,
        fup_days = fup_days,
        event_count = event_count
      )

      data.table::data.table(
        Comparison = "CapOx-B vs CapOx",
        Outcome = row_name,
        Subgroup = paste0(i, " (n=", nrow(dt_sub), ")"),
        `HR (95% CI)` = .fmt_ci(
          hr["est"],
          hr["lb"],
          hr["ub"]
        )
      )
    }),

    use.names = TRUE
  )
}

# ## Fit Weibull AFT for estimand 3
# .fit_weibull_aft <- function(dt) {
#   tryCatch(
#     flexsurv::flexsurvreg(
#       survival::Surv(fup_length_days_1_3, eventCount_1_3) ~ group,
#       data = dt,
#       dist = "weibull",
#       weights = dt$.w
#     ),
#     error = function(e) NULL
#   )
# }

## Fit Weibull AFT for estimand 3
.fit_weibull_aft <- function(dt, weights = NULL) {
  
  fit_args <- list(
    formula = survival::Surv(fup_length_days_1_3, eventCount_1_3) ~ group,
    data = dt,
    dist = "weibull"
  )
  
  # Add weights only if supplied
  if (!is.null(weights)) {
    fit_args$weights <- dt[[weights]]
  }
  
  tryCatch(
    do.call(flexsurv::flexsurvreg, fit_args),
    error = function(e) NULL
  )
}

## Compute RMST for a given set of taus
.rmst_by_horizon <- function(fit, taus) {
  na_vec <- rep(NA_character_, length(taus))
  if (is.null(fit)) {
    return(list(control = na_vec, exposed = na_vec, DIFF = na_vec))
  }
  ss <- data.table::as.data.table(suppressMessages(flexsurv::standsurv(
    fit,
    type = "rmst",
    t = taus,
    at = list(list(group = "control"), list(group = "exposed")),
    contrast = "difference",
    ci = TRUE,
    boot = TRUE,
    B = 1000
      )))
  ss <- ss[match(taus, time)]
  list(
    control = .fmt_weeks(ss$at1, ss$at1_lci, ss$at1_uci),
    exposed = .fmt_weeks(ss$at2, ss$at2_lci, ss$at2_uci),
    DIFF = .fmt_weeks(
      ss$contrast2_1,
      ss$contrast2_1_lci,
      ss$contrast2_1_uci
    )
  )
}

# ## Create a table with incidence rates and RMST for the three estimands (table 5)
# create_IR_RMST_table <-  function(
#     dt,
#     estimand_map,
#     year_length = 365.25,
#     rmst_taus = c(52, 104) * 7
# ) {

#   #----------------------------------------------------------
#   # Create incidence summaries
#   #----------------------------------------------------------

#   res_long <- data.table::rbindlist(
#     Map(
#       function(est, fup_col, event_col) {

#         dt[, {

#           n_patients <- .N

#         # Sum of all events (counting 1s in the event column)
#           n_events <- sum(
#             get(event_col),
#             na.rm = TRUE
#           )
#         # and sum of all follow-up days (counting the follow-up length for each patient)
#           total_fup_days <- sum(
#             get(fup_col),
#             na.rm = TRUE
#           )
#         # sum of all follw-up days divided by the length of a year gives the total person-years of follow-up
#           person_years <- total_fup_days / year_length

#         # Use the function from the Cohort Analytics package
#           ir <- CohortAnalytics::est_inc_prev(
#             n_pat = n_patients,
#             n_out = n_events,
#             py = person_years,
#             scale_IR = 1000, # still not sure if this should be 1000 or 100
#             type = "incidence"
#           )

#           .(
#             estimand = est,
#             n_patients = n_patients,
#             n_events = n_events,
#             person_years = round(person_years, 1),
#             incidence_rate = sprintf(
#               "%.2f (%.2f, %.2f)",
#               ir$ir_est,
#               ir$ir_lb,
#               ir$ir_ub
#             )
#           )

#         }, by = group]

#       },
#       estimand_map$estimand,
#       estimand_map$fup_var,
#       estimand_map$event_var
#     )
#   )

#   #----------------------------------------------------------
#   # Convert to wide format
#   #----------------------------------------------------------

#   out <- data.table::dcast(
#     res_long,
#     estimand ~ group,
#     value.var = c(
#       "n_patients",
#       "n_events",
#       "person_years",
#       "incidence_rate"
#     )
#   )

#   #----------------------------------------------------------
#   # RMST for Estimand 3
#   #----------------------------------------------------------
# # Fit Weibull AFT model to the data for Estimand 3 and compute RMST at specified horizons
#   fit <- .fit_weibull_aft(dt)

#   rmst <- .rmst_by_horizon(
#     fit = fit,
#     taus = rmst_taus
#   )

#   out[, `:=`(
#     rmst52_exposed  = NA_character_,
#     rmst104_exposed = NA_character_,
#     rmst52_control  = NA_character_,
#     rmst104_control = NA_character_
#   )]

#   out[
#     estimand == "Estimand 3",
#     `:=`(
#       rmst52_exposed  = rmst$exposed[1],
#       rmst104_exposed = rmst$exposed[2],
#       rmst52_control  = rmst$control[1],
#       rmst104_control = rmst$control[2]
#     )
#   ]

#   #----------------------------------------------------------
#   # Column order
#   # CapOx-B = exposed
#   # CapOx   = control
#   #----------------------------------------------------------

#   data.table::setcolorder(
#     out,
#     c(
#       "estimand",

#       "n_patients_exposed",
#       "n_events_exposed",
#       "person_years_exposed",
#       "incidence_rate_exposed",
#       "rmst52_exposed",
#       "rmst104_exposed",

#       "n_patients_control",
#       "n_events_control",
#       "person_years_control",
#       "incidence_rate_control",
#       "rmst52_control",
#       "rmst104_control"
#     )
#   )

#   out
# }

# optional weighted
create_IR_RMST_table <- function(
    dt,
    estimand_map,
    year_length = 365.25,
    rmst_taus = c(52, 104) * 7,
    weights = NULL
) {

  #----------------------------------------------------------
  # Create incidence summaries
  #----------------------------------------------------------

  res_long <- data.table::rbindlist(
    Map(
      function(est, fup_col, event_col) {

        dt[, {

          #----------------------------------------------------
          # Unweighted calculations
          #----------------------------------------------------

          if (is.null(weights)) {

            n_patients <- .N

            n_events <- sum(
              get(event_col),
              na.rm = TRUE
            )

            total_fup_days <- sum(
              get(fup_col),
              na.rm = TRUE
            )

          #----------------------------------------------------
          # Weighted calculations
          #----------------------------------------------------

          } else {

            w <- get(weights)

            n_patients <- sum(
              w,
              na.rm = TRUE
            )

            # round to have no decimals
            n_patients <- round(n_patients, 1)

            n_events <- sum(
              get(event_col) * w,
              na.rm = TRUE
            )

            total_fup_days <- sum(
              get(fup_col) * w,
              na.rm = TRUE
            )
          }

          #----------------------------------------------------
          # Person-years
          #----------------------------------------------------

          person_years <- total_fup_days / year_length

          #----------------------------------------------------
          # Incidence rate
          #----------------------------------------------------

          ir <- CohortAnalytics::est_inc_prev(
            n_pat = n_patients,
            n_out = n_events,
            py = person_years,
            scale_IR = 1000,
            type = "incidence"
          )

          .(
            estimand = est,
            n_patients = n_patients,
            n_events = n_events,
            person_years = round(person_years, 1),
            incidence_rate = sprintf(
              "%.2f (%.2f, %.2f)",
              ir$ir_est,
              ir$ir_lb,
              ir$ir_ub
            )
          )

        }, by = group]

      },
      estimand_map$estimand,
      estimand_map$fup_var,
      estimand_map$event_var
    )
  )

  #----------------------------------------------------------
  # Convert to wide format
  #----------------------------------------------------------

  out <- data.table::dcast(
    res_long,
    estimand ~ group,
    value.var = c(
      "n_patients",
      "n_events",
      "person_years",
      "incidence_rate"
    )
  )

  #----------------------------------------------------------
  # RMST for Estimand 3
  #----------------------------------------------------------

  fit <- .fit_weibull_aft(
    dt,
    weights = weights
  )

  rmst <- .rmst_by_horizon(
    fit = fit,
    taus = rmst_taus
  )

  out[, `:=`(
    rmst52_exposed  = NA_character_,
    rmst104_exposed = NA_character_,
    rmst52_control  = NA_character_,
    rmst104_control = NA_character_
  )]

  out[
    estimand == "Estimand 3",
    `:=`(
      rmst52_exposed  = rmst$exposed[1],
      rmst104_exposed = rmst$exposed[2],
      rmst52_control  = rmst$control[1],
      rmst104_control = rmst$control[2]
    )
  ]

  #----------------------------------------------------------
  # Column order
  #----------------------------------------------------------

  data.table::setcolorder(
    out,
    c(
      "estimand",

      "n_patients_exposed",
      "n_events_exposed",
      "person_years_exposed",
      "incidence_rate_exposed",
      "rmst52_exposed",
      "rmst104_exposed",

      "n_patients_control",
      "n_events_control",
      "person_years_control",
      "incidence_rate_control",
      "rmst52_control",
      "rmst104_control"
    )
  )

  out
}

## A function that creates a table with the difference in RMST between the two treatment groups for Estimand 3 (table 8.2)
create_RMST_DIFF_table <- function(dt_unmatched, dt_matched, rmst_taus = c(52, 104) * 7) {

   # fit the Weibull AFT model for both unmatched and matched datasets
  fit_unmatched <- .fit_weibull_aft(dt_unmatched, weights = NULL)
  fit_matched <- .fit_weibull_aft(dt_matched, weights = "ip_weight")

  # Extract the RMST estimates for both datasets at the specified horizons
  rmst_unmatched <- .rmst_by_horizon(
    fit = fit_unmatched,
    taus = rmst_taus
  )

  rmst_matched <- .rmst_by_horizon(
    fit = fit_matched,
    taus = rmst_taus
  )

  # output table
  data.table::data.table(
    Comparison = "CapOx-B vs CapOx",
    Outcome = "Difference in RMST (95% CI)",
    Horizon_weeks = c(52, 104),
    "Unmatched and unweighted difference in RMST" = rmst_unmatched$DIFF,
    "Matched and weighted difference in RMST" = rmst_matched$DIFF
  )
}


#' Create a 2x2 table from a data.table with binary group and outcome variables.
#' 
#' @param dt data.table with binary group and outcome variables.
#' @param group_var Name of the treatment group variable ("exposed" and "control").
#' @param outcome_var Name of the binary outcome variable (1 = event, 0 = no event).
#' @return data.table with counts a, b, c, d corresponding to the 2x2 table.
create_2x2_table <- function(
    dt,
    group_var = "group",
    outcome_var = "eventCount_1_3",
    weights = NULL
) {

  if (is.null(weights)){
  data.table::data.table(
    a = dt[get(group_var) == "exposed" & get(outcome_var) == 1, .N],
    b = dt[get(group_var) == "control" & get(outcome_var) == 1, .N],
    c = dt[get(group_var) == "exposed" & get(outcome_var) == 0, .N],
    d = dt[get(group_var) == "control" & get(outcome_var) == 0, .N]
  )
} else {
  data.table::data.table(
    a = sum(dt[get(group_var) == "exposed" & get(outcome_var) == 1, get(weights)], na.rm = TRUE),
    b = sum(dt[get(group_var) == "control" & get(outcome_var) == 1, get(weights)], na.rm = TRUE),
    c = sum(dt[get(group_var) == "exposed" & get(outcome_var) == 0, get(weights)], na.rm = TRUE),
    d = sum(dt[get(group_var) == "control" & get(outcome_var) == 0, get(weights)], na.rm = TRUE)
  )
}

}

# # Cumulative incidence table (addition to table 5)
# km_cuminc_table <- function(dt,
#                             time_col,
#                             event_col,
#                             group_col,
#                             weeks = c(52, 104),
#                             digits = 2) {

#   surv_fit <- survival::survfit(
#     survival::Surv(get(time_col), get(event_col)) ~ get(group_col),
#     data = dt
#   )

#   ## maximum follow-up per group
#   max_fup <- dt[, .(max_fup = max(get(time_col), na.rm = TRUE)), by = get(group_col)]
#   data.table::setnames(max_fup, "get", "group")

#   ## obtain KM estimates
#   s <- tryCatch(
#     summary(surv_fit, times = weeks * 7),
#     error = function(e) NULL
#   )

#   ## all requested group × week combinations
#   res <- data.table::CJ(
#     group = unique(dt[[group_col]]),
#     week = weeks
#   )

#   res[, value := NA_character_]

#   ## fill in estimates that exist
#   if (!is.null(s) && length(s$time) > 0) {

#     tmp <- data.table::data.table(
#       group = sub("^.*=", "", s$strata),
#       week = s$time / 7,
#       est = 1 - s$surv,
#       lb  = 1 - s$upper,
#       ub  = 1 - s$lower
#     )

#     tmp[, value := .fmt_ci(est, lb, ub, digits)]

#     res[tmp, value := i.value, on = .(group, week)]
#   }

#   ## log unavailable estimates
#   for (i in seq_len(nrow(max_fup))) {

#     grp <- max_fup$group[i]
#     max_week <- max_fup$max_fup[i] / 7

#     missing_weeks <- weeks[weeks > max_week]

#     if (length(missing_weeks) > 0) {

#       logger::log_warn(
#         "Maximum follow-up for group '{grp}' is {round(max_week, 1)} weeks ({round(max_fup$max_fup[i])} days). KM cumulative incidence at week(s) {paste(missing_weeks, collapse = ', ')} cannot be estimated; returning NA."
#       )
#     }
#   }

#   out <- data.table::dcast(
#     res,
#     . ~ group + week,
#     value.var = "value"
#   )

#   out[, . := NULL]

#   data.table::setnames(
#     out,
#     paste0("CI_", gsub("\\.", "_", names(out)))
#   )

#   ## ensure all expected columns exist
#   wanted <- c(
#     "CI_exposed_52", "CI_exposed_104",
#     "CI_control_52", "CI_control_104"
#   )

#   missing <- setdiff(wanted, names(out))
#   for (x in missing) out[, (x) := NA_character_]

#   data.table::setcolorder(out, wanted)

#   out[]
# }

# Cumulative incidence table (addition to table 5)
km_cuminc_table <- function(
    dt,
    time_col,
    event_col,
    group_col,
    weeks = c(52, 104),
    digits = 2,
    weights = NULL
) {

  #----------------------------------------------------------
  # Fit Kaplan-Meier
  #----------------------------------------------------------

  fit_args <- list(
    formula = survival::Surv(
      get(time_col),
      get(event_col)
    ) ~ get(group_col),
    data = dt
  )

  # Add weights only if supplied
  if (!is.null(weights)) {
    fit_args$weights <- dt[[weights]]
  }

  surv_fit <- do.call(
    survival::survfit,
    fit_args
  )

  #----------------------------------------------------------
  # Maximum follow-up per group
  #----------------------------------------------------------

  max_fup <- dt[
    ,
    .(max_fup = max(get(time_col), na.rm = TRUE)),
    by = get(group_col)
  ]

  data.table::setnames(max_fup, "get", "group")

  #----------------------------------------------------------
  # Obtain KM estimates
  #----------------------------------------------------------

  s <- tryCatch(
    summary(
      surv_fit,
      times = weeks * 7
    ),
    error = function(e) NULL
  )

  #----------------------------------------------------------
  # All requested group × week combinations
  #----------------------------------------------------------

  res <- data.table::CJ(
    group = unique(dt[[group_col]]),
    week = weeks
  )

  res[, value := NA_character_]

  #----------------------------------------------------------
  # Fill in estimates that exist
  #----------------------------------------------------------

  if (!is.null(s) && length(s$time) > 0) {

    tmp <- data.table::data.table(
      group = sub("^.*=", "", s$strata),
      week = s$time / 7,
      est = 1 - s$surv,
      lb  = 1 - s$upper,
      ub  = 1 - s$lower
    )

    tmp[, value := .fmt_ci(
      est,
      lb,
      ub,
      digits
    )]

    res[
      tmp,
      value := i.value,
      on = .(group, week)
    ]
  }

  #----------------------------------------------------------
  # Log unavailable estimates
  #----------------------------------------------------------

  for (i in seq_len(nrow(max_fup))) {

    grp <- max_fup$group[i]
    max_week <- max_fup$max_fup[i] / 7

    missing_weeks <- weeks[weeks > max_week]

    if (length(missing_weeks) > 0) {

      logger::log_warn(
        "Maximum follow-up for group '{grp}' is {round(max_week, 1)} weeks ({round(max_fup$max_fup[i])} days). KM cumulative incidence at week(s) {paste(missing_weeks, collapse = ', ')} cannot be estimated; returning NA."
      )
    }
  }

  #----------------------------------------------------------
  # Wide format
  #----------------------------------------------------------

  out <- data.table::dcast(
    res,
    . ~ group + week,
    value.var = "value"
  )

  out[, . := NULL]

  data.table::setnames(
    out,
    paste0("CI_", gsub("\\.", "_", names(out)))
  )

  #----------------------------------------------------------
  # Ensure all expected columns exist
  #----------------------------------------------------------

  wanted <- c(
    "CI_exposed_52", "CI_exposed_104",
    "CI_control_52", "CI_control_104"
  )

  missing <- setdiff(wanted, names(out))

  for (x in missing) {
    out[, (x) := NA_character_]
  }

  data.table::setcolorder(
    out,
    wanted
  )

  out[]
}
