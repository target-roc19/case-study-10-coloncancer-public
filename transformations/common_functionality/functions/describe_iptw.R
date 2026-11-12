#' Summarize Stabilized IPTW Weights by Group
#'
#' Creates a compact summary table of stabilized inverse probability of treatment
#' weights for each study group.
#'
#' @param p_matrix A data.table/data.frame containing at least `group` and the
#'   weight column specified in `stab_weight_col`.
#' @param groups Character vector of group names to summarize. Defaults to
#'   `c("control", "exposed")`.
#' @param stab_weight_col Character scalar; name of the stabilized weight
#'   column in `p_matrix`.
#' @param custom_truncation Numeric length-2 vector with optional lower and
#'   upper truncation cutoffs to report.
#' @param digits Integer; number of decimal places for rounded output.
#'
#' @return A data.frame with summary rows (mean/sd, quartiles, median, range,
#'   p1-p99, truncation cutoffs) and two columns per group.
#' @export
describe_iptw <- function(p_matrix,
                          groups = c("control", "exposed"), # names of groups
                          stab_weight_col = "IPTW_full_corr", # column name stabilised weights
                          custom_truncation = c(NA, NA), # if additional truncation used, specify here
                          digits = 2) {
  for (gname in groups) {
    # calculate statistics
    mean_weight <- mean(p_matrix[group == gname, get(stab_weight_col)])
    median_weight <- median(p_matrix[group == gname, get(stab_weight_col)])
    sd_weight <- sd(p_matrix[group == gname, get(stab_weight_col)])
    quants <- quantile(p_matrix[group == gname, get(stab_weight_col)], c(0.01, 0.25, 0.75, 0.99))
    min_weight <- min(p_matrix[group == gname, get(stab_weight_col)])
    max_weight <- max(p_matrix[group == gname, get(stab_weight_col)])

    # collect into a data.frame
    gtemp <- data.frame(
      labels = c(
        "Mean (SD)",
        "Q1-Q3",
        "Median",
        "Range, Min-Max",
        "P1-P99",
        "If truncated, cut-offs"
      ),
      V1 = round(c(
        mean_weight,
        quants[2],
        median_weight,
        min_weight,
        quants[1],
        custom_truncation[1]
      ), digits),
      V2 = round(c(
        sd_weight,
        quants[3],
        NA,
        max_weight,
        quants[4],
        custom_truncation[2]
      ), digits)
    )

    # change row names
    colnames(gtemp)[c(2, 3)] <- paste0(gname, c("_V1", "_V2"))

    if (gname == groups[1]) {
      gout <- gtemp
    } else {
      gout <- cbind(gout, gtemp[, -1])
    }
  } # end loop over groups

  return(gout)
}

#' Format a `describe_iptw()` summary into one column per treatment group
#'
#' Collapses the generic `<group>_V1` / `<group>_V2` pair into a single
#' report-ready string per row (`Mean (SD)` -> `"m (sd)"`, ranges -> `"lo - hi"`,
#' single-value rows -> `"v"`) and names the columns after the drugs.
#'
#' @param summary_tbl Output of `describe_iptw()`.
#' @param group_labels Named vector mapping group code -> drug label; order sets
#'   the column order.
#' @return data.table with `Characteristic` + one column per group.
#' @export
format_iptw_summary <- function(summary_tbl,
                                group_labels = c(
                                  exposed = "CapOx-B",
                                  control = "CapOx"
                                )) {
  dt <- data.table::as.data.table(summary_tbl)
  out <- data.table::data.table(Characteristic = dt$labels)
  for (g in names(group_labels)) {
    v1 <- dt[[paste0(g, "_V1")]]
    v2 <- dt[[paste0(g, "_V2")]]
    cell <- data.table::fifelse(
      dt$labels == "Mean (SD)",
      paste0(v1, " (", v2, ")"),
      data.table::fifelse(is.na(v2), as.character(v1), paste0(v1, " - ", v2))
    )
    out[, (group_labels[[g]]) := cell]
  }
  out[]
}

# #' Format fitted PS-model coefficients as an odds-ratio table
# #'
# #' Maps the raw glm coefficient names back to human labels and reference
# #' categories from `ps_variables`, emitting one `Characteristic` /
# #' `Odds Ratio (95% CI)` row per parameter (CAT variables get a header row, a
# #' reference row, and one row per modelled level). CI = `exp(estimate +/- 1.96 SE)`.
# #'
# #' @param d5_ps_coef D5_PS_Coef table (`coefficient`, `estimate`, `std_error`).
# #' @param ps_variables psmodel_variables metadata (`var`, `vartype`,
# #'   `reference_category`, `label`).
# #' @return data.table with `Characteristic`, `Odds Ratio (95% CI)`.
# #' @export
# format_ps_model_parameters <- function(d5_ps_coef, ps_variables) {
#   coef <- data.table::as.data.table(d5_ps_coef)[coefficient != "(Intercept)"]
#   coef[, `:=`(
#     or = exp(estimate),
#     lo = exp(estimate + stats::qnorm(0.025) * std_error),
#     hi = exp(estimate + stats::qnorm(0.975) * std_error)
#   )]
#   or_str <- function(cf) {
#     i <- match(cf, coef$coefficient)
#     if (is.na(i)) {
#       return(NA_character_)
#     }
#     sprintf("%.2f (%.2f-%.2f)", coef$or[i], coef$lo[i], coef$hi[i])
#   }

#   pv <- data.table::as.data.table(ps_variables)
#   rows <- vector("list", nrow(pv))
#   for (i in seq_len(nrow(pv))) {
#     var <- pv$var[i]
#     if (pv$vartype[i] == "CAT") {
#       levels_cf <- coef$coefficient[startsWith(coef$coefficient, var)]
#       rows[[i]] <- data.table::rbindlist(c(
#         list(data.table::data.table(
#           Characteristic = c(pv$label[i], paste0("  ", pv$reference_category[i])),
#           OR = c(NA_character_, "1.00 (ref.)")
#         )),
#         lapply(levels_cf, function(cf) {
#           data.table::data.table(
#             Characteristic = paste0("  ", sub(var, "", cf)),
#             OR = or_str(cf)
#           )
#         })
#       ))
#     } else {
#       cf <- if (pv$vartype[i] == "TF") paste0(var, "TRUE") else var
#       rows[[i]] <- data.table::data.table(
#         Characteristic = pv$label[i], OR = or_str(cf)
#       )
#     }
#   }
#   out <- data.table::rbindlist(rows)
#   data.table::setnames(out, "OR", "Odds Ratio (95% CI)")
#   out[]
# }

#' Format fitted PS-model coefficients as an odds-ratio table
#'
#' Maps raw glm coefficient names back to human labels and reference
#' categories from `ps_variables`, emitting one `Characteristic` /
#' `Odds Ratio (95% CI)` row per parameter.
#'
#' CAT variables get a header row, a reference row, and one row per
#' modelled level.
#'
#' TF variables get one row corresponding to the TRUE coefficient.
#'
#' NUM variables get one row if modelled linearly, or one row per
#' natural-spline basis coefficient if modelled using `splines::ns()`.
#'
#' CI = exp(estimate +/- 1.96 SE).
#'
#' @param d5_ps_coef D5_PS_Coef table (`coefficient`, `estimate`, `std_error`).
#' @param ps_variables psmodel_variables metadata (`var`, `vartype`,
#'   `reference_category`, `label`).
#' @return data.table with `Characteristic`, `Odds Ratio (95% CI)`.
#' @export
format_ps_model_parameters <- function(d5_ps_coef, ps_variables) {

  coef <- data.table::as.data.table(d5_ps_coef)[
    coefficient != "(Intercept)"
  ]

  coef[, `:=`(
    or = exp(estimate),
    lo = exp(estimate + stats::qnorm(0.025) * std_error),
    hi = exp(estimate + stats::qnorm(0.975) * std_error)
  )]

  or_str <- function(cf) {

    i <- match(cf, coef$coefficient)

    if (is.na(i)) {
      return(NA_character_)
    }

    sprintf(
      "%.2f (%.2f-%.2f)",
      coef$or[i],
      coef$lo[i],
      coef$hi[i]
    )
  }

  pv <- data.table::as.data.table(ps_variables)

  rows <- vector("list", nrow(pv))

  for (i in seq_len(nrow(pv))) {

    var <- pv$var[i]

    # ------------------------------------------------------------
    # CAT variables
    # ------------------------------------------------------------
    if (pv$vartype[i] == "CAT") {

      levels_cf <- coef$coefficient[
        startsWith(coef$coefficient, var)
      ]

      rows[[i]] <- data.table::rbindlist(
        c(
          list(
            data.table::data.table(
              Characteristic = c(
                pv$label[i],
                paste0("  ", pv$reference_category[i])
              ),
              OR = c(
                NA_character_,
                "1.00 (ref.)"
              )
            )
          ),
          lapply(levels_cf, function(cf) {
            data.table::data.table(
              Characteristic = paste0(
                "  ",
                sub(var, "", cf)
              ),
              OR = or_str(cf)
            )
          })
        )
      )

    # ------------------------------------------------------------
    # NUM variables
    # ------------------------------------------------------------
    } else if (pv$vartype[i] == "NUM") {

      # Look for natural-spline coefficients
      spline_prefix <- paste0(
        "splines::ns(",
        var,
        ","
      )

      spline_cf <- coef$coefficient[
        startsWith(coef$coefficient, spline_prefix)
      ]

      if (length(spline_cf) > 0) {

        # NUM modelled using natural splines
        rows[[i]] <- data.table::rbindlist(
          lapply(seq_along(spline_cf), function(j) {
            data.table::data.table(
              Characteristic = paste0(
                "  ",
                pv$label[i],
                " - spline ",
                j
              ),
              OR = or_str(spline_cf[j])
            )
          })
        )

      } else {

        # NUM modelled linearly
        rows[[i]] <- data.table::data.table(
          Characteristic = pv$label[i],
          OR = or_str(var)
        )
      }

    # ------------------------------------------------------------
    # TF variables
    # ------------------------------------------------------------
    } else {

      cf <- paste0(var, "TRUE")

      rows[[i]] <- data.table::data.table(
        Characteristic = pv$label[i],
        OR = or_str(cf)
      )
    }
  }

  out <- data.table::rbindlist(rows)

  data.table::setnames(
    out,
    "OR",
    "Odds Ratio (95% CI)"
  )

  out[]
}
