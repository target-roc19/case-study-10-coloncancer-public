# ---------------------------------------------------------- #
# ------------------ Useful summary functions -------------- #
# ---------------------------------------------------------- #

#' Create summary statistisc for a numeric vector
#'
#' This function computes descriptive summary statistics of a
#' numeric vector. It returns the median, IQR (q1, q3) min and
#' max.
#' The missing values are ignored in all calculations.
#' The numbers are rounded to 2 decimal points by default.
#' The formatting was avoided in purpose, and the user
#' can further wrangle the output in any desired way.
#' This function helps to avoid repeating these
#' calculations.
#' @param x A numeric vector for which summary statistics
#' will be calculated
#' @param n_dec Integer number of the decimals for rounding
summ_fun <- function(x, n_dec = 2) {
  x <- as.numeric(x)

  stats <- c(
    med = median(x, na.rm = TRUE),
    q1  = unname(quantile(x, 0.25, na.rm = TRUE)),
    q3  = unname(quantile(x, 0.75, na.rm = TRUE)),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )

  round(stats, n_dec)
}

#' Create summary statistics for a numeric vector within groups.
#'
#' This function computes descriptive summary statistics for a subset of a
#' numeric vector defined by one or more grouping values.
#'
#' The function uses `summ_fun()` internally to calculate the median,
#' quartiles (Q1, Q3), minimum, and maximum for the selected subset.
#'
#' If no observations match the specified grouping values, the function
#' returns NULL.
#'
#' @param num_vec A numeric vector for which summary statistics will be
#'   calculated.
#' @param group_vec A vector defining the grouping variable.
#' @param group_values A vector of values in `group_vec` used to define
#'   the subset of observations included in the summary calculations.
#' @param n_dec Integer specifying the number of decimal places used for
#'   rounding the output. Default is 2.
summ_fun_by_group <- function(num_vec,
                              group_vec,
                              group_values,
                              n_dec = 2) {
  idx <- group_vec %in% group_values
  if (!any(idx, na.rm = TRUE)) {
    return(NULL)
  }
  summ_fun(num_vec[idx], n_dec)
}

summarise_event <- function(data, date_var, label) {
  event_dates <- data[[date_var]]
  event_time <- as.numeric(event_dates - data$T0)

  n_event <- sum(!is.na(event_dates))
  n_total <- nrow(data)

  pct <- if (n_total > 0) round(100 * n_event / n_total, 2) else NA_real_

  med <- if (n_event > 0) round(median(event_time, na.rm = TRUE), 2) else NA_real_
  q1 <- if (n_event > 0) round(unname(quantile(event_time, 0.25, na.rm = TRUE)), 2) else NA_real_
  q3 <- if (n_event > 0) round(unname(quantile(event_time, 0.75, na.rm = TRUE)), 2) else NA_real_
  mn <- if (n_event > 0) round(min(event_time, na.rm = TRUE), 2) else NA_real_
  mx <- if (n_event > 0) round(max(event_time, na.rm = TRUE), 2) else NA_real_

  data.table::data.table(
    `N_%` = paste0(n_event, ", ", if (!is.na(pct)) format(pct, nsmall = 2) else NA_character_),
    median = if (!is.na(med)) format(med, nsmall = 2) else NA_character_,
    `min_max` = paste0(
      if (!is.na(mn)) format(mn, nsmall = 2) else NA_character_,
      ", ",
      if (!is.na(mx)) format(mx, nsmall = 2) else NA_character_
    ),
    `Q1_Q3` = paste0(
      if (!is.na(q1)) format(q1, nsmall = 2) else NA_character_,
      ", ",
      if (!is.na(q3)) format(q3, nsmall = 2) else NA_character_
    ),
    event = label
  )
}
