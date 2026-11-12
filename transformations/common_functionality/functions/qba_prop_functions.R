# Helper functions for the CS10 quantitative bias analysis for outcome misclassification (Brown et al. 2024).

#' Extract Risk Ratio and Confidence Interval from a 2x2 Table
#'
#' Calculates the risk ratio (RR) and corresponding confidence interval
#' using \code{epitools::riskratio()} and returns the estimate for the
#' non-reference exposure group.
#'
#' @param tab A 2 × 2 contingency table supplied as a matrix or table.
#'   Rows represent exposure groups and columns represent outcome groups.
#'   The first row is treated as the reference group.
#' @param conf.level Numeric value specifying the confidence level for the
#'   interval estimate. Default is \code{0.95}.
#'
#' @return A data frame with one row and three columns:
#' \describe{
#'   \item{RR}{Estimated risk ratio.}
#'   \item{CI_lower}{Lower confidence limit.}
#'   \item{CI_upper}{Upper confidence limit.}
#' }
#'
#' @examples
#' tab <- matrix(
#'   c(20, 80,
#'     10, 90),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#'
#' get_rr_ci(tab)
get_rr_ci <- function(tab, conf.level = 0.95) {
  if (!is.matrix(tab) && !is.table(tab)) {
    stop("Input must be a 2x2 matrix or table.")
  }
  
  if (!all(dim(tab) == c(2, 2))) {
    stop("Input must be a 2x2 table.")
  }
  
  rr <- epitools::riskratio(tab, conf.level = conf.level)
  
  data.frame(
    RR = rr$measure[2, "estimate"],
    CI_lower = rr$measure[2, "lower"],
    CI_upper = rr$measure[2, "upper"],
    row.names = NULL
  )
}

### Definitions
# Sensitivity: The probability that someone who truly had the outcome is recorded as having the outcome in the data. 
# 70-95% of all true outcomes (death/disease progression) are captured in the data 

# Specificity: The probability that someone who truly did not have the outcome is recorded as not having the outcome in the data.
# 90-95% means that very few patients are expected to be misclassified as having the outcome when they did not.

#' Check whether the assumed specificity range is compatible with the observed event rates.
#'
#' Negative corrected counts can occur when the false-positive rate
#' (1 - specificity) exceeds the observed event rate. This function
#' calculates the minimum specificity required to avoid negative corrected
#' counts and compares it with the user-specified plausible range.
#'
#' @param original_table data.table with counts a, b, c, d.
#' @param qba_parameters List containing the specificity parameter range.
#' @return Invisibly returns the minimum required specificity.

check_specificity_range <- function(
    original_table,
    qba_parameters
) {

  # Observed event rates
  event_rate_exposed <-
    original_table$a / (original_table$a + original_table$c)

  event_rate_control <-
    original_table$b / (original_table$b + original_table$d)

  # Minimum specificity required in each group
  min_specificity_exposed <- 1 - event_rate_exposed
  min_specificity_control <- 1 - event_rate_control

  # Most restrictive threshold
  min_specificity <-
    max(
      min_specificity_exposed,
      min_specificity_control
    )

  # Compare with the assumed range
  if (qba_parameters$specificity$upper < min_specificity) {

    warning(
      sprintf(
        paste(
          "The assumed specificity range (%.3f-%.3f) lies entirely",
          "below the minimum required specificity (%.3f).",
          "All simulations are expected to produce",
          "negative corrected counts."
        ),
        qba_parameters$specificity$lower,
        qba_parameters$specificity$upper,
        min_specificity
      )
    )

  } else if (qba_parameters$specificity$lower < min_specificity) {

    warning(
      sprintf(
        paste(
          "Part of the assumed specificity range (%.3f-%.3f)",
          "lies below the minimum required specificity (%.3f).",
          "Some simulations may produce",
          "negative corrected counts."
        ),
        qba_parameters$specificity$lower,
        qba_parameters$specificity$upper,
        min_specificity
      )
    )

  } else {

    message(
      sprintf(
        "The assumed specificity range (%.3f-%.3f) is compatible with the observed event rates (minimum required specificity = %.3f).",
        qba_parameters$specificity$lower,
        qba_parameters$specificity$upper,
        min_specificity
      )
    )

  }

  invisible(min_specificity)

}

#' Sample sensitivity from a truncated normal distribution.
#' 
#' @return Numeric sensitivity value.
sample_sensitivity <- function(qba_parameters) {

  truncnorm::rtruncnorm(
    1,
    a    = qba_parameters$sensitivity$lower,
    b    = qba_parameters$sensitivity$upper,
    mean = qba_parameters$sensitivity$mean,
    sd   = qba_parameters$sensitivity$sd
  )

}

#' Sample specificity from a truncated normal distribution.
#' 
#' @return Numeric specificity value.
sample_specificity <- function(qba_parameters) {

  truncnorm::rtruncnorm(
    1,
    a    = qba_parameters$specificity$lower,
    b    = qba_parameters$specificity$upper,
    mean = qba_parameters$specificity$mean,
    sd   = qba_parameters$specificity$sd
  )

}

#' Correct a 2x2 table for outcome misclassification using the sensitivity and specificity values.
#' 
#' @param table data.table with counts a, b, c, d corresponding to the 2x2 table.
#' @param sensitivity Numeric sensitivity value.
#' @param specificity Numeric specificity value.
#' @return data.table with corrected counts A, B, C, D corresponding to the corrected 2x2 table.
correct_outcome_misclassification <- function(
    table,
    sensitivity,
    specificity
) {

  a <- table$a
  b <- table$b
  c <- table$c
  d <- table$d

# denominator for A and B calculations
denom <- sensitivity + specificity - 1

if (denom <= 0) {
  stop("Sensitivity + specificity must be > 1.")
}

  A <- (a - (a + c) * (1 - specificity)) / denom
  B <- (b - (b + d) * (1 - specificity)) /denom
  C <- (a + c) - A
  D <- (b + d) - B
  
  # check if all counts are non-negatived
  valid <- all(c(A, B, C, D) >= 0)

  data.table::data.table(
    A = A,
    B = B,
    C = C,
    D = D,
    valid = valid
  )

}

#' Calculate the effect measure (risk ratio) from a corrected 2x2 table.  
#' 
#' @param corrected_table data.table with corrected counts A, B, C, D corresponding to the corrected 2x2 table.
#' @return Numeric risk ratio.
calculate_risk_ratio <- function(corrected_table) {

  risk_exp <-
    corrected_table$A /
    (corrected_table$A + corrected_table$C)

  risk_ctrl <-
    corrected_table$B /
    (corrected_table$B + corrected_table$D)

  risk_exp / risk_ctrl

}

#' Run a single iteration of the quantitative bias analysis for outcome misclassification.
#' If that iteration produces invalid corrected counts (negative values), the estimate is set to NA and valid is set to FALSE.
#' 
#' @param observed_table data.table with counts a, b, c, d corresponding to the observed 2x2 table.
#' @param qba_parameters list with n_sim, sensitivity and specificity
#' @return data.table with sensitivity, specificity, estimate, and valid column.
run_qba_iteration <- function(
    observed_table,
    qba_parameters
) {

  sensitivity <- sample_sensitivity(qba_parameters)
  specificity <- sample_specificity(qba_parameters)

  corrected_table <-

    correct_outcome_misclassification(
      table = observed_table,
      sensitivity = sensitivity,
      specificity = specificity
    )
  
    if (!corrected_table$valid) {

  return(
    data.table::data.table(
      sensitivity = sensitivity,
      specificity = specificity,
      A = corrected_table$A,
      B = corrected_table$B,
      C = corrected_table$C,
      D = corrected_table$D,
      estimate = NA_real_,
      valid = FALSE
    )
  )

  }

  estimate <- calculate_risk_ratio(corrected_table)

  data.table::data.table(
    sensitivity = sensitivity,
    specificity = specificity,
    A = corrected_table$A,
    B = corrected_table$B,
    C = corrected_table$C,
    D = corrected_table$D,
    estimate = estimate,
    valid = TRUE
  )

}

#' Run the quantitative bias analysis for outcome misclassification for a specified number of simulations.
#'
#' @param observed_table data.table with counts a, b, c, d corresponding to the observed 2x2 table.
#' @param qba_parameters list with n_sim, sensitivity and specificity
#' @return data.table with sensitivity, specificity, estimate, and valid column for each simulation.
run_qba <- function(
    observed_table,
    qba_parameters
) {

  data.table::rbindlist(

    lapply(
      seq_len(qba_parameters$n_sim),

      function(i)
        run_qba_iteration(
          observed_table,
          qba_parameters
        )
    )
  )

}

#' Summarise the results of the quantitative bias analysis simulations.
#' If a simulation produced invalid corrected counts (negative values), it is excluded from the summary statistics.
#' 
#' @param simulations data.table with sensitivity, specificity, estimate, and valid column for each simulation.
#' @return data.table with median estimate, 95% CI, number of valid and invalid simulations, and percentage of invalid simulations.
summarise_qba <- function(simulations) {

  simulations[, .(

    estimate = median(
      estimate,
      na.rm = TRUE
    ),

  lower = unname(
    quantile(
      estimate,
      0.025,
      na.rm = TRUE
    )
  ),

  upper = unname(
    quantile(
      estimate,
      0.975,
      na.rm = TRUE
    )
  ),

    n_valid = sum(valid),
    n_invalid = sum(!valid),
    pct_invalid = mean(!valid) * 100

  )]

}

#' Plot a histogram of the sensitivity and specificity distributions sampled for the iterations of the quantitative bias analysis.
#' 
#' @param simulations data.table with sensitivity, specificity, estimate, and valid column for each simulation.
#' @return ggplot object with histogram of sensitivity and specificity distributions.
plot_sensitivity_specificity_histogram <- function(
    simulations, valid = FALSE
) {   
  if (valid) {
    simulations <- simulations[valid == TRUE]
  } else {
    simulations <- simulations
  }

  sensitivity_plot <-
    ggplot2::ggplot(
      simulations,
      ggplot2::aes(x = sensitivity)
    ) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = "blue",
      alpha = 0.5
    ) +
    ggplot2::labs(
      title = "Sensitivity Distribution",
      x = "Sensitivity",
      y = "Count"
    )

  specificity_plot <-
    ggplot2::ggplot(
      simulations,
      ggplot2::aes(x = specificity)
    ) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = "green",
      alpha = 0.5
    ) +
    ggplot2::labs(
      title = "Specificity Distribution",
      x = "Specificity",
      y = "Count"
    )

  list(
    sensitivity_plot = sensitivity_plot,
    specificity_plot = specificity_plot
  )

}

#' Plot a histogram of the estimates sampled for the iterations
#' of the quantitative bias analysis.
#'
#' @param simulations data.table with sensitivity, specificity, estimate,
#' and valid column for each simulation.
#' @param valid logical indicating whether to include only valid simulations.
#' @return ggplot object with histogram of estimates.
plot_estimate_histogram <- function(
    simulations, valid = FALSE
) {
  
  if (valid) {
    simulations <- simulations[valid == TRUE]
  } else {
    simulations <- simulations
  }
  
  estimate_plot <-
    ggplot2::ggplot(
      simulations,
      ggplot2::aes(x = estimate)
    ) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = "purple",
      alpha = 0.5
    ) +
    ggplot2::labs(
      title = "Estimate Distribution",
      x = "Estimate",
      y = "Count"
    )
  
  estimate_plot
  
}
