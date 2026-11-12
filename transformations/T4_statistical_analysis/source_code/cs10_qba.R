# =============================================================================
# TARGET CS10 — Quantitative bias analysis (misclassification of outcome)
# =============================================================================
#
# In this script we applied the quantitative bias analysis for outcome 
# misclassification to the CS10 study. 
# Using the paper of Brown et al. (2024) as a guide, we correct a 2x2 table for 
# outcome misclassification using the equations posted on the GitHub issue. 
# The idea is that we sample values for specificity and sensitivity,
# and then use those values to correct the counts in the 2x2 table.
# Since it is not possible to calculate the HRs anymore, we will calculate the 
# risk ratio instead.
#
# The specificity and sensitivity are assumed to be normally distributed with 
# the mean and standard deviation calculated from the plausible ranges. Also they
# are identical in both groups. We are using a truncated normal distriubtion:
#
# sample sensitivity from a truncated normal distribution with mean = 0.92, sd = 0.015 
# (which is (0.92 - 0.89) / 2), lower bound (a) = 0.89 and upper bound (b) = 0.95.
# sample specificity from a truncated normal distrubution with mean 0.925, sd = 0.0125
# (which is (0.925 - 0.9) / 2)), lower bound (a) = 0.9 and upper bound (b) = 0.95
#
# If any of the corrected counts (A, B, C, D) are negative, we flag this iteration as 
# invalid and do not include it in the final results.
#
# The effect measure is the risk ratio, which is calculated as (A / (A + C)) / (B / (B + D)),
# where A, B, C, D are the corrected counts in the 2x2 table.
# 
# We run 10,000 simulations to obtain the distribution of the risk ratio and 
# then summarize the results.
# 
# We do this for the matched and unmatched 2x2 tables, and then we report the
# results of the simulations, including the mean, median, and 95% confidence
#
# Brown et al., (2024) https://doi.org/10.1002/pds.70026
# =============================================================================

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("cs10_qba.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: QBA for outcome misclassification")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")

dir_functions <- config_project$cs10_qba.R$functions$dir_common
cs10_qba <- config_project$cs10_qba.R
dir_d5 <- config_project$cs10_qba.R$input$dir_d5

# Load data from D5_results
file_path_matched_w <- file.path(dir_d5, "table_2x2_matched_w.csv")
file_path_matched_unw <- file.path(dir_d5, "table_2x2_matched_unw.csv")
file_path_unmatched <- file.path(dir_d5, "table_2x2_unmatched.csv")

table_matched_w <- picard::load(file_path_matched_w, sep = ";")
table_matched_unw <- picard::load(file_path_matched_unw, sep = ";")
table_unmatched <- picard::load(file_path_unmatched, sep = ";") 

logger::log_info("table 2x2_matched_w.csv loaded")
logger::log_info("table 2x2_matched_unw.csv loaded")
logger::log_info("table 2x2_unmatched.csv loaded")

# a --> x=1, y=1
# b --> x=1, y=0
# c --> x=0, y=1
# d --> x=1, y=1

# set seed
qba_seed <- 1234

# ----------------------------
# LOAD FUNCTIONS
# ----------------------------
source(file.path(dir_functions, "qba_prop_functions.R"))
logger::log_info("functions sourced")

# QBA parameters list 
## matched weighted
qba_parameters_matched_w <- list(
    # Based on the issue on Github, the sensitivity and specificity are centeres at the midpoint of their
    # plausible ranges. That is why the mean is calculated as (lower + upper) / 2. 
    # The standard deviation is calculated as (upper - lower) / 4, still needs confirmation.
    n_sim = 10000,
    
    sensitivity = list(
      lower = 0.915,
      upper = 0.95,
      mean  = (0.915 + 0.95) / 2,
      sd    = (0.95 - 0.915) / 4
    ),
    
    specificity = list(
      lower = 0.90,
      upper = 0.95,
      mean  = (0.90 + 0.95) / 2,
      sd    = (0.95 - 0.90) / 4
    )
    
)
## matched unweighted
qba_parameters_matched_unw <- list(
  # Based on the issue on Github, the sensitivity and specificity are centeres at the midpoint of their
  # plausible ranges. That is why the mean is calculated as (lower + upper) / 2. 
  # The standard deviation is calculated as (upper - lower) / 4, still needs confirmation.
  n_sim = 10000,
  
  sensitivity = list(
    lower = 0.895,
    upper = 0.95,
    mean  = (0.895 + 0.95) / 2,
    sd    = (0.95 - 0.895) / 4
  ),
  
  specificity = list(
    lower = 0.90,
    upper = 0.95,
    mean  = (0.90 + 0.95) / 2,
    sd    = (0.95 - 0.90) / 4
  )
  
)
## unmatched
qba_parameters_unmatched <- list(
  # Based on the issue on Github, the sensitivity and specificity are centeres at the midpoint of their
  # plausible ranges. That is why the mean is calculated as (lower + upper) / 2. 
  # The standard deviation is calculated as (upper - lower) / 4, still needs confirmation.
  n_sim = 10000,
  
  sensitivity = list(
    lower = 0.905,
    upper = 0.95,
    mean  = (0.905 + 0.95) / 2,
    sd    = (0.95 - 0.905) / 4
  ),
  
  specificity = list(
    lower = 0.90,
    upper = 0.95,
    mean  = (0.90 + 0.95) / 2,
    sd    = (0.95 - 0.90) / 4
  )
  
)

# Checks:
# for the unmatched table
logger::log_info("Checking specificity range for unmatched table")
check_specificity_range(
  original_table = table_unmatched,
  qba_parameters = qba_parameters_unmatched
)

# for the matched weighted table
logger::log_info("Checking specificity range for matched_w table")
check_specificity_range(
  original_table = table_matched_w,
  qba_parameters = qba_parameters_matched_w
)

# for the matched unweighted table
logger::log_info("Checking specificity range for matched_unw table")
check_specificity_range(
  original_table = table_matched_unw,
  qba_parameters = qba_parameters_matched_unw
)

### For the matched_w
# obtain risk ratio (before adjusting for outcome misclass)
# (100/ (100 + 102.03)) / (12 / (12 + 9.81)) // risk exposed / risk unexposed
rr_matched_w <-
  get_rr_ci(matrix(unlist(table_matched_w), nrow = 2, byrow = TRUE))
# Run the quantitative bias analysis simulations
withr::with_seed(
  qba_seed + 1,
  { simulations_matched_w <-
      run_qba(table_matched_w, qba_parameters_matched_w)
  }
)

# Summarize the results of the simulations
summary_matched_w_qba <- summarise_qba(simulations_matched_w)
print(summary_matched_w_qba)

# Count how many valid simulations were performed (i.e., how many times the corrected counts were non-negative)
valid_simulations_count <- sum(simulations_matched_w$valid == TRUE)
message(paste("Number of valid simulations (non-negative corrected counts):", valid_simulations_count))

# Plot a histogram of the sensitivity and specificity distributions
plots_all_matched_w <-plot_sensitivity_specificity_histogram(simulations_matched_w, valid = FALSE)

# Plot a histogram of the sensitivity and specificity distributions for the valid iterations
plots_valid_matched_w <- plot_sensitivity_specificity_histogram(simulations_matched_w, valid = TRUE)

# Plots histogram of estimates
plots_estimates_matched_w <- plot_estimate_histogram(simulations_matched_w, valid = TRUE)

### For the matched_unw
# obtain risk ratio (before adjusting for outcome misclass)
# (200 / (200 + 198)) / (24 / (24 + 26)) // risk exposed / risk unexposed
rr_matched_unw <-
  get_rr_ci(matrix(unlist(table_matched_unw), nrow = 2, byrow = TRUE))
# Run the quantitative bias analysis simulations
withr::with_seed(
  qba_seed + 2,
  { simulations_matched_unw <-
    run_qba(table_matched_unw, qba_parameters_matched_unw)
  }
)

# Summarize the results of the simulations
summary_matched_unw_qba <- summarise_qba(simulations_matched_unw)
print(summary_matched_unw_qba)

# Count how many valid simulations were performed (i.e., how many times the corrected counts were non-negative)
valid_simulations_count <- sum(simulations_matched_unw$valid == TRUE)
message(paste("Number of valid simulations (non-negative corrected counts):", valid_simulations_count))

# Plot a histogram of the sensitivity and specificity distributions
plots_all_matched_unw <-plot_sensitivity_specificity_histogram(simulations_matched_unw, valid = FALSE)

# Plot a histogram of the sensitivity and specificity distributions for the valid iterations
plots_valid_matched_unw <- plot_sensitivity_specificity_histogram(simulations_matched_unw, valid = TRUE)

# Plots histogram of estimates
plots_estimates_matched_unw <- plot_estimate_histogram(simulations_matched_unw, valid = TRUE)

### For the unmatched:
# obtain risk ratio (before adjusting for outcome misclass)
# (235 / (235 + 212)) / (25 / (25 + 27)) // risk exposed / risk unexposed
rr_unmatched <-
  get_rr_ci(matrix(unlist(table_unmatched), nrow = 2, byrow = TRUE))
# Run the quantitative bias analysis simulations
withr::with_seed(
  qba_seed + 3,
  { simulations_unmatched <-
    run_qba(table_unmatched, qba_parameters_unmatched)
  }
)
# Summarize the results of the simulations
summary_unmatched_qba <- summarise_qba(simulations_unmatched)
print(summary_unmatched_qba)

# Count how many valid simulations were performed (i.e., how many times the corrected counts were non-negative)
valid_simulations_count <- sum(simulations_unmatched$valid == TRUE)
message(paste("Number of valid simulations (non-negative corrected counts):", valid_simulations_count))

# Plot a histogram of the sensitivity and specificity distributions
plots_all_unmatched <-plot_sensitivity_specificity_histogram(simulations_unmatched, valid = FALSE)

# Plot a histogram of the sensitivity and specificity distributions for the valid iterations
plots_valid_unmatched <- plot_sensitivity_specificity_histogram(simulations_unmatched, valid = TRUE)

# Plots histogram of estimates
plots_estimates_unmatched <- plot_estimate_histogram(simulations_unmatched, valid = TRUE)

# Create table reporting the results of the simulations for both matched and unmatched tables
results_table <- data.table::data.table(
  table_type = c("matched_w", "matched_unw", "unmatched"),
  rr_uncorr = c(rr_matched_w$RR, rr_matched_unw$RR, rr_unmatched$RR),
  rr_uncorr_lower = c(rr_matched_w$CI_lower, rr_matched_unw$CI_lower, rr_unmatched$CI_lower),
  rr_uncorr_upper = c(rr_matched_w$CI_upper, rr_matched_unw$CI_upper, rr_unmatched$CI_upper),
  sens_lower = c(qba_parameters_matched_w$sensitivity$lower, qba_parameters_matched_unw$sensitivity$lower, qba_parameters_unmatched$sensitivity$lower),
  sens_upper = c(qba_parameters_matched_w$sensitivity$upper, qba_parameters_matched_unw$sensitivity$upper, qba_parameters_unmatched$sensitivity$upper),
  spec_lower = c(qba_parameters_matched_w$specificity$lower, qba_parameters_matched_unw$specificity$lower, qba_parameters_unmatched$specificity$lower),
  spec_upper = c(qba_parameters_matched_w$specificity$upper, qba_parameters_matched_unw$specificity$upper, qba_parameters_unmatched$specificity$upper),
  median_estimate = c(summary_matched_w_qba$estimate, summary_matched_unw_qba$estimate, summary_unmatched_qba$estimate),
  lower_ci = c(summary_matched_w_qba$lower, summary_matched_unw_qba$lower, summary_unmatched_qba$lower),
  upper_ci = c(summary_matched_w_qba$upper, summary_matched_unw_qba$upper, summary_unmatched_qba$upper),
  n_valid = c(summary_matched_w_qba$n_valid, summary_matched_unw_qba$n_valid, summary_unmatched_qba$n_valid),
  n_invalid = c(summary_matched_w_qba$n_invalid, summary_matched_unw_qba$n_invalid, summary_unmatched_qba$n_invalid),
  pct_invalid = c(summary_matched_w_qba$pct_invalid, summary_matched_unw_qba$pct_invalid, summary_unmatched_qba$pct_invalid)
)

## Save the results table to a CSV file
output_file_path <- file.path(dir_d5, "qba_results_summary.csv")
picard::save(results_table, output_file_path, sep = ";")  

# save the plots to files
ggplot2::ggsave(file.path(dir_d5, "sensitivity_histogram_matched_w.png"), plots_all_matched_w$sensitivity_plot)
ggplot2::ggsave(file.path(dir_d5, "specificity_histogram_matched_w.png"), plots_all_matched_w$specificity_plot)
ggplot2::ggsave(file.path(dir_d5, "sensitivity_histogram_valid_matched_w.png"), plots_valid_matched_w$sensitivity_plot)
ggplot2::ggsave(file.path(dir_d5, "specificity_histogram_valid_matched_w.png"), plots_valid_matched_w$specificity_plot)

ggplot2::ggsave(file.path(dir_d5, "sensitivity_histogram_matched_unw.png"), plots_all_matched_unw$sensitivity_plot)
ggplot2::ggsave(file.path(dir_d5, "specificity_histogram_matched_unw.png"), plots_all_matched_unw$specificity_plot)
ggplot2::ggsave(file.path(dir_d5, "sensitivity_histogram_valid_matched_unw.png"), plots_valid_matched_unw$sensitivity_plot)
ggplot2::ggsave(file.path(dir_d5, "specificity_histogram_valid_matched_unw.png"), plots_valid_matched_unw$specificity_plot)

ggplot2::ggsave(file.path(dir_d5, "sensitivity_histogram_unmatched.png"), plots_all_unmatched$sensitivity_plot)
ggplot2::ggsave(file.path(dir_d5, "specificity_histogram_unmatched.png"), plots_all_unmatched$specificity_plot)
ggplot2::ggsave(file.path(dir_d5, "sensitivity_histogram_valid_unmatched.png"), plots_valid_unmatched$sensitivity_plot)
ggplot2::ggsave(file.path(dir_d5, "specificity_histogram_valid_unmatched.png"), plots_valid_unmatched$specificity_plot)

ggplot2::ggsave(file.path(dir_d5, "corr_estimates_matched_w.png"), plots_estimates_matched_w)
ggplot2::ggsave(file.path(dir_d5, "corr_estimates_matched_unw.png"), plots_estimates_matched_unw)
ggplot2::ggsave(file.path(dir_d5, "corr_estimates_unmatched.png"), plots_estimates_unmatched)

# clean environment
rm(qba_parameters_matched_w, qba_parameters_matched_unw, qba_parameters_unmatched, 
   table_matched_w, table_matched_unw, table_unmatched,
   simulations_matched_w, simulations_matched_unw, simulations_unmatched,
   summary_matched_w_qba, summary_matched_unw_qba, summary_unmatched_qba,
   plots_all_matched_w, plots_all_matched_unw,
   plots_valid_matched_w, plots_valid_matched_unw,
   plots_all_unmatched, plots_valid_unmatched,
   plots_estimates_matched_w, plots_estimates_matched_unw, plots_estimates_unmatched,
   rr_matched_w, rr_matched_unw, rr_unmatched)
