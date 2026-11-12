# ---------------------------------------------------------------
# Step: create_d4_StudyCohort_Analysis
#
# Input:  D4_StudyCohort_FUP
#         D4_StudyCohort_IPTW
#         D4_StudyCohort_Covariates
#
# Output: D4_StudyCohort_Analysis.parquet
# ---------------------------------------------------------------

# --- Initial setup of the environment ----

logger::log_info("[create_d4_StudyCohort_Analysis] - START")

# --- Config loading ---
picard::load_config()

cfg <- config_project$create_d4_StudyCohort_Analysis
dir_d4 <- config_project$outputs$dir_d4
functions_dir <- cfg$functions$dir_common

# --- Source functions ---
source(file.path(functions_dir, "create_d4_StudyCohort_Analysis_fun.R"))

# --- Read inputs ---
# Load the study cohort covariates
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) {
  # if dummy data are used
  file_path_covariates <- file.path(
    "tests",
    "testthat",
    "data",
    "D4_StudyCohort",
    paste0("D4_StudyCohort_Covariates", file_extension)
  )
} else {
  # for real data
  file_path_covariates <- file.path(
    cfg$input$dir_d4,
    paste0("D4_StudyCohortCovariates", file_extension)
  )
}

# load covariates
d4_covariates <- picard::load(file_path_covariates, sep = ";")
# load iptw
d4_iptw <- picard::load(
  file.path(cfg$input$dir_d4, "D4_StudyCohort_IPTW.parquet"),
  sep = ";"
)
# load fup
d4_fup <- picard::load(
  file.path(cfg$input$dir_d4, "D4_StudyCohortFUP.parquet"),
  sep = ";"
)

# --- Build analysis dataset ---
d4_analysis <- create_d4_studycohort_analysis(
  d4_fup = d4_fup,
  d4_iptw = d4_iptw,
  d4_cov = d4_covariates)

logger::log_info(paste(
  "D4_StudyCohort_Analysis:",
  nrow(d4_analysis),
  "rows |",
  sum(d4_analysis$include_after_trimming),
  "after trimming "
))

# --- Save output ---
picard::save(
  data = d4_analysis,
  file_path = dir_d4,
  file_name = "D4_StudyCohort_Analysis.parquet"
)

# --- Cleanup ---
rm(d4_fup, d4_iptw, d4_analysis)

logger::log_info("[create_d4_StudyCohort_Analysis] - SUCCEED")
