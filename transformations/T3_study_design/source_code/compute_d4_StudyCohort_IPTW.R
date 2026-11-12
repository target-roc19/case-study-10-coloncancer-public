# ---------------------------------------------------------------
# Step: compute_d4_StudyCohort_IPTW
#
# Input:  D4_StudyCohort_Covariates
#         psmodel_variables.csv
#
# Output: D4_StudyCohort_IPTW
#         <DEAP>_D5_PS_Coef.csv
#         <DEAP>_D5_PS_ModelBuilding.csv
#
# Orchestrator script to generate a propensity score model and compute IPTW.
# ---------------------------------------------------------------

# --- Initial setup of the environment ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("compute_d4_StudyCohort_IPTW.R")
  lm$start_capturing_prints()
}

logger::log_info("[compute_d4_StudyCohort_IPTW] - START")

# --- Config loading ---
picard::load_config()

deap_name <- config_project$DEAP_configuration$name
cfg <- config_project$compute_d4_StudyCohort_IPTW
functions_dir <- cfg$functions$dir_common

# --- Source functions ---
source(file.path(functions_dir, "fit_ps_model.R"))
source(file.path(functions_dir, "compute_d4_studycohort_iptw_function.R"))

# --- Read inputs ---
# Load the study cohort covariates
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) { # if dummy data are used
  file_path_covariates <- file.path(
    "tests", "testthat", "data", "D4_StudyCohort",
    paste0("D4_StudyCohort_Covariates", file_extension)
  )
} else { # for real data
  file_path_covariates <- file.path( 
    cfg$input$dir_d4,
    paste0("D4_StudyCohortCovariates", file_extension)
  )
}

d4_studycovariates <- picard::load(file_path_covariates, sep = ";")
ps_variables <- picard::load(file.path(cfg$input$dir_metadata, "psmodel_variables.csv"), sep = ",")

# --- Filter d4_covariates ---
# Filter covariates d4 data to control and exposed only using data.table
d4_studycovariates <- d4_studycovariates[group %in% c('control', 'exposed')]

# --- Compute ---
results <- compute_d4_studycohort_iptw(
  d4_covariates = d4_studycovariates,
  ps_variables = ps_variables,
  ps_trim_bounds = cfg$ps_trim_bounds,
  se_threshold = if (is.null(cfg$se_threshold)) 10 else cfg$se_threshold
)

# --- Save outputs ---
# D4 (analytic dataset): no DEAP prefix; each DAP owns its D4 directory
picard::save(
  data = results$D4_StudyCohort_IPTW,
  file_path = cfg$output$dir_d4,
  file_name = "D4_StudyCohort_IPTW.parquet"
)

# D5 (per-DAP results): prefixed with DEAP name for the central meta-analysis
picard::save(
  data = results$D5_PS_Coef,
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_PS_Coef.csv")
)

picard::save(
  data = results$D5_PS_ModelBuilding,
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_PS_ModelBuilding.csv")
)

logger::log_info(paste(
  "D4_StudyCohort_IPTW:",
  nrow(results$D4_StudyCohort_IPTW),
  "rows"
))

# --- Cleanup ---
rm(d4_studycovariates, ps_variables, results)

logger::log_info("[compute_d4_StudyCohort_IPTW] - SUCCEED")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
