## Crude and adjusted hazard ratios (95% CI) for the effect of CapOx-B vs CapOx 
## on disease progression or death (Estimand 2)

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_table_8_1_2.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Table 8.1 - crude and adjusted HR for estimand 2")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

dir_functions <- config_project$create_table_8_1_2.R$functions$dir_common
create_table_8_1_2 <- config_project$create_table_8_1_2.R
dir_d4 <- config_project$create_table_8_1_2.R$input$dir_d4
dir_d5 <- config_project$create_table_8_1_2.R$output$dir_d5

# Load the Study Cohort FUP dataset
# for matched and weightted cohort
file_path_fup <- file.path(dir_d4, "D4_StudyCohort_Analysis.parquet")
d4_fup <- picard::load(file_path_fup, sep = ";")
logger::log_info("D4 StudyCohort Analysis loaded")

# for calculations before matching we will use the 
d4_fup_unmatched <- picard::load(file.path(dir_d4, "D4_StudyCohortFUP_before_matching.parquet"), sep = ";")
logger::log_info("D4 StudyCohortFUP_before_matching loaded")

# ----------------------------
# LOAD FUNCTIONS
# ----------------------------
source(file.path(dir_functions, "outcome_analysis_fun.R"))
logger::log_info("functions sourced")

# ----------------------------
# TABLE 8.1
# ----------------------------
# include only people with include_trimming == TRUE for the matched and weighted analysis
d4_fup_tr <- d4_fup[include_after_trimming == TRUE]

table_8_1 <- HR_table_with_unweighted(
  dt_matched = d4_fup_tr,
  dt_unmatched = d4_fup_unmatched,
  dt_unw = d4_fup,
  fup_days = "fup_length_days_2",
  event_count = "eventCount_2",
  row_name = "Hazard ratio (95% CI) for estimand 2")

logger::log_info("Finished creating Table 8.1 with hazard ratios for each subgroup")

# ----------------------------
# TABLE 8.2
# ----------------------------
table_8_2 <- create_RMST_DIFF_table(dt_unmatched = d4_fup_unmatched, dt_matched = d4_fup)

logger::log_info("Finished creating Table 8.2 with RMST differences for each subgroup")
# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
  data = table_8_1,
  file_path = dir_d5,
  file_name = "Table_8_1.csv"
)
logger::log_info("Saving: Table 8.1")

picard::save(
  data = table_8_2,
  file_path = dir_d5,
  file_name = "Table_8_2.csv"
)
logger::log_info("Saving: Table 8.2")

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d4_fup, table_8_1, table_8_2)

logger::log_info("Completed: Table 8.1 created")
logger::log_info("Completed: Table 8.2 created")
