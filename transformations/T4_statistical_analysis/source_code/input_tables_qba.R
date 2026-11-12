
# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("input_tables_qba.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Input tables for QBA analysis")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

dir_functions <- config_project$input_tables_qba.R$functions$dir_common
input_tables_qba <- config_project$input_tables_qba.R
dir_d4 <- config_project$input_tables_qba.R$input$dir_d4
dir_d5 <- config_project$input_tables_qba.R$output$dir_d5

# Load the Study Cohort FUP dataset
# for matched and weighted cohort
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

# ----------------------------
# 2x2 TABLES FOR QBA
# ----------------------------
# Create 2x2 table for matched + weighted cohort
# include only people with include_trimming == TRUE for the matched and weighted analysis
d4_fup <- d4_fup[include_after_trimming == TRUE]

table_2x2_matched_w <- create_2x2_table(
  dt = d4_fup,
  group_var = "group",
  outcome_var = "eventCount_1_3",
  weights = "ip_weight"
)
logger::log_info("2x2 table for matched + weighted cohort created")

table_2x2_matched_unw <- create_2x2_table(
  dt = d4_fup,
  group_var = "group",
  outcome_var = "eventCount_1_3",
  weights = NULL
)
logger::log_info("2x2 table for matched + unweighted cohort created")

# Create 2x2 table for unmatched + unweighted cohort
table_2x2_unmatched <- create_2x2_table(
  dt = d4_fup_unmatched,
  group_var = "group",
  outcome_var = "eventCount_1_3",
  weights = NULL
)
logger::log_info("2x2 table for unmatched + unweighted cohort created")

# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(table_2x2_matched_w, file.path(dir_d5, "table_2x2_matched_w.csv"), sep = ";")
picard::save(table_2x2_matched_unw, file.path(dir_d5, "table_2x2_matched_unw.csv"), sep = ";")
picard::save(table_2x2_unmatched, file.path(dir_d5, "table_2x2_unmatched.csv"), sep = ";")

# ----------------------------
# CLEAN UP ENVIRONMENT
# ----------------------------
rm(d4_fup, d4_fup_unmatched, table_2x2_matched_w, table_2x2_matched_unw, table_2x2_unmatched)
