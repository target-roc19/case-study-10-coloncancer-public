## Crude and adjusted hazard ratio (95% CI) for the effect of CapOx-B vs CapOx
##  on disease progression or death (Estimand 1)

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_table_6_1_2.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Table 6.1 Crude and adjusted Hazard Ratios for estimand 1")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

dir_functions <- config_project$create_table_6_1_2.R$functions$dir_common
create_table_6_1_2 <- config_project$create_table_6_1_2.R
dir_d4 <- config_project$create_table_6_1_2.R$input$dir_d4
dir_d5 <- config_project$create_table_6_1_2.R$output$dir_d5

# Load the Study Cohort FUP dataset
# for matched and weightted cohort
file_path_fup <- file.path(dir_d4, "D4_StudyCohort_Analysis.parquet")
d4_fup <- picard::load(file_path_fup, sep = ";")
logger::log_info("D4 StudyCohort Analysis loaded")

# for calculations before matching we will use the 
d4_fup_unmatched <- picard::load(file.path(dir_d4, "D4_StudyCohortFUP_before_matching.parquet"), sep = ";")
logger::log_info("D4 StudyCohortFUP_before_matching loaded")

# study cohort covariates
d4_cov <- picard::load(file.path(dir_d4, "D4_StudyCohortCovariates.parquet"), sep = ";")
logger::log_info("D4 StudyCohortCovariates loaded")

# ----------------------------
# LOAD FUNCTIONS
# ----------------------------
source(file.path(dir_functions, "outcome_analysis_fun.R"))
logger::log_info("functions sourced")
# ----------------------------
# TABLE 6.1
# ----------------------------
# include only people with include_trimming == TRUE for the matched and weighted analysis
d4_fup <- d4_fup[include_after_trimming == TRUE]

table_6_1 <- HR_table(
  dt_matched = d4_fup,
  dt_unmatched = d4_fup_unmatched,
  fup_days = "fup_length_days_1_3",
  event_count = "eventCount_1_3",
  row_name = "Hazard ratio (95% CI) for estimand 1"
)
logger::log_info("Table 6.1 created")

# ----------------------------
# TABLE 6.2
# ----------------------------
# # Create a subgroup variable for the analysis
# d4_cov[, sub_group_var := data.table::fcase(
#   SV_ECOG_STATUS == 0 & SV_METASTASIS_LIVER == FALSE, "Subgroup 1",
#   SV_ECOG_STATUS == 0 & SV_METASTASIS_LIVER == TRUE,  "Subgroup 2",
#   SV_ECOG_STATUS == 1 & SV_METASTASIS_LIVER == FALSE, "Subgroup 3",
#   SV_ECOG_STATUS == 1 & SV_METASTASIS_LIVER == TRUE,  "Subgroup 4"
#   )]
# logger::log_info("Subgroup variable created based on ECOG status and liver metastasis")

# # left join on d4_fup the sub_group_var from d4_cov
# d4_fup <- merge(
#   d4_fup,
#   d4_cov[, .(person_id, sub_group_var)],
#   by = "person_id",
#   all.x = TRUE
# )   

# # count the number of patients in each subgroup
# subgroup_counts <- d4_fup[, .N, by = sub_group_var]
# logger::log_info(
#   paste(
#     subgroup_counts[, paste0(sub_group_var, "=", N)],
#     collapse = ", "
#   )
# )

# # Apply the function to create the table with hazard ratios for each subgroup
# table_6_2 <- HR_table_subgroup(
#   dt = d4_fup,
#   fup_days = "fup_length_days_1_3",
#   event_count = "eventCount_1_3",
#   row_name = "Hazard ratio (95% CI) for estimand 1",
#   sub_group_var = "sub_group_var",
#   sub_group_lev = c("Subgroup 1", "Subgroup 2", "Subgroup 3", "Subgroup 4")
# )

# # transform the table to wide format
# dcast_table_6_2 <- data.table::dcast(
#   table_6_2,
#   Comparison + Outcome ~ Subgroup,
#   value.var = "HR (95% CI)"
# )
# logger::log_info("Finished creating Table 6.2 with hazard ratios for each subgroup")

# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
  data = table_6_1,
  file_path = dir_d5,
  file_name = "Table_6_1.csv"
)
logger::log_info("Saving: Table 6.1")

# picard::save(
#   data = dcast_table_6_2,
#   file_path = dir_d5,
#   file_name = "Table_6_2.csv"
# )
# logger::log_info("Saving: Table 6.2")

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d4_fup, d4_fup_unmatched, table_6_1, table_6_2, dcast_table_6_2, subgroup_counts)

logger::log_info("Completed: Table 6.1 created")
logger::log_info("Completed: Table 6.2 created")
