### Table 5 Crude Incidence Rates (per stimand) and restricted mean survival time
### by group (CapOx-B vs CapOx) for disease progression or death 

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_table_5.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Table 5 Crude Incidence Rates and Restricted Mean Survival Time")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

dir_functions <- config_project$create_table_5.R$functions$dir_common
create_table_5 <- config_project$create_table_5.R
dir_d4 <- config_project$create_table_5.R$input$dir_d4
dir_d5 <- config_project$create_table_5.R$output$dir_d5
year_length <- config_values$year_length

# Load the Study Cohort Analysis dataset (matched + weighted)
file_path_fup <- file.path(dir_d4, "D4_StudyCohort_Analysis.parquet")
d4_fup <- picard::load(file_path_fup)
logger::log_info("D4 StudyCohort_Analysis loaded")

# Load unmatched + unweighted dataset
file_path_fup_unmatched <- file.path(dir_d4, "D4_StudyCohortFUP_before_matching.parquet")
d4_fup_unmatched <- picard::load(file_path_fup_unmatched)
logger::log_info("D4 StudyCohortFUP_before_matching loaded")

# # Load matched + weighted dataset
# file_path_fup_matched <- file.path(dir_d4, "D4_StudyCohortFUP.parquet")
# d4_fup_matched <- picard::load(file_path_fup_matched)
# logger::log_info("D4 StudyCohortFUP (matched) loaded")

# ----------------------------
# LOAD FUNCTIONS
# ----------------------------
source(file.path(dir_functions, "outcome_analysis_fun.R"))
logger::log_info("functions sourced")

# ----------------------------
# TABLE 5
# ----------------------------
# Create a mapping of estimands to the corresponding follow-up and event variables
# estimand 1 and 3 share the same follow-up and event variables, while estimand 2 has its own.
# Estimand 3, additional to the IR, will also include the RMST analysis

estimand_map <- data.table::data.table(
  estimand = c(
    "Estimand 1",
    "Estimand 2",
    "Estimand 3"
  ),
  fup_var = c(
    "fup_length_days_1_3",
    "fup_length_days_2",
    "fup_length_days_1_3"
  ),
  event_var = c(
    "eventCount_1_3",
    "eventCount_2",
    "eventCount_1_3"
  )
)

# Apply the function to create the incidence rate and RMST table
# Unmatched and unweighted
table_5 <- create_IR_RMST_table(
  dt = d4_fup_unmatched,
  estimand_map = estimand_map
)

# include only people with include_trimming == TRUE for the matched and weighted analysis
d4_fup_tr <- d4_fup[include_after_trimming == TRUE]

# Macthed and Weighted
table_5_weighted <- create_IR_RMST_table(
  dt = d4_fup_tr, # use the trimmed dataset for the matched and weighted analysis
  estimand_map = estimand_map,
  weights = "ip_weight"
)

# Matched and Unweighted - no weights applied
table_5_unweighted <- create_IR_RMST_table(
  dt = d4_fup,
  estimand_map = estimand_map
)

# Calculate the Cumulative Incidence (CI) at 52 and 104 weeks for each estimand and group
## Estimand 1
# Unmatched and unweighted
ci_results_est1 <- km_cuminc_table(dt = d4_fup_unmatched,
                            time_col = "fup_length_days_1_3",
                            event_col = "eventCount_1_3",
                            group_col = "group")
# Matched and weighted  
ci_results_est1_w <- km_cuminc_table(dt = d4_fup_tr,
                            time_col = "fup_length_days_1_3",
                            event_col = "eventCount_1_3",
                            group_col = "group",
                            weights = "ip_weight")

# Matched and unweighted - no weights applied
ci_results_est1_unw <- km_cuminc_table(dt = d4_fup,
                            time_col = "fup_length_days_1_3",
                            event_col = "eventCount_1_3",
                            group_col = "group")  

## Estimand 2
# Unmatched and unweighted
ci_results_est2 <- km_cuminc_table(dt = d4_fup_unmatched,
                            time_col = "fup_length_days_2",
                            event_col = "eventCount_2",
                            group_col = "group")
# Matched and weighted  
ci_results_est2_w <- km_cuminc_table(dt = d4_fup_tr,
                            time_col = "fup_length_days_2",
                            event_col = "eventCount_2",
                            group_col = "group",
                            weights = "ip_weight")

ci_results_est2_unw <- km_cuminc_table(dt = d4_fup,
                            time_col = "fup_length_days_2",
                            event_col = "eventCount_2",
                            group_col = "group")

# Bind the results for all estimands (for estimand 3 add NA values) into a single data.table and add the estimand column
ci_results <- data.table::rbindlist(list(
  cbind(estimand = "Estimand 1", ci_results_est1),
  cbind(estimand = "Estimand 2", ci_results_est2)
))

ci_results_w <- data.table::rbindlist(list(
  cbind(estimand = "Estimand 1", ci_results_est1_w),
  cbind(estimand = "Estimand 2", ci_results_est2_w)
))

ci_results_unw <- data.table::rbindlist(list(
  cbind(estimand = "Estimand 1", ci_results_est1_unw),
  cbind(estimand = "Estimand 2", ci_results_est2_unw)
))

# Merge together the CI results with the main table_5
table_5 <- merge(
  table_5,
  ci_results,
  by = "estimand",
  all.x = TRUE
)

# rearrange columns to have exposed and then control columns
cols <- names(table_5)

new_order <- c(
  "estimand",
  grep("exposed", cols, value = TRUE, ignore.case = TRUE),
  grep("control", cols, value = TRUE, ignore.case = TRUE)
)

data.table::setcolorder(table_5, new_order)

# Merge together the CI results with the main table_5_weighted
table_5_w <- merge(
  table_5_weighted,
  ci_results_w,
  by = "estimand",
  all.x = TRUE
)

# rearrange columns to have exposed and then control columns
cols <- names(table_5_w)

new_order_w <- c(
  "estimand",
  grep("exposed", cols, value = TRUE, ignore.case = TRUE),
  grep("control", cols, value = TRUE, ignore.case = TRUE)
)

data.table::setcolorder(table_5_w, new_order_w)


# Merge together the CI results with the main table_5_unweighted
table_5_unw <- merge(
  table_5_unweighted,
  ci_results_unw,
  by = "estimand",
  all.x = TRUE
) 

# rearrange columns to have exposed and then control columns
cols <- names(table_5_unw)  

new_order_unw <- c(
  "estimand",
  grep("exposed", cols, value = TRUE, ignore.case = TRUE),
  grep("control", cols, value = TRUE, ignore.case = TRUE)
)

data.table::setcolorder(table_5_unw, new_order_unw)

# row bind the weighted and unweighted tables together
table_5_f <- data.table::rbindlist(list(
  cbind(weighted = "Unmatched + Unweighted", table_5),
  cbind(weighted = "Matched + Weighted", table_5_w),
  cbind(weighted = "Matched + Unweighted", table_5_unw)
))

# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
  data = table_5_f,
  file_path = dir_d5,
  file_name = "Table_5.csv"
)
logger::log_info("Saving: Table 5")

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d4_fup, table_5, table_5_w, ci_results, ci_results_w, ci_results_est1, 
ci_results_est2, ci_results_est1_w, ci_results_est2_w, ci_results_est1_unw,
ci_results_est2_unw, ci_results_unw, estimand_map, table_5_f, table_5_weighted,
table_5_unweighted, table_5_unw)

logger::log_info("Completed: Table 5 created")
