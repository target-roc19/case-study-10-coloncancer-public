# ---------------------------------------------- #
# --------- Table 1.1 Attrition table ---------- #
# ---------------------------------------------- #

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_table_1_1.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Creating Table 1.1")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_table_1_1 <- config_project$create_table_1_1.R
dir_d3 <- config_project$create_table_1_1.R$input$dir_d3
dir_d5 <- config_project$create_table_1_1.R$output$dir_d5
save_output <- config_project$create_table_1_1.R$output$save_output
dir_functions <- config_project$create_table_1_1.R$functions$dir_common


# Load eligibility data
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) { # if dummy data are used
  file_path_eligibility <- file.path(
    "tests", "testthat", "data", "D4_StudyCohort",
    paste0("D3_ELIGIBILITY", file_extension)
  )
  D3_Eligibility <- picard::load(file_path_eligibility, sep = ",")
} else { # for real data
  file_path_eligibility <- file.path(
    dir_d3,
    paste0("D3_ELIGIBILITY", file_extension)
  )
  D3_Eligibility <- picard::load(file_path_eligibility)
}

# source functions
source(file.path(dir_functions, "fn_attrition_tables.R"))

#--------------------------------------------------#
# Create attrition table 1.1
#--------------------------------------------------#
## Table 1.1: Attrition table for entry into the eligible population
attrition_table_1_1 <- create_attrition_table_1_1(
  d3_eligibility = D3_Eligibility,
  return_details = TRUE
)

table_1_1 <- attrition_table_1_1$table

logger::log_info(paste0(
  "Among patients dropped at n_2c, persons missing ECOG status on all records: ",
  attrition_table_1_1$n_missing_ecog,
  " (", attrition_table_1_1$pct_missing_ecog, "%)"
))

logger::log_info("Finish creating Table 1.1.")

#--------------------------------------------------#
# Save output
#--------------------------------------------------#
picard::save(
  table_1_1,
  file_path = file.path(dir_d5, "Table_1_1.csv")
)


#--------------------------------------------------#
# Clean environment
#--------------------------------------------------#
rm(
  D3_Eligibility, table_1_1, attrition_table_1_1,
  config_project, config_values, create_table_1_1
)
