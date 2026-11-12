# ---------------------------------------------- #
# ------- Table 2 Matching statistics -------- #
# ---------------------------------------------- #

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_table_2.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Creating Table 2")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_table_2 <- config_project$create_table_2.R
dir_d3 <- config_project$create_table_2.R$input$dir_d3
dir_d4 <- config_project$create_table_2.R$input$dir_d4
dir_d5 <- config_project$create_table_2.R$output$dir_d5
save_output <- config_project$create_table_2.R$output$save_output
dir_functions <- config_project$create_table_2.R$functions$dir_common

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

# Load study cohort data
file_path_studycohort <- file.path(dir_d4, "D4_StudyCohort.Rds")
D4_StudyCohort <- picard::load(file_path_studycohort)

# source functions
source(file.path(dir_functions, "fn_attrition_tables.R"))
#--------------------------------------------------#
# Create matching statistics table 2
#--------------------------------------------------#
## Table 2: Matching statistics (without replacement) for the CapOx-B and CapOx cohort in NCR
table_2 <- create_attrition_table_1b(d3_eligibility = D3_Eligibility, d4_main = D4_StudyCohort)

logger::log_info("Finish creating Table 2.")

#--------------------------------------------------#
# Save output
#--------------------------------------------------#
picard::save(
  table_2,
  file_path = file.path(dir_d5, "Table_2.csv")
)

#--------------------------------------------------#
# Clean environment
#--------------------------------------------------#
rm(
  D3_Eligibility, D4_StudyCohort, table_2
)
