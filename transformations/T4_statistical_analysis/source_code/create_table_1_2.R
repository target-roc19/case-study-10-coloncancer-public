# ---------------------------------------------- #
# ------- Table 1.2 PREVIOUS MALIGNANCIES ------ #
# ---------------------------------------------- #

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_table_1_2.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Creating Table 1.2")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_table_1_2 <- config_project$create_table_1_2.R
dir_d3 <- config_project$create_table_1_2.R$input$dir_d3
dir_d5 <- config_project$create_table_1_2.R$output$dir_d5
save_output <- config_project$create_table_1_2.R$output$save_output
dummy_data_used <-config_values$dummy_data_used
dir_functions <- config_project$create_table_1_1.R$functions$dir_common

# Load eligibility dataset
d3_eligibility <- picard::load(
  file_path = dir_d3,
  file_name = "D3_ELIGIBILITY.parquet"
)
logger::log_info("Loading D3_ELIGIBILITY dataset for creating Table 1.2")

# Load metadata for table 1.2
metadata <- picard::load(
  file_path = "Transformations/common_configuration/Metadata/",
  file_name = "Table_1_2_Metadata.csv", sep = ";"
)
logger::log_info("Loading metadata for Table 1.2")

# Load label lookup for table 1.2
labels <- picard::load(
  file_path = "Transformations/common_configuration/Metadata/",
  file_name = "Label_table_1_2_lookup.csv", sep = ","
)
logger::log_info("Loading label lookup for Table 1.2")

# source functions
source(file.path(dir_functions, "fn_attrition_tables.R"))

#--------------------------------------------------#
# CREATE TABLE 1.2
#--------------------------------------------------#
# apply the function to create the table 1.2
table1_2 <- create_previous_malignancy_tab(
  d3_eligibility,
  debug = TRUE
)

# extract the data.table for those persons from the list
table1_2_dt <- data.table::copy(table1_2$dropped_n2c_n2d)

# add a new column to indicate the group (exposed or control)
table1_2_dt[, group := data.table::fifelse(
  receives_exposure,
  "exposed",
  "control"
)]

# check the number of rows in d3_prev_mal
logger::log_info(paste0("Number of rows in data: ", nrow(table1_2_dt)))
# check the nummber of patients (unique person_ids)
logger::log_info(paste0("Number of unique person_ids in data: ", length(unique(table1_2_dt$person_id))))
# check the number of rows in each group
logger::log_info(paste0("Number of rows in the exposed group: ", nrow(table1_2_dt[group == "exposed"])))
logger::log_info(paste0("Number of rows in the control group: ", nrow(table1_2_dt[group == "control"])))  
# check the number of patients in each group
logger::log_info(paste0("Number of unique person_ids in the exposed group: ", length(unique(table1_2_dt[group == "exposed"]$person_id))))
logger::log_info(paste0("Number of unique person_ids in the control group: ", length(unique(table1_2_dt[group == "control"]$person_id)))) 

# Patients with more than one row
patients_multiple_rows <- table1_2_dt[, .N, by = person_id][N > 1]
logger::log_info(paste0("Patients with >1 row: ",nrow(patients_multiple_rows)))

# split the data.table into two groups: exposed and control 
exposed_dt <- table1_2_dt[group == "exposed"]
control_dt <- table1_2_dt[group == "control"]

# in the control data, remove episodes with the person_id that are in the exposed data, to avoid double counting
control_dt <- control_dt[!person_id %in% exposed_dt$person_id]

# from the controls data, select only one episode per person_id, to avoid double counting. 
# We will select an episode that the previous diagnosis is not missing, and if there are multiple episodes with previous diagnosis, we will select the earliest episode.
control_dt <- control_dt[!is.na(SV_DIAG_EXCL)]  
control_dt <- control_dt[order(person_id, start_episode)]
control_dt <- control_dt[, .SD[1], by = person_id]

# put together the exposed and control data.tables again
table1_2_dt <- rbind(exposed_dt, control_dt)

# check the number of rows in d3_prev_mal
logger::log_info(paste0("Number of rows in data: ", nrow(table1_2_dt)))
# check the nummber of patients (unique person_ids)
logger::log_info(paste0("Number of unique person_ids in data: ", length(unique(table1_2_dt$person_id))))
# check the number of rows in each group
logger::log_info(paste0("Number of rows in the exposed group: ", nrow(table1_2_dt[group == "exposed"])))
logger::log_info(paste0("Number of rows in the control group: ", nrow(table1_2_dt[group == "control"])))  
# check the number of patients in each group
logger::log_info(paste0("Number of unique person_ids in the exposed group: ", length(unique(table1_2_dt[group == "exposed"]$person_id))))
logger::log_info(paste0("Number of unique person_ids in the control group: ", length(unique(table1_2_dt[group == "control"]$person_id)))) 

# Apply descriptive table function
tableout <- DescriptiveTable::DescriptivesTable(popdf = table1_2_dt[, c("SV_DIAG_EXCL", "group")],
                  table_metadata = metadata, 
                  groupcol = "group", 
                  output_format = "processed",
                  calculate_asd = FALSE, 
                  keep_varinfo = TRUE, 
                  label_lookup = labels,
                  missing_vars = FALSE,
                  missing_flag = -99, 
                  round_decimals = 2, 
                  output_asd = FALSE
)

# Select only the columns of counts and percentages
tableout <- tableout[,c("label",paste0("V", 1:2,"_exposed"), paste0("V",1:2,"_control"))]

# Rename columns
tableout <- data.table::setnames(tableout,c(paste0("V",1:2,"_exposed"), 
        paste0("V",1:2,"_control")),
        c("N_exposed","perc_exposed",
          "N_control","perc_control"))

# Format the columns as N (%) per group
tableout <- tableout[, .(
  label,
  `CapOx`   = sprintf("%d (%.1f%%)", N_control, perc_control),
  `CapOx-B` = sprintf("%d (%.1f%%)", N_exposed, perc_exposed)
)]

# Move lung cancer to the end of the table
tableout <- rbind(tableout[!grepl("Lung", label)], tableout[grepl("Lung", label)])
# Manually add the column about previous malignancies on row 4 and 10
tableout[, "label_1" := ""]

# remove row 1 and 3
tableout <- tableout[-c(1,3),]

tableout[c(1, 2, 7, 11), "label_1" := c("Treatment of previous malignancies",
                                                                "Previous malignancies potentially treated with oxaliplatin",
                                                                "Previous malignancies potentially treated with bevacizumab", "Previous malignancy potentially treated with oxaliplatin and/or bevacizumab")]

# in row 2, keep N (%) in the Capox and Capox-B columns, as this row is only a header for the next rows
tableout[1, c("CapOx", "CapOx-B") := "N (%)"]

# Reorder columns
tableout_f <- tableout[, .(label_1, label, `CapOx`, `CapOx-B`)]       
logger::log_info("Creating Table 1.2: Finished")

# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
  tableout_f,
  file_path = file.path(dir_d5, "Table_1_2.csv")
)
# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(
  table1_2, table1_2_dt, tableout, tableout_f, metadata, labels,
  config_project, config_values, create_table_1_2, d3_eligibility, patients_multiple_rows
)
