# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_table_3.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Creating Table 3")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
dir_functions <- config_project$create_table_3.R$functions$dir_common
create_table_3 <- config_project$create_table_3.R
dir_d4 <- config_project$create_table_3.R$input$dir_d4
dir_metadata <- config_project$create_table_3.R$input$dir_metadata
dir_d5 <- config_project$create_table_3.R$output$dir_d5
save_output <- config_project$create_table_3.R$output$save_output


# Load the study cohort covariates
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) { # if dummy data are used
  file_path_covariates <- file.path(
    "tests", "testthat", "data", "D4_StudyCohort",
    paste0("D4_StudyCohort_Covariates", file_extension)
  )
} else { # for real data
  file_path_covariates <- file.path( 
    dir_d4,
    paste0("D4_StudyCohortCovariates", file_extension)
  )
}

d4_studycovariates <- picard::load(file_path_covariates, sep = ";")

# Covariates plus the IP weights for the matched and IPT-weighted columns
d4_analysis <- picard::load(file.path(dir_d4, "D4_StudyCohort_Analysis.parquet"), sep = ";")
logger::log_info("D4 StudyCohort Analysis loaded")

# metadata - will always be a csv
file_path_metadata <- file.path(dir_metadata, "BaselineDescriptives_Metadata.csv")
table_metadata <- picard::load(file_path_metadata, sep = ",")

# Remove comorbidities as these are not yet present
table_metadata <- table_metadata[!var %in% c("SV_COMORB_CVD", "SV_COMORB_HT", "SV_COMORB_DM", "SV_COMORB_HC")]

# table lookup - will always be a csv
file_path_lookup <- file.path(dir_metadata, "Label_report_lookup.csv")
label_lookup <- picard::load(file_path_lookup, sep = ",")

logger::log_info("Metadata loaded")

# source functions 
source(file.path(dir_functions, "create_desc_tables.R"))
source(file.path(dir_functions, "minimum_value_masking.R"))

logger::log_info("functions sourced")

# Check sex and lung as a metastatic side distribution among the matching variables (SV_ECOG_STATUS, SV_METASTASIS_LIVER) in the matched cohort
distribution_dt <- d4_studycovariates[
  group %in% c("control", "exposed"),
  c("SV_SEX", "SV_ECOG_STATUS", "SV_METASTASIS_LIVER", "SV_METASTASIS_LUNG", "group")
]

sex_ecog <- table(
  distribution_dt$SV_ECOG_STATUS,
  distribution_dt$SV_SEX,
  distribution_dt$group
)

sex_liver <- table(
  distribution_dt$SV_METASTASIS_LIVER,
  distribution_dt$SV_SEX,
  distribution_dt$group
)

lung_ecog <- table(
  distribution_dt$SV_METASTASIS_LUNG,  
distribution_dt$SV_ECOG_STATUS,
distribution_dt$group
)

lung_liver <- table(
  distribution_dt$SV_METASTASIS_LUNG,
  distribution_dt$SV_METASTASIS_LIVER,
  distribution_dt$group
)

logger::log_info(
  "Sex distribution and ECOG in matched cohort stratified by group:\n%s",
  paste(capture.output(print(sex_ecog)), collapse = "\n")
)

logger::log_info(
  "Sex distribution and Liver metastasis in matched cohort stratified by group:\n%s",
  paste(capture.output(print(sex_liver)), collapse = "\n")
)

logger::log_info(
  "Lung metastasis distribution and ECOG in matched cohort stratified by group:\n%s",
  paste(capture.output(print(lung_ecog)), collapse = "\n")
) 

logger::log_info(
  "Lung metastasis distribution and Liver metastasis in matched cohort stratified by group:\n%s",
  paste(capture.output(print(lung_liver)), collapse = "\n")
)
# ----------------------------
# MATCHED AND IPT-WEIGHTED COHORT
# ----------------------------
# The analysis dataset carries the covariates and the IP weights. Persons
# without a weight are dropped so that counts and percentages share a denominator
d4_analysis_matched <- d4_analysis[include_after_trimming == TRUE]
logger::log_info(sprintf(
  "Matched cohort: %d of %d persons included after trimming",
  nrow(d4_analysis_matched),
  nrow(d4_analysis)
))

matched_tables <- create_desc_tables(
  dt = d4_analysis_matched,
  matched_groups = c("control", "exposed"),
  overall_label = "Overall",
  table_metadata = table_metadata,
  label_lookup = label_lookup,
  use_weights = "ip_weight",
  weighted_stats = TRUE
)

table_matched <- attach_asd(
  tbl = matched_tables$grouped,
  asd_dt = asd_by_level(
    d4_analysis_matched,
    table_metadata,
    use_weights = "ip_weight"
  ),
  table_metadata = table_metadata
)
logger::log_info("Created: Matched and IPT-weighted cohort table")

# Masking follows the number of patients rather than the weighted counts, which
# can drop below the threshold with more than five patients behind them
matched_unweighted <- create_desc_tables(
  dt = d4_analysis_matched,
  matched_groups = c("control", "exposed"),
  overall_label = "Overall",
  table_metadata = table_metadata,
  label_lookup = label_lookup
)$grouped

# ----------------------------
# UNMATCHED AND UNWEIGHTED COHORT
# ----------------------------
# copy d4_studycovariates to avoid modifying the original dataset in place when creating the tables
d4_studycovariates_unmatched <- data.table::copy(d4_studycovariates)
# remove group column
d4_studycovariates_unmatched$group <- NULL
# rename group_unmatched to group (this is needed for the create_desc_tables function to work)
data.table::setnames(d4_studycovariates_unmatched, "group_unmatched", "group")

# count the number of exposed and control patients in the unmatched cohort
unmatched_counts <- d4_studycovariates_unmatched[group %in% c("control", "exposed"), .N, by = group]
logger::log_info("Unmatched cohort counts: {unmatched_counts}")

full_tables <- create_desc_tables(
  dt = d4_studycovariates_unmatched,
  matched_groups = NULL,
  overall_label = "Overall",
  table_metadata = table_metadata,
  label_lookup = label_lookup
)

table_full <- attach_asd(
  tbl = full_tables$grouped,
  asd_dt = asd_by_level(d4_studycovariates_unmatched, table_metadata),
  table_metadata = table_metadata
)
logger::log_info("Created: Unmatched and unweighted cohort table")

# ----------------------------
# OVERALL DISTRIBUTIONS TO THE LOG
# ----------------------------
# The overall column is no longer part of the table, so age and sex from the
# overall population are written to the log instead
for (block in c("unmatched", "matched")) {

  overall_tbl <- if (block == "unmatched") full_tables$overall else matched_tables$overall

  logger::log_info(sprintf(
    "Overall age and sex (%s cohort):\n%s",
    block,
    paste(
      capture.output(print(overall_tbl[var %in% c("SV_AGE", "SV_SEX")])),
      collapse = "\n"
    )
  ))
}

# ----------------------------
# FINAL TABLE
# ----------------------------
# Both blocks are built from the same metadata, so their rows line up one to one
stopifnot(identical(table_full$label, table_matched$label))

table_final <- cbind(
  table_full[, .(
    label,
    var,
    type,

    N_exposed_unmatched    = N_exposed,
    perc_exposed_unmatched = perc_exposed,
    aux_exposed_unmatched  = aux_exposed,

    N_control_unmatched    = N_control,
    perc_control_unmatched = perc_control,
    aux_control_unmatched  = aux_control,

    asd_unmatched = asd
  )],

  table_matched[, .(
    N_exposed_matched    = N_exposed,
    perc_exposed_matched = perc_exposed,
    aux_exposed_matched  = aux_exposed,

    N_control_matched    = N_control,
    perc_control_matched = perc_control,
    aux_control_matched  = aux_control,

    asd_matched = asd
  )],

  # patient counts behind the weighted columns, used for masking only
  matched_unweighted[, .(
    N_exposed_matched_unwt = N_exposed,
    N_control_matched_unwt = N_control
  )]
)
logger::log_info("Created: Table 3")

# ----------------------------
# MASKING
# ----------------------------
# patient counts decide what is masked
count_cols <- c(
  "N_exposed_unmatched",
  "N_control_unmatched",
  "N_exposed_matched_unwt",
  "N_control_matched_unwt"
)

# the weighted counts are hidden on the same rows
weighted_count_cols <- c("N_exposed_matched", "N_control_matched")

percent_cols <- c(
  "perc_exposed_unmatched",
  "perc_control_unmatched",
  "perc_exposed_matched",
  "perc_control_matched"
)

table_3 <- minimum_value_masking(
  dataset = table_final,
  min_value_masking = 5,
  value_columns = count_cols,
  percentage_columns = percent_cols
)

table_3[, (weighted_count_cols) := lapply(.SD, as.character), .SDcols = weighted_count_cols]
table_3[N_exposed_matched_unwt == "<5", (weighted_count_cols) := "<5"]
table_3[, c("N_exposed_matched_unwt", "N_control_matched_unwt") := NULL]

logger::log_info("Finish creating Table 3")
# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
  table_3,
  file_path = dir_d5,
  file_name = "Table_3.csv"
)
logger::log_info("Saving: Table 3")

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(
  d4_studycovariates, d4_analysis, d4_analysis_matched,
  d4_studycovariates_unmatched, table_metadata, label_lookup,
  matched_tables, matched_unweighted, full_tables, table_matched, table_full,
  table_final, table_3, count_cols, percent_cols, weighted_count_cols
)
