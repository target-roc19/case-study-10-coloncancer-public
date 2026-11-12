# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_baseline_covariates")
  lm$start_capturing_prints()
}

## Config loading 
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
config_pipeline <- picard::load_config("configuration/config_pipeline.yaml")
create_anchored_datasets <- config_project$create_anchored_datasets

# Output path for anchored variables in the Hive format
anchor_hive_dir <- create_anchored_datasets$output$anchoring_dir

# ----------------------------
# LOAD METADATA
# ----------------------------
# load anchoring window metadata which defines the variables to be anchored,
# their data type, and the time windows for anchoring
anchoring_window_metadata <- picard::load(
  file_path = create_anchored_datasets$input$metadata_dir,
  file_name = "anchoring_metadata.csv")

# for variable_id="SV_METASTASIS_MULTIPLE", change data_type column to "INT"
anchoring_window_metadata[anchoring_window_metadata$variable_id == "SV_METASTASIS_MULTIPLE", "data_type"] <- "INT"

# Anchor only the variables that need anchoring (i.e., those with needs_anchoring == TRUE)
anchoring_window_metadata <- anchoring_window_metadata[anchoring_window_metadata$needs_anchoring == TRUE, ]

data.table::setnames(anchoring_window_metadata, "data_type", "variable_type")
anchoring_window_metadata[,is_expected_missing := FALSE]

# ----------------------------
# Extract BASELINE COVARIATES
# ----------------------------
covariates <- anchoR::get_anchor_result(
    metadata = anchoring_window_metadata[covariate == TRUE],
    anchor_hive_path = anchor_hive_dir,
    impute_missing = TRUE,
    result_shape = "wide"
)

logger::log_info("Finalised: extracting baseline covariates")

# Merge additional variables we need for the D4_StudyCohort_Covariates dataset

# 1. group (D4_StudyCohort)
logger::log_info("Merging in group from D4_StudyCohort")
d4_study_cohort <- data.table::as.data.table(picard::load(
    file_path = create_anchored_datasets$input$cohort_dir,
    file_name = "D4_StudyCohort.Rds"
))
#d4_study_cohort <- d4_study_cohort[group != "unmatched"]
d4_study_cohort <- d4_study_cohort[!duplicated(person_id)]
d4_study_cohort <- d4_study_cohort[, T0 := base::as.Date(T0, format = "%Y-%m-%d")]
covariates <- data.table::merge.data.table(
  x = covariates,
  y = d4_study_cohort[, .(person_id, eligible_exposed,eligible_control,group)],
  by = "person_id",
  all.x = TRUE
)
# check if value_SV_SITE_SIDEDNESS has NA values
if (any(is.na(covariates$value_SV_SITE_SIDEDNESS))) {
  logger::log_info("There are NA values in value_SV_SITE_SIDEDNESS.")
}

# create a new 'group' variable where we combine the eligible_exposed and eligible_control variables with the existing 'group' variable to create the exposed/controls before matching (this is needed for table 3)
covariates <- covariates[, group_unmatched := data.table::fifelse(eligible_exposed == TRUE & group == "unmatched", "exposed", 
data.table::fifelse(eligible_control == TRUE & group == "unmatched", "control", group))]

# 2. SV_AGE (D3_PERSONS), SV_SEX (D3_PERSONS)
logger::log_info("Merging in SV_AGE and SV_SEX from D3_PERSONS")
persons <- picard::load(
  file_path = config_project$outputs$dir_d3,
  file_name = "D3_PERSONS.parquet"
)[, .(person_id, sex_at_instance_creation, birth_date)]
covariates <- data.table::merge.data.table(
  x = covariates,
  y = persons,
  by = "person_id",
  all.x = TRUE
)
data.table::setnames(covariates, "sex_at_instance_creation", "SV_SEX")
# Calculate age at T0 using birth_date (in years)
covariates <- covariates[, SV_AGE := as.integer((T0 - birth_date) / 365.25)]

# 4. SV_ECOG_STATUS, SV_METASTASIS_LIVER, SV_DIAGNOSED_IN_2015, SV_DIAGNOSED_IN_2021 (from D4_StudyCohort)
logger::log_info("Merging in SV_ECOG_STATUS and SV_METASTASIS_LIVER from D4_StudyCohort")
covariates <- data.table::merge.data.table(
  x = covariates,
  y = d4_study_cohort[, .(person_id, SV_ECOG_STATUS, SV_METASTASIS_LIVER, SV_DIAGNOSED_IN_2015, SV_DIAGNOSED_IN_2021)],
  by = c("person_id"),
  all.x = TRUE
)

# Selectc columns needed for the covariates table (based on D4_StudyCohort_Covariates codebook)
covariates_f <- covariates[, .(person_id, T0, group, group_unmatched,
SV_AGE, SV_SEX, SV_ECOG_STATUS, SV_DIAGNOSED_IN_2015, SV_DIAGNOSED_IN_2021, value_SV_SITE_SIDEDNESS, 
SV_METASTASIS_LIVER, value_SV_METASTASIS_LUNG, value_SV_METASTASIS_PERITONEUM, 
value_SV_METASTASIS_OTHER, value_SV_METASTASIS_LYMPH, value_SV_METASTASIS_MULTIPLE, 
value_SV_MUTATION_BRAF, value_SV_MUTATION_RAS)]

# remove the 'value' prefix from the covariate names to be consistent with the codebook
data.table::setnames(covariates_f, old = c("value_SV_SITE_SIDEDNESS", "value_SV_METASTASIS_LUNG", "value_SV_METASTASIS_PERITONEUM", "value_SV_METASTASIS_OTHER", "value_SV_METASTASIS_LYMPH", "value_SV_METASTASIS_MULTIPLE", "value_SV_MUTATION_BRAF", "value_SV_MUTATION_RAS"), new = c("SV_SITE_SIDEDNESS", "SV_METASTASIS_LUNG", "SV_METASTASIS_PERITONEUM", "SV_METASTASIS_OTHER", "SV_METASTASIS_LYMPH", "SV_METASTASIS_MULTIPLE", "SV_MUTATION_BRAF", "SV_MUTATION_RAS"))  

# In mutations, replace NA with absent:
# RAS: NA to "absent"
covariates_f[is.na(SV_MUTATION_RAS), SV_MUTATION_RAS := "absent"]
# BRAF: NA to "absent"
covariates_f[is.na(SV_MUTATION_BRAF), SV_MUTATION_BRAF := "absent"]

# Convert metastasis columns to logical type (except liver as it was logical via eligibility)
# Convert SV_METASTASIS_* columns to logical type
metastasis_cols <- c("SV_METASTASIS_LUNG", "SV_METASTASIS_LYMPH", 
                     "SV_METASTASIS_PERITONEUM", "SV_METASTASIS_OTHER")

for (col in metastasis_cols) {
  covariates_f[[col]] <- base::as.logical(covariates_f[[col]])
}

logger::log_info("Converted metastasis variables to logical type")

# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
    data = covariates_f,
    file_path = create_anchored_datasets$output$dir,
    file_name = paste0(create_anchored_datasets$output$baseline_cov_file, ".parquet")
  )
