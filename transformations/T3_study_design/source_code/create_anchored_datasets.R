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
  lm$start_script("create_anchored_datasets")
  lm$start_capturing_prints()
}

## Config loading ----
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
config_pipeline <- picard::load_config("configuration/config_pipeline.yaml")
create_anchored_datasets <- config_project$create_anchored_datasets

# Set path to concepts database
concepts_hive_dir <- create_anchored_datasets$input$concepts_dir

# Output path for anchored variables in the Hive format
anchor_hive_dir <- create_anchored_datasets$output$anchoring_dir
logger::log_info("Starting: dataset anchoring")

# ----------------------------
# LOAD METADATA
# ----------------------------

# load anchoring window metadata which defines the variables to be anchored,
# their data type, and the time windows for anchoring
anchoring_window_metadata <- picard::load(
  file_path = create_anchored_datasets$input$metadata_dir,
  file_name = "anchoring_metadata.csv"
)

logger::log_info("Metadata loaded")

# Anchor only the variables that need anchoring (i.e., those with needs_anchoring == TRUE)
anchoring_window_metadata <- anchoring_window_metadata[anchoring_window_metadata$needs_anchoring == TRUE, ]

# ----------------------------
# LOAD DATA
# ----------------------------
d4_study_cohort <- data.table::as.data.table(picard::load(
    file_path = create_anchored_datasets$input$cohort_dir,
    file_name = "D4_StudyCohort.Rds"
))

# remove unmatched rows - i think we need also these info for the unmatched 
#d4_study_cohort <- d4_study_cohort[group != "unmatched"]

# keep unique person_ids
d4_study_cohort <- d4_study_cohort[!duplicated(person_id)]

# format date to be compatible with anchoring function
d4_study_cohort <- d4_study_cohort[, T0 := base::as.Date(T0, format = "%Y-%m-%d")]

logger::log_info("D4 Study cohort data loaded")
# ----------------------------
# ANCHOR ALL VARIABLES
# ----------------------------

# Run the anchoring one study variable at a time.
anchoR::anchor_by_variable(
  population = d4_study_cohort,
  metadata = anchoring_window_metadata,
  concepts = concepts_hive_dir,
  anchor_col = "T0",
  anchor_hive_path = anchor_hive_dir
)

logger::log_info("Finalised: anchoring all variables")

# ----------------------------
# CLEAN ENVIRONMENT
# -----------------------------
rm(d4_study_cohort, anchoring_window_metadata, concepts_hive_dir, anchor_hive_dir)
