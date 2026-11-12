## Script logger ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Creating D3 Univariate Episodes")
  # Start capturing print statements
  lm$start_capturing_prints()
}

logger::log_info("[Univariate Episodes] - STARTING")

picard::load_config()

function_dir <- file.path(config_t3$T3$root, config_t3$T3$functions)
output_dir <- config_project$episodes_and_eligibility$d3_univariate_episodes

start_study_date <- picard::get_date_value(config_values$start_study_date)
end_study_date <- picard::get_date_value(config_values$end_study_date)
end_date_missing_inclusion <- min(
  picard::get_date_value(config_values$end_study_date)
)

# Load study_variables and map list_sv to concept_id
study_variables <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "study_variables.csv"
)
study_variables$start_look_back <- abs(as.integer(study_variables$start_look_back))
study_variables$end_look_back <- abs(as.integer(study_variables$end_look_back))

# missing_possible <- picard::load(
#   file_path = config_pipeline$common$dir_bridge,
#   file_name = "matching_possible_missing.csv"
# )
# study_variables <- merge(study_variables, missing_possible[, .(variable_id, missing_set_to)],
#   by = "variable_id", all.x = TRUE
# )

# Map variable_id to concept_id(s)
sv_meta <- study_variables[study_variables$eligibility == TRUE, ]
concept_ids <- unique(sv_meta$concept_id)
sv_meta[, batch := TRUE] # for testing, assign all to one batch. In production, assign batches based on concept_id or variable_id to optimize processing
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# D3_CONCEPTS: read only required concept_id partitions from hive-partitioned parquet folder
concept_id_filter <- paste(sprintf("'%s'", concept_ids), collapse = ", ")
DBI::dbExecute(con, sprintf(
  "CREATE OR REPLACE VIEW D3_CONCEPTS AS
   SELECT * FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE, union_by_name = TRUE)
   WHERE concept_id IN (%s)",
  config_project$episodes_and_eligibility$d3_all_concepts,
  concept_id_filter
))

person_ids <- DBI::dbGetQuery(con, sprintf(
  "SELECT DISTINCT person_id FROM read_parquet('%s')",
  config_project$episodes_and_eligibility$d3_spells
))$person_id

unlink(output_dir, recursive = TRUE) # clean output directory before writing new results

episodeR::univariate_episodes_pipeline(
  study_variables = sv_meta,
  con = con,
  person_ids = person_ids,
  sql_dir = system.file(package = "episodeR", "sql"),
  start_study_date = start_study_date,
  end_date_missing_inclusion = end_date_missing_inclusion,
  output_hive_path = output_dir,
  batch_size = 1000, # config_project$create_univariate_episodes$batch_size,
  batch_column = "batch",
  missing_col = "missing_set_to"
)

logger::log_info(c("[Univariate Episodes] - SUCCEED"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}


# # issue with value in D3_concepts: DSP_ABRYSVO_VACC value is "ABRYSVO" instead of 0/1
# # PP_OTHER_VACC is not stored in D3_concepts, value is NULL for all
# # todo - composites
