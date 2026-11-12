## Script logger ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Creating D3 Multivariate Episodes")
  lm$start_capturing_prints()
}

logger::log_info("[Multivariate Episodes] - STARTING")

picard::load_config()

function_dir <- file.path(config_t3$T3$root, config_t3$T3$functions)
input_dir <- config_project$episodes_and_eligibility$d3_univariate_episodes
output_dir <- config_project$episodes_and_eligibility$d3_multivariate_episodes

# Load study_variables and map list_sv to variable_id
study_variables <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "study_variables.csv"
)

sv_meta <- study_variables[study_variables$eligibility == TRUE, ]
sv_meta[, batch := TRUE] # for testing; in production assign based on variable_id

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

person_ids <- DBI::dbGetQuery(con, sprintf(
  "SELECT DISTINCT person_id FROM read_parquet('%s')",
  config_project$episodes_and_eligibility$d3_spells
))$person_id

unlink(output_dir, recursive = TRUE)

episodeR::multivariate_episodes_pipeline(
  study_variables             = sv_meta,
  con                         = con,
  d3_univariate_episodes_path = input_dir,
  sql_dir                     = system.file(package = "episodeR", "sql/"),
  output_path                 = output_dir,
  person_ids                  = person_ids,
  batch_size                  = 1000,
  batch_column                = "batch"
)

DBI::dbDisconnect(con, shutdown = TRUE)

logger::log_info("[Multivariate Episodes] - SUCCEED")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
