#######################################################
# Setting up the environment
#######################################################
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_simple_concepts.R")
  # Start capturing print statements
  lm$start_capturing_prints()
}

picard::load_config()

logger::log_info("[Create simple concepts] - START")

dir_concept <- config_pipeline$databases$dir_concept
dir_d2_db <- config_pipeline$databases$dir_d2_db
root <- config_t2$T2$root
intermediate <- config_t2$T2$intermediate
dir_intermediate_t2 <- file.path(root, intermediate)
deap_name <- config_project$DEAP_configuration$name
dir_d3 <- config_project$output$dir_d3
dir_bridge <- config_pipeline$common$dir_bridge

#######################################################
# Connect to CDM origin database and concepts database
#######################################################
logger::log_info("To create a new database, we delete the old one if present.")
if (file.exists(dir_concept)) {
  if (exists("concepts_db_conn")) {
    duckdb::dbDisconnect(concepts_db_conn)
    rm(concepts_db_conn)
  }
  if (base::file.remove(dir_concept)) {
    logger::log_info("Old concepts database removed")
  }
}

# New database is created.
concepts_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_concept)

# Files are seved in parquet we prepare an empthy folder where to save them.
dir_parquet_concepts <- file.path(dir_d3, "D3_CONCEPTS_parquet")
# As before we delete and create.
if (file.exists(dir_parquet_concepts)) {
  unlink(dir_parquet_concepts, recursive = TRUE)
}
dir.create(dir_parquet_concepts)

#######################################################
# PREPARING DATASETS
#######################################################

# Attach CDM origin database to pre-matching database
d2_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)


logger::log_debug("Loading concept map and codelist")
## -----------------------------------------------------------------------------
# Load your concept map in RWE BRIDGE metadata format
# Expected columns: concept_id, cdm_table_name,
#                   column_name_*, expected_value_*,
#                   keep_value_column_name, keep_date_column_name

dap_specific_concept_map <- data.table::fread(file.path(dir_bridge, "dap_specific_concept_map.csv"))
dap_specific_concept_map[dap_specific_concept_map == "" | dap_specific_concept_map == "NA"] <- NA_character_

# Transform to long format
long_format_concept_map <- T2.DMM::wrangling_concept_map(dap_specific_concept_map)
long_format_concept_map[, id_set := paste0("conceptmap-", id_set)]

## -----------------------------------------------------------------------------
# Load the OMOP codelist specific for the CS2, and transform to long format
cdm_codelist <- data.table::fread(file.path(dir_bridge, "MERGED_CODELIST.csv"))
cdm_codelist <- unique(data.table::as.data.table(cdm_codelist))
cdm_codelist[, id_set := .I]
cdm_codelist[, order_index := 1]
cdm_codelist[, keep_value_column_name := NA_character_]
cdm_codelist[, id_set := paste0("codelist-", id_set)]

codelist <- data.table::rbindlist(list(long_format_concept_map, cdm_codelist),
  use.names = TRUE, fill = TRUE
)
codelist <- codelist[, cdm_table_name := tolower(cdm_table_name)]
codelist <- codelist[, cdm_name := "OMOP"]
# Apply the prepared codelist to the database
logger::log_debug("Applying codelist to create concept tables in the concepts database")

T2.DMM::apply_codelist(
  db_con = d2_db_conn,
  codelist = codelist,
  materialize = "in_parquet", # or "in_parquet"
  path_parquets = dir_parquet_concepts, # "/path/to/save" if    materialize =   "in_parquet"
  keep_id_set = FALSE
)

duckdb::dbDisconnect(d2_db_conn)

rm(
  dap_specific_concept_map, concepts_db_conn, cdm_codelist, dir_parquet_concepts, dir_d2_db, dir_concept, dir_bridge
)

logger::log_info(c("[Create Concepts Database] - SUCCEED"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
