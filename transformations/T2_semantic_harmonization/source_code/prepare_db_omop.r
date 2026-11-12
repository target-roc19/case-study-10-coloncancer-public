################################################################################
#### This script exports OMOP tables to partitioned Parquet files           ####
################################################################################

# --- Parameters ---
overwrite <- TRUE
schema <- "cs10"
dbname <- "postgres"
user <- ""
password <- ""

# --- Define the Table Configurations ---
# We use a list where the name is the table and the value is a list of parameters
tables_to_export <- list(
  "condition_occurrence" = list(order_by = "person_id"),
  "death" = list(order_by = "person_id"),
  "observation_period" = list(order_by = "person_id"),
  "drug_exposure" = list(order_by = "person_id"),
  "measurement" = list(order_by = "person_id"),
  "observation" = list(order_by = "person_id"),
  "person" = list(
    partition_on = "gender_concept_id",
    order_by = c("gender_concept_id", "person_id")
  ),
  "procedure_occurrence" = list(order_by = "person_id"),
  "vocabulary" = list(order_by = "vocabulary_id")
)

# --- Database Connection ---
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "localhost",
  port = 5432,
  dbname = dbname,
  user = user,
  password = password
)

# Loads functions needed ------------------
root <- config_t2$T2$root
function_path <- file.path(root, config_t2$T2$functions)
files <- list.files(function_path, pattern = "\\.[Rr]$", full.names = TRUE)
print(files)
invisible(lapply(files, source))

# --- Execution Wrapper ---
# Loop through the list and execute the function
for (table_name in names(tables_to_export)) {
  message(paste("Exporting table:", table_name, "..."))

  # Get specific params for this table
  params <- tables_to_export[[table_name]]

  # Execute function with do.call to handle variable arguments (like partition_on)
  do.call(
    export_postgres_to_parquet,
    c(
      list(
        table = table_name,
        overwrite = overwrite,
        schema = schema,
        out_dir = config_project$DEAP_configuration$data_instance,
        conn = con
      ),
      params
    )
  )
}

DBI::dbDisconnect(con)

register_parquet_root_as_duckdb_views(
  parquet_root_dir = config_project$DEAP_configuration$data_instance,
  db_path = config_project$DEAP_configuration$database_path,
  schema = "cdm_synthea10",
  overwrite_db = TRUE,
  overwrite_views = TRUE,
  hive_partitioning = TRUE
)

# --- Cleanup ---
message("All exports completed.")
