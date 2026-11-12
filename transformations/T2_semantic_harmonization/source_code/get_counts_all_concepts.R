# This step obtains the counts for each concept in the all concepts database
picard::audit_start()
picard::load_config()

logger::log_info("[Get counts database] - START")
dir_all_concepts <- config_pipeline$databases$dir_concept
dir_d6 <- config_project$output$dir_d6

allconcepts_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_all_concepts)
output_dir <- config_project$create_complex_concepts$output_dir

DBI::dbExecute(
  allconcepts_db_conn,
  paste0(
    "CREATE OR REPLACE VIEW concept_table AS
      SELECT ori_table,   -- Original CDM table
            unique_id,  -- Row ID in CDM_database
            person_id,
            UPPER(CAST(value AS VARCHAR)) AS value,
            concept_id,
            date
      FROM read_parquet('",
    output_dir,
    "/**/*.parquet',
    hive_partitioning = TRUE)"
  )
)

# Get counts per concept
concept_counts <- DBI::dbGetQuery(
  allconcepts_db_conn,
  "
  SELECT concept_id, COUNT(*) AS count
  FROM concept_table
  GROUP BY concept_id
  "
) |> data.table::as.data.table()

# mask values lower then 5 with <5
mask_idx <- which(concept_counts$count < 5)
data.table::set(
  concept_counts,
  j = "count",
  value = as.character(concept_counts$count)
)
data.table::set(
  concept_counts,
  i = mask_idx,
  j = "count",
  value = "<5"
)

# save counts per concept
picard::audit_add(concept_counts)
picard::save(
  data = concept_counts,
  file_path = "data/audits/",
  file_name = "all_concept_counts.txt",
  sep = "\t",
  create_plot = FALSE
)
duckdb::dbDisconnect(allconcepts_db_conn, shutdown = TRUE)
rm(allconcepts_db_conn)
gc()

picard::audit_end()
logger::log_info("[Get counts database] - Completed without errors")
lm$stop_capturing_prints()
