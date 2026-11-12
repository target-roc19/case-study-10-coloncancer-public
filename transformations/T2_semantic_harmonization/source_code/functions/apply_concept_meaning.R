#' Derive concept meaning from the concept_meaning sheet.
#'
#'  Sub-concepts are mapped to their corresponding meanings using the
#'  `concept_meaning` sheet. For each row, the `value` column is populated
#'  with the meaning corresponding to the sub-concept. If a person has
#'  multiple sub-concepts present simultaneously, multiple rows are written
#'  and a warning is logged.
#'
#' @param input_dir Input directory containing partitioned parquet files.
#' @param output_dir Output directory where the new partition will be written.
#' @param complex_concept_name Name of the new complex concept
#'  to create (used as partition value).
#' @param sub_concept_ids Character vector of sub_concept_ids to include.
#'
#' @returns Invisibly returns a list summarizing the operation,
#'  including matched partitions and output path.
apply_concept_meaning <- function(
  input_dir,
  output_dir,
  complex_concept_name = NULL,
  sub_concept_ids = NULL
) {
  # Normalise inputs
  params <- normalise_inputs(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids
  )
  # Validate inputs
  validate_inputs(
    input_dir = params$input_dir,
    complex_concept_name = params$complex_concept_name,
    sub_concept_ids = params$sub_concept_ids
  )
  # Clean output directory
  ensure_clean_output_directory(params$output_dir, params$complex_concept_name)

  # Getting existing sub_concept_ids
  existing_sub_concepts <- prepare_sub_concept_ids(
    sub_concept_ids = params$sub_concept_ids,
    input_directory = params$input_dir
  )

  concept_id_list <- paste0("'", existing_sub_concepts, "'", collapse = ", ")

  # Open DuckDB connection
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Map concept meanings to sub-concepts
  DBI::dbExecute(
    con,
    glue::glue(
      "CREATE OR REPLACE VIEW {params$complex_concept_name} AS
        SELECT
          unique_id,
          ori_table,
          person_id,
          cm.value AS value,
          date,
          '{params$complex_concept_name}' AS concept_id
        FROM read_parquet(
          '{params$input_dir}/**/*.parquet',
          hive_partitioning = true,
          union_by_name     = true
        ) AS rp
        JOIN read_csv(
          '{config_project$create_complex_concepts$concept_meaning}'
        ) AS cm
        ON CAST(rp.concept_id AS STRING) = CAST(cm.concept_id AS STRING)
        WHERE rp.concept_id IN ({concept_id_list})"
    )
  )

  # Warn about persons with multiple sub-concepts present simultaneously
  multi_concept <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT person_id, COUNT(*) AS n_concepts
        FROM {params$complex_concept_name}
        GROUP BY person_id
        HAVING COUNT(*) > 1"
    )
  )
  if (nrow(multi_concept) > 0L) {
    logger::log_warn(glue::glue(
      "{nrow(multi_concept)} person(s) have multiple subconcepts present ",
      "simultaneously. Multiple rows will be written per person."
    ))
  }

  tryCatch(
    {
      DBI::dbExecute(
        con,
        glue::glue(
          "COPY (
            SELECT * FROM {params$complex_concept_name}
          ) TO '{params$output_dir}'
          (FORMAT PARQUET, PARTITION_BY (concept_id), APPEND)"
        )
      )
    },
    error = function(e) {
      rlang::abort(
        glue::glue(
          "DuckDB COPY failed for '{params$complex_concept_name}': {conditionMessage(e)}"
        ),
        class = "duckdb_copy_error"
      )
    }
  )

  # Checking rows affected
  counts <- get_counts(
    connection = con,
    complex_concept_name = params$complex_concept_name,
    output_directory = params$output_dir
  )
  logger::log_info(crayon::cyan(
    "Counts:\n",
    paste(capture.output(print(t(counts), sep = ":"))[-1], collapse = "\n")
  ))
}
