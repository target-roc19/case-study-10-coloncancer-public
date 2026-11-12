#' Derive the complex concept for persons who are 18 years or older.
#'
#'  This function reads from the `D3_SPELLS` table and calculates the
#'  `date` column as `birth_date + 18 years`. The `value` column is set to
#'  `TRUE` for all rows. No sub-concepts are used.
#'
#' @param input_dir Input directory containing partitioned parquet files.
#' @param output_dir Output directory where the new partition will be written.
#' @param complex_concept_name Name of the new complex concept
#'  to create (used as partition value).
#' @param sub_concept_ids Character vector of sub_concept_ids to include
#'  (expected to be a single age sub-concept).
#'
#' @returns Invisibly returns a list summarizing the operation,
#'  including matched partitions and output path.
is_18_or_older <- function(
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

  # Open DuckDB connection
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Read from D3_SPELLS and calculate the complex concept
  DBI::dbExecute(
    con,
    glue::glue(
      "CREATE OR REPLACE VIEW {params$complex_concept_name} AS
        SELECT
          NULL AS unique_id,
          'D3_SPELLS' AS ori_table,
          person_id,
          TRUE AS value,
          birth_date + INTERVAL 18 YEARS AS date,
          '{params$complex_concept_name}' AS concept_id
        FROM read_parquet(
          '{config_project$create_complex_concepts$spells_dir}'
        ) AS d3_spells,
        "
    )
  )

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
