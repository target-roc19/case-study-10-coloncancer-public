#' Create a new complex concept by applying AND logic to existing sub-concepts
#'  where all sub-concepts must be present on the same date.
#'
#'  This function ensures that for each person, all specified sub-concepts
#'  are present and share the same `date`. If this condition is met, the
#'  complex concept is marked as TRUE.
#'
#' @param input_dir Input directory containing partitioned parquet files.
#' @param output_dir Output directory where the new partition will be written.
#' @param complex_concept_name Name of the new complex concept
#'  to create (used as partition value).
#' @param sub_concept_ids Character vector of sub_concept_ids to include.
#'
#' @returns Invisibly returns a list summarizing the operation,
#'  including matched partitions and output path.
create_and_samedate <- function(
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

  # Open DuckDB connection
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  concept_id_list <- paste(
    glue::glue("'{existing_sub_concepts}'"),
    collapse = ", "
  )

  # AND logic with same date condition
  # NULL for unique_id and ori_table
  DBI::dbExecute(
    con,
    glue::glue(
      "CREATE OR REPLACE VIEW {params$complex_concept_name} AS
        SELECT
          NULL AS unique_id,
          NULL AS ori_table,
          person_id,
          date,
          '{params$complex_concept_name}' AS concept_id,
          TRUE AS value
        FROM (
          SELECT
            person_id,
            date,
            COUNT(DISTINCT concept_id) AS concept_count
          FROM read_parquet(
            ['{params$input_dir}/**/*.parquet'],
            hive_partitioning = true,
            union_by_name     = true
          )
          WHERE concept_id IN ({concept_id_list})
          GROUP BY person_id, date
        )
        WHERE concept_count = {length(existing_sub_concepts)}"
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
