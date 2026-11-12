#' Select the first matching record per person and keep its date.
#'
#' For each person, this function looks across the requested sub-concepts,
#' keeps the earliest matching row, and writes that row's `date` to the output
#' partition for the derived concept.
#'
#' @param input_dir Input directory containing partitioned parquet files.
#' @param output_dir Output directory where the new partition will be written.
#' @param complex_concept_name Name of the new complex concept
#'  to create (used as partition value).
#' @param sub_concept_ids Character vector of sub_concept_ids to include.
#'
#' @returns Invisibly returns a list summarizing the operation,
#'  including matched partitions and output path.
select_first_record <- function(
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

  DBI::dbExecute(
    con,
    glue::glue(
      "CREATE OR REPLACE VIEW {params$complex_concept_name} AS
      SELECT
        unique_id,
        ori_table,
        person_id,
        date,
        '{params$complex_concept_name}' AS concept_id,
        TRUE AS value
      FROM read_parquet(
        ['{params$input_dir}/**/*.parquet'],
        hive_partitioning = true,
        union_by_name = true
      )
      WHERE concept_id IN ({concept_id_list})
      QUALIFY ROW_NUMBER() OVER (
        PARTITION BY person_id
        ORDER BY date ASC
      ) = 1"
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
          "DuckDB COPY failed for '{params$complex_concept_name}':\n 
            {conditionMessage(e)}"
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
    'Counts:\n',
    paste(capture.output(print(t(counts), sep = ":"))[-1], collapse = '\n')
  ))
}
