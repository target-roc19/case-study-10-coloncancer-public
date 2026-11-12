#' Export a PostgreSQL table to Parquet
#'
#' Exports a PostgreSQL table to a Parquet dataset on disk using an existing
#' DBI connection (`con`). Data is read in chunks so large tables can be
#' exported without loading the entire table into memory at once.
#'
#' Optional features:
#' \itemize{
#'   \item Hive-style partitioning with `partition_on`
#'   \item Sorting in PostgreSQL with `order_by`
#' }
#'
#' When partitioning is enabled, the function tries to avoid splitting the last
#' partition across chunk boundaries by carrying that partition forward into the
#' next chunk. This reduces the number of tiny Parquet files caused purely by
#' arbitrary fetch chunk boundaries.
#'
#' The output is always written to a subfolder named after the table:
#'
#' \preformatted{
#' out_dir/
#'   table_name/
#'     ...
#' }
#'
#' @param table A character string with the table name.
#' @param out_dir Root output directory for Parquet exports. A subdirectory with
#'   the table name will be created inside this folder.
#' @param schema A character string with the schema name. Default: `"public"`.
#' @param partition_on Optional character vector with column name(s) to use for
#'   Hive partitioning. Default: `NULL`.
#' @param order_by Optional character vector with column name(s) to sort by in
#'   PostgreSQL. Default: `NULL`.
#' @param chunk_size Number of rows to fetch per chunk. Default: `1000000`.
#' @param overwrite Logical. If `TRUE`, an existing table output folder will be
#'   deleted first. Default: `FALSE`.
#' @param conn An existing DBI connection. Defaults to `con`.
#'
#' @return Invisibly returns a list with export metadata:
#' \describe{
#'   \item{out_dir}{Final dataset directory used for the export}
#'   \item{rows_exported}{Total number of exported rows}
#'   \item{chunks_fetched}{Number of fetch chunks read from PostgreSQL}
#'   \item{partition_on}{Partition columns used}
#'   \item{order_by}{Sort columns used}
#' }
#'
#' @details
#' If `partition_on` is supplied, `order_by` should start with the same columns
#' in the same order. For example:
#'
#' \preformatted{
#' partition_on = c("year", "month")
#' order_by     = c("year", "month", "person_id")
#' }
#'
#' This ensures records for the same partition are contiguous in the PostgreSQL
#' result, which greatly reduces fragmented partition output.
#'
#' Note that if a single partition is extremely large, it may still span
#' multiple fetch chunks. In that case, the carry-over buffer for the last
#' partition can grow substantially before it is written.
#'
#' For an empty source table, the function still writes a Parquet file with
#' zero rows and the correct schema.
#'
#' @examples
#' \dontrun{
#' export_postgres_to_parquet(
#'   table = "concept",
#'   schema = "cdm_synthea10",
#'   out_dir = "c:/tmp/parquet",
#'   partition_on = "vocabulary_id",
#'   order_by = c("vocabulary_id", "concept_id"),
#'   overwrite = TRUE
#' )
#' }
#'
#' @importFrom DBI dbExistsTable dbQuoteIdentifier dbGetQuery dbSendQuery
#'   dbFetch dbHasCompleted dbClearResult Id
#' @importFrom arrow write_dataset write_parquet
#' @export
export_postgres_to_parquet <- function(table,
                                       out_dir,
                                       schema = "public",
                                       partition_on = NULL,
                                       order_by = NULL,
                                       chunk_size = 1000000L,
                                       overwrite = FALSE,
                                       conn = con) {
  # Helper function, R equivalent to SQL ISNULL()
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  # Little function to format the logging
  fmt_n <- function(x) {
    format(
      x,
      big.mark = ",",
      decimal.mark = ".",
      scientific = FALSE,
      trim = TRUE
    )
  }

  # Create safely quoted identifiers for columns
  quote_ident <- function(x) {
    vapply(
      x,
      FUN = function(xx) as.character(DBI::dbQuoteIdentifier(conn, xx)),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  }

  # Some formatting for the logging
  format_cols <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      "none"
    } else {
      paste(x, collapse = ", ")
    }
  }

  # Here we're doing the logic on carrying over rows to next chunk
  same_partition <- function(df, partition_cols, values) {
    if (nrow(df) == 0L) {
      return(rep(FALSE, 0L))
    }

    keep <- rep(TRUE, nrow(df))

    for (col in partition_cols) {
      keep <- keep & (
        (is.na(df[[col]]) & is.na(values[[col]])) |
          (df[[col]] == values[[col]])
      )
    }

    keep
  }

  # Get the partition values of the last row
  get_last_partition_values <- function(df, partition_cols) {
    row_ix <- nrow(df)
    stats::setNames(
      lapply(partition_cols, function(col) df[[col]][row_ix]),
      partition_cols
    )
  }

  # Make sure our partitions are sorted, otherwise we'll break our logic
  validate_order_by <- function(partition_on, order_by) {
    if (is.null(partition_on) || length(partition_on) == 0L) {
      return(invisible(NULL))
    }

    if (is.null(order_by) || length(order_by) < length(partition_on)) {
      stop(
        "If `partition_on` is provided, `order_by` must start with the same columns."
      )
    }

    if (!identical(order_by[seq_along(partition_on)], partition_on)) {
      stop(
        "If `partition_on` is provided, `order_by` must begin with exactly the same columns, in the same order."
      )
    }

    invisible(NULL)
  }

  ##################################
  #### Validation of parameters ####
  ##################################

  if (!inherits(conn, "DBIConnection")) {
    stop("`conn` must be a valid DBI connection.")
  }

  if (!is.character(table) || length(table) != 1L || is.na(table) || !nzchar(table)) {
    stop("`table` must be a non-empty character string.")
  }

  if (!is.character(schema) || length(schema) != 1L || is.na(schema) || !nzchar(schema)) {
    stop("`schema` must be a non-empty character string.")
  }

  if (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir) || !nzchar(out_dir)) {
    stop("`out_dir` must be a non-empty character string.")
  }

  if (!is.null(partition_on)) {
    if (!is.character(partition_on) || anyNA(partition_on) || any(!nzchar(partition_on))) {
      stop("`partition_on` must be `NULL` or a character vector of valid column names.")
    }
    partition_on <- unique(partition_on)
  }

  if (!is.null(order_by)) {
    if (!is.character(order_by) || anyNA(order_by) || any(!nzchar(order_by))) {
      stop("`order_by` must be `NULL` or a character vector of valid column names.")
    }
    order_by <- unique(order_by)
  }

  validate_order_by(partition_on, order_by)

  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1) {
    stop("`chunk_size` must be a positive number.")
  }
  chunk_size <- as.integer(chunk_size)

  table_id <- DBI::Id(schema = schema, table = table)

  if (!DBI::dbExistsTable(conn, table_id)) {
    stop(sprintf("Table '%s.%s' does not exist.", schema, table))
  }

  full_table_id <- paste(
    as.character(DBI::dbQuoteIdentifier(conn, schema)),
    as.character(DBI::dbQuoteIdentifier(conn, table)),
    sep = "."
  )

  preview_sql <- sprintf("SELECT * FROM %s LIMIT 0", full_table_id)
  empty_template <- DBI::dbGetQuery(conn, preview_sql)
  cols_in_table <- names(empty_template)

  missing_partition <- setdiff(partition_on %||% character(0), cols_in_table)
  missing_order <- setdiff(order_by %||% character(0), cols_in_table)

  if (length(missing_partition) > 0L) {
    stop(
      "Column(s) in `partition_on` not found in the table: ",
      paste(missing_partition, collapse = ", ")
    )
  }

  if (length(missing_order) > 0L) {
    stop(
      "Column(s) in `order_by` not found in the table: ",
      paste(missing_order, collapse = ", ")
    )
  }

  ###########################################
  #### Remove output directory if exists ####
  ###########################################

  dataset_dir <- file.path(out_dir, table)

  if (dir.exists(dataset_dir)) {
    if (isTRUE(overwrite)) {
      message(sprintf("Output directory already exists and will be removed: %s", dataset_dir))
      unlink(dataset_dir, recursive = TRUE, force = TRUE)
    } else {
      stop(sprintf(
        "Output directory already exists: %s\nUse `overwrite = TRUE` to remove it first.",
        dataset_dir
      ))
    }
  }

  dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)

  #######################################
  #### The SQL query to get the data ####
  #######################################

  sql <- sprintf("SELECT * FROM %s", full_table_id)

  if (!is.null(order_by) && length(order_by) > 0L) {
    sql <- paste0(
      sql,
      "\nORDER BY ",
      paste(quote_ident(order_by), collapse = ", ")
    )
  }

  #################
  #### Logging ####
  #################

  message("Starting export PostgreSQL -> Parquet")
  message(sprintf("Table            : %s.%s", schema, table))
  message(sprintf(
    "Output directory : %s",
    normalizePath(dataset_dir, winslash = "/", mustWork = FALSE)
  ))
  message(sprintf("Partition_on     : %s", format_cols(partition_on)))
  message(sprintf("Order_by         : %s", format_cols(order_by)))
  message(sprintf("Chunk size       : %s", fmt_n(chunk_size)))

  if (!is.null(order_by) && length(order_by) > 0L) {
    message("Sorting is performed in PostgreSQL using ORDER BY.")
  }

  if (!is.null(partition_on) && length(partition_on) > 0L) {
    message("The last partition in each chunk is carried over to avoid splitting partitions across chunk boundaries.")
  } else {
    message("No partitioning requested: each fetched chunk will be written as a separate Parquet file.")
  }

  ###############################
  #### Now the actual export ####
  ###############################

  start_time <- Sys.time()
  res <- DBI::dbSendQuery(conn, sql)
  on.exit(DBI::dbClearResult(res), add = TRUE)

  total_rows <- 0L
  chunk_id <- 0L
  files_written <- 0L
  carry <- NULL

  # The function to write the actual files
  write_data <- function(df) {
    if (nrow(df) == 0L) {
      return(invisible(NULL))
    }

    if (!is.null(partition_on) && length(partition_on) > 0L) {
      arrow::write_dataset(
        dataset = df,
        path = dataset_dir,
        format = "parquet",
        partitioning = partition_on,
        hive_style = TRUE,
        existing_data_behavior = "overwrite",
        max_open_files = 1000
      )
    } else {
      files_written <<- files_written + 1L
      arrow::write_parquet(
        df,
        sink = file.path(dataset_dir, sprintf("part-%05d.parquet", files_written))
      )
    }

    invisible(NULL)
  }

  # And the chunking of data
  repeat {
    chunk <- DBI::dbFetch(res, n = chunk_size)

    if (nrow(chunk) == 0L) {
      break
    }

    chunk_id <- chunk_id + 1L
    total_rows <- total_rows + nrow(chunk)

    message(sprintf(
      "Chunk %s fetched (%s rows, total: %s).",
      chunk_id,
      fmt_n(nrow(chunk)),
      fmt_n(total_rows)
    ))

    if (!is.null(carry) && nrow(carry) > 0L) {
      chunk <- rbind(carry, chunk)
      message(sprintf("Carry-over added (%s rows).", fmt_n(nrow(carry))))
      carry <- NULL
    }

    if (!is.null(partition_on) && length(partition_on) > 0L) {
      last_vals <- get_last_partition_values(chunk, partition_on)
      is_last_partition <- same_partition(chunk, partition_on, last_vals)

      carry <- chunk[is_last_partition, , drop = FALSE]
      write_chunk <- chunk[!is_last_partition, , drop = FALSE]

      message(sprintf(
        "Chunk %s split: %s writable rows, %s carry-over rows for the last partition.",
        chunk_id,
        fmt_n(nrow(write_chunk)),
        fmt_n(nrow(carry))
      ))

      if (nrow(write_chunk) > 0L) {
        write_data(write_chunk)
        message(sprintf("Chunk %s written.", chunk_id))
      } else {
        message(sprintf(
          "Chunk %s not written yet; the full combined chunk belongs to the same trailing partition.",
          chunk_id
        ))
      }
    } else {
      write_data(chunk)
      message(sprintf("Chunk %s written.", chunk_id))
    }

    if (DBI::dbHasCompleted(res)) {
      break
    }
  }

  if (!is.null(carry) && nrow(carry) > 0L) {
    message(sprintf("Writing final carry-over (%s rows).", fmt_n(nrow(carry))))
    write_data(carry)
  }

  # Empty table: still write a 0-row parquet file with correct schema
  if (total_rows == 0L) {
    files_written <- files_written + 1L
    empty_file <- file.path(dataset_dir, sprintf("part-%05d.parquet", files_written))

    arrow::write_parquet(
      empty_template,
      sink = empty_file
    )

    message("Table is empty: wrote a 0-row Parquet file with the correct schema.")
  }

  # And some verbosity
  duration_secs <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)

  message("Export completed.")
  message(sprintf("Total rows exported : %s", fmt_n(total_rows)))
  message(sprintf("Fetch chunks        : %s", fmt_n(chunk_id)))
  message(sprintf("Files written       : %s", fmt_n(files_written)))
  message(sprintf("Duration            : %s seconds", fmt_n(duration_secs)))

  invisible(list(
    out_dir = dataset_dir,
    rows_exported = total_rows,
    chunks_fetched = chunk_id,
    files_written = files_written,
    partition_on = partition_on,
    order_by = order_by
  ))
}
