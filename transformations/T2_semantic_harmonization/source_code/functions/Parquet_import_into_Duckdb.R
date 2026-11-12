#' Register Parquet datasets as DuckDB views
#'
#' Scans a root directory containing one subdirectory per dataset and creates a
#' DuckDB view for each dataset. The view name is the name of the immediate
#' subdirectory under `parquet_root_dir`.
#'
#' This function is designed to work with Parquet datasets written in the style:
#'
#' \preformatted{
#' parquet_root_dir/
#'   concept/
#'     part-00001.parquet
#'     ...
#'   person/
#'     gender_concept_id=8507/
#'       part-00001.parquet
#'     ...
#' }
#'
#' Hive partitioning is supported through DuckDB's `parquet_scan()` with
#' `hive_partitioning = TRUE`.
#'
#' @param parquet_root_dir Character string. Root folder containing one
#'   subfolder per dataset/table.
#' @param db_path Character string. Full path to the DuckDB database file to
#'   create or open.
#' @param schema Character string. Schema where the views should be created.
#'   Default: `"parquet"`.
#' @param overwrite_db Logical. If `TRUE` and the database file already exists,
#'   it will be deleted before creating a new database. Default: `FALSE`.
#' @param overwrite_views Logical. If `TRUE`, existing views with the same name
#'   will be replaced. If `FALSE`, the function errors when a view already
#'   exists. Default: `TRUE`.
#' @param include_empty_dirs Logical. If `TRUE`, subdirectories without Parquet
#'   files are included in the scan result and then skipped with a message.
#'   Default: `FALSE`.
#' @param recursive_root Logical. If `FALSE`, only immediate subdirectories of
#'   `parquet_root_dir` are treated as datasets. Default: `FALSE`.
#' @param hive_partitioning Logical. Passed to DuckDB `parquet_scan()`.
#'   If `TRUE` then the partition column is added as a column in the view.
#'   Default: `TRUE`.
#' @param union_by_name Logical. Passed to DuckDB `parquet_scan()`. Useful when
#'   schemas differ slightly across files. Default: `FALSE`.
#' @param filename Logical. Passed to DuckDB `parquet_scan()`. If `TRUE`, DuckDB
#'   adds a `filename` column. Default: `FALSE`.
#' @param dbdir Optional character string. Directory to create before opening
#'   `db_path`. If `NULL`, the parent directory of `db_path` is used.
#' @param return_connection Logical. If `TRUE`, the open DBI connection is
#'   returned and left open. If `FALSE`, the function disconnects before
#'   returning metadata. Default: `FALSE`.
#'
#' @return If `return_connection = FALSE`, invisibly returns a list with:
#' \describe{
#'   \item{db_path}{Path to the DuckDB database}
#'   \item{schema}{Schema used}
#'   \item{datasets_found}{Number of dataset folders found}
#'   \item{views_created}{Number of views created}
#'   \item{views_skipped}{Number of folders skipped}
#'   \item{views}{Character vector of created view names}
#' }}
#'
#' If `return_connection = TRUE`, returns a list with the same metadata plus the
#' open DBI connection in element `conn`.
#'
#' @details
#' For each dataset folder, all `.parquet` files below that folder are collected
#' recursively and registered in DuckDB as:
#'
#' \preformatted{
#' CREATE OR REPLACE VIEW "catalog"."schema"."dataset_name" AS
#' SELECT * FROM parquet_scan([...], hive_partitioning = true)
#' }
#'
#' Using an explicit file list avoids relying on filesystem glob semantics and
#' works well for both partitioned and non-partitioned datasets.
#'
#' @examples
#' \dontrun{
#' register_parquet_root_as_duckdb_views(
#'   parquet_root_dir = "c:/tmp/parquet",
#'   db_path = "c:/tmp/duckdb/parquet.duckdb",
#'   schema = "raw_parquet",
#'   overwrite_db = TRUE,
#'   overwrite_views = TRUE
#' )
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery Id
#' @importFrom duckdb duckdb
#' @export
register_parquet_root_as_duckdb_views <- function(parquet_root_dir,
                                                  db_path,
                                                  schema = "parquet",
                                                  overwrite_db = FALSE,
                                                  overwrite_views = TRUE,
                                                  include_empty_dirs = FALSE,
                                                  recursive_root = FALSE,
                                                  hive_partitioning = TRUE,
                                                  union_by_name = FALSE,
                                                  filename = FALSE,
                                                  dbdir = NULL,
                                                  return_connection = FALSE) {
  
  ###############################
  #### Some helper functions ####
  ###############################
  
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
  
  # Function for quoted identifiers
  sql_quote_string <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  # Helper function for safe SQL boleans  
  sql_bool <- function(x) {
    if (isTRUE(x)) "true" else "false"
  }
  
  # Normalize paths
  normalize_fs_path <- function(x) {
    normalizePath(x, winslash = "/", mustWork = FALSE)
  }
  
  # Ensure fully qualified names
  quote_ident_sql <- function(conn,
                              schema_name,
                              object_name = NULL,
                              catalog_name = NULL) {
    parts <- character(0)
    
    if (!is.null(catalog_name)) {
      parts <- c(parts, as.character(DBI::dbQuoteIdentifier(conn, catalog_name)))
    }
    
    parts <- c(parts, as.character(DBI::dbQuoteIdentifier(conn, schema_name)))
    
    if (!is.null(object_name)) {
      parts <- c(parts, as.character(DBI::dbQuoteIdentifier(conn, object_name)))
    }
    
    paste(parts, collapse = ".")
  }
  
  # Fucntion to get all dataset folders
  list_dataset_dirs <- function(root_dir, recursive = FALSE) {
    if (!recursive) {
      dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
    } else {
      dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
      dirs <- dirs[normalize_fs_path(dirs) != normalize_fs_path(root_dir)]
    }
    
    dirs[file.info(dirs)$isdir %in% TRUE]
  }
  
  # The function to create the SQL parquet scan
  build_parquet_scan_sql <- function(files,
                                     hive_partitioning,
                                     union_by_name,
                                     filename) {
    file_sql <- paste(sql_quote_string(files), collapse = ", ")
    
    paste0(
      "SELECT * FROM parquet_scan([",
      file_sql,
      "], hive_partitioning = ", sql_bool(hive_partitioning),
      ", union_by_name = ", sql_bool(union_by_name),
      ", filename = ", sql_bool(filename),
      ")"
    )
  }
  
  ##################################
  #### Validation of parameters ####
  ##################################
  
  if (!is.character(parquet_root_dir) || length(parquet_root_dir) != 1L ||
      is.na(parquet_root_dir) || !nzchar(parquet_root_dir)) {
    stop("`parquet_root_dir` must be a non-empty character scalar.")
  }
  
  if (!dir.exists(parquet_root_dir)) {
    stop(sprintf("`parquet_root_dir` does not exist: %s", parquet_root_dir))
  }
  
  if (!is.character(db_path) || length(db_path) != 1L ||
      is.na(db_path) || !nzchar(db_path)) {
    stop("`db_path` must be a non-empty character scalar.")
  }
  
  if (!is.character(schema) || length(schema) != 1L ||
      is.na(schema) || !nzchar(schema)) {
    stop("`schema` must be a non-empty character scalar.")
  }
  
  if (!is.logical(overwrite_db) || length(overwrite_db) != 1L || is.na(overwrite_db)) {
    stop("`overwrite_db` must be TRUE or FALSE.")
  }
  
  if (!is.logical(overwrite_views) || length(overwrite_views) != 1L || is.na(overwrite_views)) {
    stop("`overwrite_views` must be TRUE or FALSE.")
  }
  
  if (!is.logical(include_empty_dirs) || length(include_empty_dirs) != 1L || is.na(include_empty_dirs)) {
    stop("`include_empty_dirs` must be TRUE or FALSE.")
  }
  
  if (!is.logical(recursive_root) || length(recursive_root) != 1L || is.na(recursive_root)) {
    stop("`recursive_root` must be TRUE or FALSE.")
  }
  
  if (!is.logical(hive_partitioning) || length(hive_partitioning) != 1L || is.na(hive_partitioning)) {
    stop("`hive_partitioning` must be TRUE or FALSE.")
  }
  
  if (!is.logical(union_by_name) || length(union_by_name) != 1L || is.na(union_by_name)) {
    stop("`union_by_name` must be TRUE or FALSE.")
  }
  
  if (!is.logical(filename) || length(filename) != 1L || is.na(filename)) {
    stop("`filename` must be TRUE or FALSE.")
  }
  
  if (!is.null(dbdir)) {
    if (!is.character(dbdir) || length(dbdir) != 1L || is.na(dbdir) || !nzchar(dbdir)) {
      stop("`dbdir` must be NULL or a non-empty character scalar.")
    }
  }
  
  if (!is.logical(return_connection) || length(return_connection) != 1L || is.na(return_connection)) {
    stop("`return_connection` must be TRUE or FALSE.")
  }
  
  ################################
  #### Database path handling ####
  ################################
  
  dbdir <- if (is.null(dbdir)) dirname(db_path) else dbdir
  dir.create(dbdir, recursive = TRUE, showWarnings = FALSE)
  
  db_path <- normalize_fs_path(db_path)
  parquet_root_dir <- normalize_fs_path(parquet_root_dir)
  
  if (file.exists(db_path) && isTRUE(overwrite_db)) {
    message(sprintf("Existing DuckDB database will be removed: %s", db_path))
    unlink(db_path, force = TRUE)
  }
  
  ##################################
  #### Discover dataset folders ####
  ##################################
  
  dataset_dirs <- list_dataset_dirs(parquet_root_dir, recursive = recursive_root)
  
  if (length(dataset_dirs) == 0L) {
    stop(sprintf("No dataset folders found under: %s", parquet_root_dir))
  }
  
  dataset_dirs <- normalize_fs_path(dataset_dirs)
  dataset_names <- basename(dataset_dirs)
  
  #######################################
  #### Open DuckDB and create schema ####
  #######################################
  
  message("Starting Parquet -> DuckDB view registration")
  message(sprintf("Parquet root      : %s", parquet_root_dir))
  message(sprintf("DuckDB database   : %s", db_path))
  message(sprintf("Schema            : %s", schema))
  message(sprintf("Dataset folders   : %s", fmt_n(length(dataset_dirs))))
  message(sprintf("Overwrite DB      : %s", overwrite_db))
  message(sprintf("Overwrite views   : %s", overwrite_views))
  message(sprintf("Hive partitioning : %s", hive_partitioning))
  message(sprintf("Union by name     : %s", union_by_name))
  message(sprintf("Filename column   : %s", filename))
  
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  conn_open <- TRUE
  
  # Close connection if return_connection is not true
  on.exit({
    if (conn_open && !isTRUE(return_connection)) {
      DBI::dbDisconnect(conn, shutdown = TRUE)
    }
  }, add = TRUE)
  
  catalog_name <- DBI::dbGetQuery(
    conn,
    "SELECT current_database() AS db"
  )$db[[1]]
  
  DBI::dbExecute(
    conn,
    sprintf(
      "CREATE SCHEMA IF NOT EXISTS %s",
      quote_ident_sql(
        conn,
        schema_name = schema,
        catalog_name = catalog_name
      )
    )
  )
  
  ########################################################
  #### And now we're going to create the actual views ####
  ########################################################
  
  created_views <- character(0)
  skipped_views <- character(0)
  
  # Loop over all datasets
  for (i in seq_along(dataset_dirs)) {
    dataset_dir <- dataset_dirs[i]
    view_name <- dataset_names[i]
    
    # List all files per dataset
    parquet_files <- list.files(
      path = dataset_dir,
      pattern = "\\.parquet$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    # Normalize the pathj
    parquet_files <- normalize_fs_path(parquet_files)
    
    # Report if no files are found
    if (length(parquet_files) == 0L) {
      msg <- sprintf(
        "Skipping '%s': no Parquet files found under %s",
        view_name, dataset_dir
      )
      
      message(msg)
      skipped_views <- c(skipped_views, view_name)
      next
    }
    
    # Create fully qualified view name
    fq_view_name <- quote_ident_sql(
      conn,
      schema_name = schema,
      object_name = view_name,
      catalog_name = catalog_name
    )
    
    # If overwrite_views is TRUE then create or replace target view
    if (isTRUE(overwrite_views)) {
      create_sql <- sprintf(
        "CREATE OR REPLACE VIEW %s AS %s",
        fq_view_name,
        build_parquet_scan_sql(
          files = parquet_files,
          hive_partitioning = hive_partitioning,
          union_by_name = union_by_name,
          filename = filename
        )
      )
    } 
    
    # Else create the view ...
    else {
      exists_sql <- sprintf(
        paste(
          "SELECT COUNT(*) AS n",
          "FROM information_schema.views",
          "WHERE table_schema = %s",
          "  AND table_name = %s"
        ),
        sql_quote_string(schema),
        sql_quote_string(view_name)
      )
      
      view_exists <- DBI::dbGetQuery(conn, exists_sql)$n[[1]] > 0L
      
      # .... but warn if it already exists
      if (isTRUE(view_exists)) {
        stop(sprintf(
          "View already exists and `overwrite_views = FALSE`: %s.%s",
          schema, view_name
        ))
      }
      
      create_sql <- sprintf(
        "CREATE VIEW %s AS %s",
        fq_view_name,
        build_parquet_scan_sql(
          files = parquet_files,
          hive_partitioning = hive_partitioning,
          union_by_name = union_by_name,
          filename = filename
        )
      )
    }
    
    # Some verbosity
    message(sprintf(
      "Creating view %s from %s Parquet file(s).",
      fq_view_name,
      fmt_n(length(parquet_files))
    ))
    
    # And execute the query to create the view
    DBI::dbExecute(conn, create_sql)
    created_views <- c(created_views, view_name)
  }
  
  # Summary
  message("Registration completed.")
  message(sprintf("Views created: %s", fmt_n(length(created_views))))
  message(sprintf("Views skipped: %s", fmt_n(length(skipped_views))))

result <- list(
  db_path = db_path,
  schema = schema,
  datasets_found = length(dataset_dirs),
  views_created = length(created_views),
  views_skipped = length(skipped_views),
  views = created_views
)

# Should the connection stay open, if not close
if (isTRUE(return_connection)) {
  result$conn <- conn
  conn_open <- FALSE
  return(result)
}

invisible(result)
}