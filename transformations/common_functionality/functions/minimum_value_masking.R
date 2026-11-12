#' Applies minimum cell-count masking to selected count columns and their
#' associated percentage columns within specific row groups of a dataset.
#'
#' Rows are masked only when:
#' - the row belongs to one of the specified row filter categories
#' - AND at least one value column contains a non-zero value below the
#'   masking threshold (usually <5).
#'
#' Count columns meeting the masking condition are replaced with a string
#' representation of the threshold (e.g. "<5"), while associated percentage
#' columns are set to NA.
#'
#' @param dataset A data.table containing the summary table
#'   to be masked.
#'
#' @param value_columns Character vector containing the names of count
#'   columns to evaluate and mask.
#'
#' @param percentage_columns Character vector containing the names of
#'   percentage columns associated with `value_columns`. Percentage columns
#'   are masked whenever their corresponding count row is masked (with NA).
#'
#' @param min_value_masking Numeric threshold below which non-zero counts
#'   are masked.
#'
#' @param row_filter_col Character string specifying the column used to
#'   identify rows eligible for masking.
#'   Default: `"type"`.
#'
#' @param row_filter_values Character vector specifying the values within
#'   `row_filter_col` that are eligible for masking.
#'   Default: `c("CAT", "TF")`.
#'
#' @return A data.table where:
#' - selected count columns are masked with values like `"<5"`
#' - associated percentage columns are set to `NA`
#' - all other rows and columns remain unchanged.

minimum_value_masking <- function(
    dataset,
    value_columns,
    percentage_columns,
    min_value_masking,
    row_filter_col = "type",
    row_filter_values = c("CAT", "TF")
) {
  
  dt <- data.table::copy(dataset)
  
  cols_to_mask <- c(value_columns, percentage_columns)
  
  # Identify rows to mask
  mask_rows <- dt[[row_filter_col]] %in% row_filter_values
  
  # Store numeric values for comparison
  numeric_dt <- dt[, lapply(.SD, as.numeric), .SDcols = value_columns]
  
  # Find rows where masking applies
  mask_logic <- Reduce(`|`, lapply(numeric_dt, function(x) {
    x < min_value_masking & x != 0
  }))
  
  final_rows <- mask_rows & mask_logic
  
  # Convert target columns to character 
  dt[, (cols_to_mask) := lapply(.SD, as.character), .SDcols = cols_to_mask]
  
  # Apply masking to selected rows + columns
  dt[final_rows, (value_columns) := lapply(.SD, function(x) {
    paste0("<", min_value_masking)
  }), .SDcols = value_columns]
  
  dt[final_rows, (percentage_columns) := lapply(.SD, function(x) {
    NA_character_
  }), .SDcols = percentage_columns]
  
  return(dt)
}
