#' Create descriptive summary tables for overall and grouped populations
#'
#' Generates descriptive statistics tables for a study population using
#' `DescriptivesTable()`. The function creates:
#'   an overall summary table for all selected observations
#'   a grouped summary table comparing exposed vs control groups
#'
#' @param dt A data.table containing the study population data. Must include
#'   a grouping column named `group` and all variables required by
#'   `DescriptivesTable()`.
#'
#' @param matched_groups A character vector specifying which values of the
#'   `group` column should be included in the grouped comparison.
#'   Defaults to `c("control", "exposed")`.
#'   If `NULL`, all rows in `dt` are included.
#'
#' @param overall_label A character string used as the label for the overall
#'   population grouping in the overall summary table.
#'   Defaults to `"Overall"`.
#'
#' @param overall_groupvar A character string specifying the temporary column
#'   name used internally to create the overall grouping variable.
#'   Defaults to `"group_overall"`.
#'
#' @return A named list with two data.tables:
#' \describe{
#'   \item{overall}{Descriptive statistics for the overall eligible population.}
#'   \item{grouped}{Descriptive statistics comparing exposed and control groups,
#'   including ASD values.}
#' }
#'
#' @details
#' The function filters the input data to the selected `matched_groups`
#' (if provided), then:
#' \enumerate{
#'   \item Creates an overall descriptive table by assigning all observations
#'         to a single artificial group.
#'   \item Creates a grouped descriptive table using the original `group`
#'         variable.
#' }
#'
#' The function depends on externally available objects/functions:
#' \itemize{
#'   \item `DescriptivesTable()`
#' }
#'
#' @param table_metadata A data.table containing variable metadata for descriptive table generation.
#' @param label_lookup A data.table containing label lookups for descriptive table generation.
#'
#' @examples
#' result <- create_desc_tables(
#'   dt = cohort_dt,
#'   matched_groups = c("control", "exposed"),
#'   table_metadata = my_metadata,
#'   label_lookup = my_labels
#' )
#'
#' result$overall
#' result$grouped

create_desc_tables <- function(
  dt,
  matched_groups = c("control", "exposed"),
  overall_label = "Overall",
  overall_groupvar = "group_overall",
  table_metadata = NULL,
  label_lookup = NULL,
  use_weights = FALSE,
  weighted_stats = FALSE
) {
  library(DescriptiveTable)

  # select data (for matched/unmatched)
  dt_group <- if (!is.null(matched_groups)) {
    data.table::copy(dt[group %in% matched_groups])
  } else {
    data.table::copy(dt)
  }

  # for the overall (all eligible pop) create a column to use inside the table
  # as grouping column
  overall_dt <- data.table::copy(dt_group)[,
    (overall_groupvar) := overall_label
  ][,
    !"group" # remove the existing grouping variable
  ]

  overall_tbl <- DescriptiveTable::DescriptivesTable(
    popdf = overall_dt,
    groupcol = overall_groupvar,
    table_metadata = table_metadata,
    output_format = "processed",
    calculate_asd = FALSE, # this is FALSE as we only have one group
    keep_varinfo = TRUE,
    missing_flag = -99,
    round_decimals = 1,
    label_lookup = label_lookup,
    output_asd = FALSE,
    control_types = FALSE
  )[
    label != "",
    .(
      label,
      N_overall = V1_Overall,
      perc_overall = V2_Overall,
      aux_overall = V3_Overall,
      var,
      type
    )
  ]

  # table for the exposed/controls
  # ASDs are added afterwards by asd_by_level(), which also reports one value
  # per category level
  grouped_tbl <- DescriptiveTable::DescriptivesTable(
    popdf = dt_group,
    groupcol = "group",
    table_metadata = table_metadata,
    output_format = "processed",
    calculate_asd = FALSE,
    keep_varinfo = TRUE,
    missing_flag = -99,
    round_decimals = 1,
    label_lookup = label_lookup,
    output_asd = FALSE,
    control_types = FALSE,
    use_weights = use_weights,
    weighted_stats = weighted_stats
  )[
    label != "",
    .(
      label,
      N_exposed = V1_exposed,
      perc_exposed = V2_exposed,
      aux_exposed = V3_exposed,
      N_control = V1_control,
      perc_control = V2_control,
      aux_control = V3_control,
      var,
      type,
      cat
    )
  ]

  # relabel from the category values
  if (!is.null(label_lookup)) {
    grouped_tbl[
      data.table::as.data.table(label_lookup),
      on = c(var = "VarName", cat = "integerVal"),
      label := i.category
    ]
  }

  # return a list with overall and group tables.
  list(
    overall = overall_tbl,
    grouped = grouped_tbl
  )
}


#' Absolute standardized differences per variable and per category level
#'
#' `DescriptiveTable::compute_asd()` reports a single ASD for a categorical
#' variable. To also report one per level, every level is recoded as a dummy
#' TF variable and sent back through `compute_asd()`.
#'
#' @param dt data.table holding `group`, the metadata variables and, for
#'   weighted ASDs, the weights column.
#' @param table_metadata Metadata used for the table.
#' @param use_weights FALSE, or the name of the weights column.
#'
#' @return data.table with `var`, `cat` (NA on the variable-level value) and `asd`.
asd_by_level <- function(dt, table_metadata, use_weights = FALSE) {
  weight_col <- if (isFALSE(use_weights)) NULL else use_weights

  dt_asd <- data.table::copy(dt)[
    group %in% c("control", "exposed"),
    intersect(c("group", weight_col, table_metadata$var), names(dt)),
    with = FALSE
  ]

  # need to change this once PR#20 on DescriptiveTable gets merged
  dt_asd[, group := toupper(group)]

  var_asd <- data.table::as.data.table(DescriptiveTable::compute_asd(
    CovariatesInformation = dt_asd,
    metadata = table_metadata[, .(var, type)],
    referenceExtension = "1",
    use_weights = use_weights,
    group_names = c("CONTROL", "EXPOSED")
  ))

  # one dummy variable per expected level of each categorical variable
  levels_dt <- table_metadata[
    type == "CAT" & !is.na(expectedCat) & var %in% names(dt_asd),
    .(cat = trimws(strsplit(expectedCat, ",")[[1]])),
    by = var
  ]
  levels_dt[, dummy := paste0("dummy_", .I)]

  for (i in seq_len(nrow(levels_dt))) {
    dt_asd[,
      (levels_dt$dummy[i]) := as.character(get(levels_dt$var[i])) ==
        levels_dt$cat[i]
    ]
  }

  level_asd <- data.table::as.data.table(DescriptiveTable::compute_asd(
    CovariatesInformation = dt_asd,
    metadata = data.table::data.table(var = levels_dt$dummy, type = "TF"),
    referenceExtension = "1",
    use_weights = use_weights,
    group_names = c("CONTROL", "EXPOSED")
  ))

  levels_dt <- merge(
    levels_dt,
    level_asd[, .(dummy = var, asd = as.numeric(asd_1))],
    by = "dummy",
    sort = FALSE
  )

  data.table::rbindlist(list(
    var_asd[!is.na(var), .(var, cat = NA_character_, asd = as.numeric(asd_1))],
    levels_dt[, .(var, cat, asd)]
  ))
}

#' Write ASDs onto a descriptive table
#'
#' Variable-level values go on the row carrying the metadata label: the header
#' row of a numeric or categorical variable, or the single row of a TF
#' variable. Per-level values go on the category rows underneath.
#'
#' @param tbl Grouped table from `create_desc_tables()`.
#' @param asd_dt Output of `asd_by_level()`.
#' @param table_metadata Metadata used for the table.
#'
#' @return `tbl` with an `asd` column.
attach_asd <- function(tbl, asd_dt, table_metadata) {
  tbl <- data.table::copy(tbl)
  tbl[, `:=`(cat = as.character(cat), asd = NA_real_)]

  var_level <- merge(
    table_metadata[, .(var, label)],
    asd_dt[is.na(cat), .(var, asd)],
    by = "var"
  )
  tbl[var_level, on = "label", asd := i.asd]

  tbl[asd_dt[!is.na(cat)], on = c("var", "cat"), asd := i.asd]

  # the weighted ASDs come back unrounded, the unweighted ones at stddiff's 3 dp
  tbl[, asd := round(asd, 3)]

  tbl[]
}
