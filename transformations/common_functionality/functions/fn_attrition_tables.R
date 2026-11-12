################################################################
# fn_attrition_tables ---
# Aim:
# Build attrition tables for D3 eligibility and matched D4 cohorts.
#
# Tables:
# - Table 1.1: entry into eligible population
# - Table 2: matched cohort main analysis (without replacement)
################################################################

# Convert common encodings to logical without changing source data.
.as_logical <- function(x) {
  # Keep logical vectors unchanged.
  if (is.logical(x)) {
    return(x)
  }
  # Treat non-zero numeric/integer values as TRUE.
  if (is.numeric(x) || is.integer(x)) {
    return(x != 0)
  }
  # Normalize common string representations used in extracts.
  if (is.character(x)) {
    x_up <- toupper(trimws(x))
    return(x_up %in% c("TRUE", "T", "1", "YES", "Y"))
  }
  # Fallback for other classes.
  as.logical(x)
}

.assert_required_cols <- function(dt, required_cols, table_name) {
  # Fail early with a clear message when expected input fields are absent.
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "%s is missing required columns: %s",
        table_name,
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

.pct <- function(numerator, denominator, digits = 1) {
  # Return blank-compatible NA when denominator is undefined or zero.
  if (is.na(denominator) || denominator == 0) {
    return(NA_character_)
  }
  format(round((numerator / denominator) * 100, digits), nsmall = digits)
}

.format_count <- function(x) {
  # Keep counts as character so output tables are report-ready on write.
  if (is.na(x)) {
    return(NA_character_)
  }
  format(x, scientific = FALSE, trim = TRUE)
}

.finalize_attrition_table <- function(dt, return_na = FALSE) {
  out <- data.table::copy(dt)
  if (!return_na) {
    # Replace missing display cells with empty strings for final CSV shells.
    out[is.na(N), N := ""]
    out[is.na(Percentage), Percentage := ""]
    out[is.na(Aux), Aux := ""]
  }
  out[]
}

#' Table 1.1: Attrition table for entry into the eligible population.
#'
#' @param d3_eligibility D3 eligibility dataset.
#' @param digits Number of decimals for percentages.
#' @param return_na Keep NA values if TRUE; otherwise replace with empty strings.
#' @param return_details Return the table and ECOG missingness diagnostics as a list.
#'
#' @return data.table with columns Label, N, Percentage, Aux, or a list when
#'   return_details is TRUE.
create_attrition_table_1_1 <- function(
  d3_eligibility,
  digits = 1,
  return_na = FALSE,
  return_details = FALSE
) {
  # Work on a copy to avoid modifying upstream objects by reference.
  dt <- data.table::as.data.table(data.table::copy(d3_eligibility))

  .assert_required_cols(
    dt,
    c(
      "SV_FIRST_CRC_DIAGNOSIS", "SV_CRC_DIAGNOSIS", "SV_STAGE4",
      "SV_DIAGNOSED_IN_2015", "SV_DIAGNOSED_IN_2021",
      "SV_AGE_18_OR_OLDER", "SV_ECOG_STATUS", "SV_RT_EXCL", "SV_SX_EXCL", "SV_DIAG_EXCL",
      "SV_CAPECITABINE", "SV_OXALIPLATIN", "SV_BEVACIZUMAB",
      "receives_exposure", "receives_control",
      "in_eligible_pop", "eligible_exposed", "eligible_control"
    ),
    "d3_eligibility"
  )

  # Harmonize all logical flags used in shell criteria and totals.
  dt[, SV_FIRST_CRC_DIAGNOSIS := .as_logical(SV_FIRST_CRC_DIAGNOSIS)]
  dt[, SV_CRC_DIAGNOSIS := .as_logical(SV_CRC_DIAGNOSIS)]
  dt[, SV_STAGE4 := .as_logical(SV_STAGE4)]
  dt[, SV_DIAGNOSED_IN_2015 := .as_logical(SV_DIAGNOSED_IN_2015)]
  dt[, SV_DIAGNOSED_IN_2021 := .as_logical(SV_DIAGNOSED_IN_2021)]
  dt[, SV_AGE_18_OR_OLDER := .as_logical(SV_AGE_18_OR_OLDER)]
  dt[, SV_RT_EXCL := .as_logical(SV_RT_EXCL)]
  dt[, SV_SX_EXCL := .as_logical(SV_SX_EXCL)]
  dt[, SV_CAPECITABINE := .as_logical(SV_CAPECITABINE)]
  dt[, SV_OXALIPLATIN := .as_logical(SV_OXALIPLATIN)]
  dt[, SV_BEVACIZUMAB := .as_logical(SV_BEVACIZUMAB)]
  dt[, receives_exposure := .as_logical(receives_exposure)]
  dt[, receives_control := .as_logical(receives_control)]
  dt[, in_eligible_pop := .as_logical(in_eligible_pop)]
  dt[, eligible_exposed := .as_logical(eligible_exposed)]
  dt[, eligible_control := .as_logical(eligible_control)]

  # Unique people in the base source population (n).
  n_total <- dt[, data.table::uniqueN(person_id)]

  # Exposed pathway in the shell.
  # n_1: All individuals diagnosed with mCRC (stage IV) between 01/01/2015 and 30/06/2015 or 01/01/2021 and 20/06/2021 and
  # registered in the NCR
  n <- dt[SV_CRC_DIAGNOSIS == TRUE & SV_STAGE4 == TRUE &
    (SV_DIAGNOSED_IN_2015 == TRUE | SV_DIAGNOSED_IN_2021 == TRUE), data.table::uniqueN(person_id)]
  # take the person ids for row 2
  n_ids <- unique(dt[SV_CRC_DIAGNOSIS == TRUE & SV_STAGE4 == TRUE &
    (SV_DIAGNOSED_IN_2015 == TRUE | SV_DIAGNOSED_IN_2021 == TRUE), person_id])

  # n_1: AND started CapOx-B (capecitabine + oxaliplatin + bevacizumab) or CapOx (capecitabine + oxaliplatin)
  # this condition could be in a different spell
  receives_exp_con <- unique(dt[receives_exposure == TRUE | receives_control == TRUE, person_id])

  # that is why we intersect to get he person_ids meeting condition 1 and 2 in different spells
  n_1_ids <- intersect(n_ids, receives_exp_con)
  # take the length
  n_1 <- length(n_1_ids)
  # select all spells
  row2_spells <- dt[
    person_id %in% n_1_ids &
      (receives_exposure == TRUE | receives_control == TRUE)
  ]

  # n_2a: AND started index or reference drugs within 180 days from mCRC diagnosis
  n_2a <- row2_spells[SV_FIRST_CRC_DIAGNOSIS == TRUE, data.table::uniqueN(person_id)]
  # n_2b: AND are aged 18 at index
  ids_n2b <- unique(row2_spells[
    SV_FIRST_CRC_DIAGNOSIS == TRUE &
      SV_AGE_18_OR_OLDER == TRUE,
    person_id
  ])
  n_2b <- length(ids_n2b)
  # n_2c: AND, had an ECOG of 0 or 1
  ids_n2c <- unique(row2_spells[
    SV_FIRST_CRC_DIAGNOSIS == TRUE &
    SV_AGE_18_OR_OLDER == TRUE &
    SV_ECOG_STATUS %in% c(0, 1), person_id
  ])
  n_2c <- length(ids_n2c)

  # Count patients dropped at n_2c whose ECOG is missing across all episodes.
  dropped_n2b_n2c_ids <- setdiff(ids_n2b, ids_n2c)
  ecog_missing_by_person <- dt[
    person_id %in% dropped_n2b_n2c_ids,
    .(ecog_missing = all(is.na(SV_ECOG_STATUS))),
    by = person_id
  ]
  n_missing_ecog <- ecog_missing_by_person[ecog_missing == TRUE, .N]
  pct_missing_ecog <- .pct(n_missing_ecog, length(dropped_n2b_n2c_ids), digits)
  # n_2d: AND, had no previous cancer diagnosis that could potentially have been treated with
  # oxaliplatin and/or bevacizumab
  n_2d <- row2_spells[SV_FIRST_CRC_DIAGNOSIS == TRUE &
    SV_AGE_18_OR_OLDER == TRUE &
    SV_ECOG_STATUS %in% c(0, 1) &
    is.na(SV_DIAG_EXCL), data.table::uniqueN(person_id)]
  # n_2e: AND, if having undergone radiotherapy or surgery for mCRC, had this completed >= 4 weeks before T0
  n_2e <- row2_spells[SV_FIRST_CRC_DIAGNOSIS == TRUE &
    SV_AGE_18_OR_OLDER == TRUE &
    SV_ECOG_STATUS %in% c(0, 1) &
    is.na(SV_DIAG_EXCL) &
    (SV_RT_EXCL == FALSE & SV_SX_EXCL == FALSE), data.table::uniqueN(person_id)]

  n_cohort <- dt[eligible_exposed == TRUE | eligible_control == TRUE, data.table::uniqueN(person_id)]
  n_exposed <- dt[eligible_exposed == TRUE, data.table::uniqueN(person_id)] # Final exposed total after full eligibility logic.
  n_control <- dt[eligible_control == TRUE, data.table::uniqueN(person_id)] # Final unexposed total after full eligibility logic.

  # Build shell-aligned row labels and values in final reporting order.
  table_1a <- data.table::data.table(
    Label = c(
      "All individuals diagnosed with mCRC (stage IV) between 01 January 2015 - 30 June 2015 or
      01 January 2021 - 30 June 2021, and registered in the NCR",
      "    AND started CapOx-B (capecitabine + oxaliplatin + bevacizumab) or CapOx (capecitabine + oxaliplatin)",
      "    AND started index or reference drugs within 180 days from mCRC diagnosis",
      "    AND are aged 18 >= at index",
      "    AND had an Eastern Cooperative Oncology Group (ECOG) Performance Status (PS) of 0 or 1",
      "    AND had no previous cancer diagnosis that could potentially have been treated with oxaliplatin and/or bevacizumab ",
      "    AND if having undergone radiotherapy or surgery for mCRC, had this completed ≥ 4 weeks before T0",
      "Total Eligible Cohort",
      "      Eligible CapOx-B",
      "      Eligible CapOx"
    ),
    N = c(
      .format_count(n),
      .format_count(n_1),
      .format_count(n_2a),
      .format_count(n_2b),
      .format_count(n_2c),
      .format_count(n_2d),
      .format_count(n_2e),
      .format_count(n_cohort),
      .format_count(n_exposed),
      .format_count(n_control)
    ),
    Percentage = c(
      NA_character_, # Row 1 percentage intentionally blank.
      "100",
      .pct(n_2a, n_1, digits),
      .pct(n_2b, n_1, digits),
      .pct(n_2c, n_1, digits),
      .pct(n_2d, n_1, digits),
      .pct(n_2e, n_1, digits),
      .pct(n_cohort, n_1, digits),
      .pct(n_exposed, n_cohort, digits),
      .pct(n_control, n_cohort, digits)
    ),
    Aux = NA_character_
  )

  table_1_1 <- .finalize_attrition_table(table_1a, return_na = return_na)

  if (return_details) {
    return(list(
      table = table_1_1,
      n_missing_ecog = n_missing_ecog,
      pct_missing_ecog = pct_missing_ecog
    ))
  }

  table_1_1
}

#' Table 1(b): Matching statistics for the matched cohort main analysis.
#'
#' @param d3_eligibility D3 eligibility dataset.
#' @param d4_main D4 matched cohort for the main analysis.
#' @param digits Number of decimals for percentages.
#' @param return_na Keep NA values if TRUE; otherwise replace with empty strings.
#'
#' @return data.table with columns Label, N, Percentage, Aux.
create_attrition_table_1b <- function(d3_eligibility, d4_main, digits = 1, return_na = FALSE) {
  # Copy inputs to preserve caller state and coerce to data.table.
  d3 <- data.table::as.data.table(data.table::copy(d3_eligibility))
  d4 <- data.table::as.data.table(data.table::copy(d4_main))

  .assert_required_cols(d3, c("person_id", "eligible_exposed", "eligible_control"), "d3_eligibility")
  .assert_required_cols(d4, c("person_id", "group"), "d4_main")

  d3[, eligible_exposed := .as_logical(eligible_exposed)]
  d4[, group := as.character(group)]

  # Use eligible exposed as the cohort denominator for main matching metrics.
  n_cohort <- d3[eligible_exposed == TRUE | eligible_control == TRUE, data.table::uniqueN(person_id)] # Eligible exposed and control denominator from D3.
  n_matched_exp <- d4[group == "exposed", .N] # Number of matched exposed rows in D4 main.
  n_matched_con <- d4[group == "control", .N] # Number of matched control rows in D4 main.
  n_unmatched_exp <- d4[group == "unmatched" & receives_exposure == TRUE, .N] # Number of unmatched exposed rows in D4 main.
  n_matched <- n_matched_exp + n_matched_con # Total matched rows (exposed + control).

  table_1b <- data.table::data.table(
    Label = c(
      "Matched cohort",
      "  Exposed matched (CapOx-B)",
      "  Control matched individuals in the cohort (CapOx)",
      "  Unmatched exposed"
    ),
    N = c(
      .format_count(n_matched), # Row 1 count.
      .format_count(n_matched_exp), # Row 2 count.
      .format_count(n_matched_con), # Row 3 count.
      .format_count(n_unmatched_exp) # Row 4 count.
    ),
    Percentage = c(
      NA_character_, # Row 1 percentage intentionally blank.
      .pct(n_matched_exp, n_cohort, digits), # Row 2: n_matched_exp / n_cohort.
      .pct(n_matched_con, n_cohort, digits), # Row 3: n_matched_con / n_cohort.
      .pct(n_unmatched_exp, n_cohort, digits) # Row 4: n_unmatched_exp / n_cohort.
    ),
    Aux = NA_character_
  )

  .finalize_attrition_table(table_1b, return_na = return_na)
}


create_previous_malignancy_tab <- function(
    d3_eligibility,
    digits = 1,
    return_na = FALSE,
    debug = FALSE
) {
    # Work on a copy to avoid modifying upstream objects by reference.
  dt <- data.table::as.data.table(data.table::copy(d3_eligibility))

  .assert_required_cols(
    dt,
    c(
      "SV_FIRST_CRC_DIAGNOSIS", "SV_CRC_DIAGNOSIS", "SV_STAGE4",
      "SV_DIAGNOSED_IN_2015", "SV_DIAGNOSED_IN_2021",
      "SV_AGE_18_OR_OLDER", "SV_ECOG_STATUS", "SV_RT_EXCL", "SV_SX_EXCL", "SV_DIAG_EXCL",
      "SV_CAPECITABINE", "SV_OXALIPLATIN", "SV_BEVACIZUMAB",
      "receives_exposure", "receives_control",
      "in_eligible_pop", "eligible_exposed", "eligible_control"
    ),
    "d3_eligibility"
  )

  # Harmonize all logical flags used in shell criteria and totals.
  dt[, SV_FIRST_CRC_DIAGNOSIS := .as_logical(SV_FIRST_CRC_DIAGNOSIS)]
  dt[, SV_CRC_DIAGNOSIS := .as_logical(SV_CRC_DIAGNOSIS)]
  dt[, SV_STAGE4 := .as_logical(SV_STAGE4)]
  dt[, SV_DIAGNOSED_IN_2015 := .as_logical(SV_DIAGNOSED_IN_2015)]
  dt[, SV_DIAGNOSED_IN_2021 := .as_logical(SV_DIAGNOSED_IN_2021)]
  dt[, SV_AGE_18_OR_OLDER := .as_logical(SV_AGE_18_OR_OLDER)]
  dt[, SV_RT_EXCL := .as_logical(SV_RT_EXCL)]
  dt[, SV_SX_EXCL := .as_logical(SV_SX_EXCL)]
  dt[, SV_CAPECITABINE := .as_logical(SV_CAPECITABINE)]
  dt[, SV_OXALIPLATIN := .as_logical(SV_OXALIPLATIN)]
  dt[, SV_BEVACIZUMAB := .as_logical(SV_BEVACIZUMAB)]
  dt[, receives_exposure := .as_logical(receives_exposure)]
  dt[, receives_control := .as_logical(receives_control)]
  dt[, in_eligible_pop := .as_logical(in_eligible_pop)]
  dt[, eligible_exposed := .as_logical(eligible_exposed)]
  dt[, eligible_control := .as_logical(eligible_control)]

  # Unique people in the base source population (n).
  n_total <- dt[, data.table::uniqueN(person_id)]

  # Exposed pathway in the shell.
  # n_1: All individuals diagnosed with mCRC (stage IV) between 01/01/2015 and 30/06/2015 or 01/01/2021 and 20/06/2021 and
  # registered in the NCR
  n <- dt[SV_CRC_DIAGNOSIS == TRUE & SV_STAGE4 == TRUE &
    (SV_DIAGNOSED_IN_2015 == TRUE | SV_DIAGNOSED_IN_2021 == TRUE), data.table::uniqueN(person_id)]
  # take the person ids for row 2
  n_ids <- unique(dt[SV_CRC_DIAGNOSIS == TRUE & SV_STAGE4 == TRUE &
    (SV_DIAGNOSED_IN_2015 == TRUE | SV_DIAGNOSED_IN_2021 == TRUE), person_id])

  # n_1: AND started CapOx-B (capecitabine + oxaliplatin + bevacizumab) or CapOx (capecitabine + oxaliplatin)
  # this condition could be in a different spell
  receives_exp_con <- unique(dt[receives_exposure == TRUE | receives_control == TRUE, person_id])

  # that is why we intersect to get he person_ids meeting condition 1 and 2 in different spells
  n_1_ids <- intersect(n_ids, receives_exp_con)
  # take the length
  n_1 <- length(n_1_ids)
  # select all spells
  row2_spells <- dt[
    person_id %in% n_1_ids &
      (receives_exposure == TRUE | receives_control == TRUE)
  ]

  # n_2a: AND started index or reference drugs within 180 days from mCRC diagnosis
  n_2a <- row2_spells[SV_FIRST_CRC_DIAGNOSIS == TRUE, data.table::uniqueN(person_id)]
  # n_2b: AND are aged 18 at index
  n_2b <- row2_spells[SV_FIRST_CRC_DIAGNOSIS == TRUE &
    SV_AGE_18_OR_OLDER == TRUE, data.table::uniqueN(person_id)]
  # n_2c: AND, had an ECOG of 0 or 1
  ids_n2c <- unique(
    row2_spells[
      SV_FIRST_CRC_DIAGNOSIS == TRUE &
        SV_AGE_18_OR_OLDER == TRUE &
        SV_ECOG_STATUS %in% c(0, 1),
      person_id
    ]
  )

  n_2c <- length(ids_n2c)

  # n_2d: AND, had no previous cancer diagnosis that could potentially have been treated with
  # oxaliplatin and/or bevacizumab
  ids_n2d <- unique(
    row2_spells[
      SV_FIRST_CRC_DIAGNOSIS == TRUE &
        SV_AGE_18_OR_OLDER == TRUE &
        SV_ECOG_STATUS %in% c(0, 1) &
        is.na(SV_DIAG_EXCL),
      person_id
    ]
  )

  n_2d <- length(ids_n2d)

  # Patients lost between n_2c and n_2d
  dropped_n2c_n2d_ids <- setdiff(ids_n2c, ids_n2d)

  # Keep ALL rows for those patients
  dropped_n2c_n2d <- dt[
    person_id %in% dropped_n2c_n2d_ids
  ]
  # n_2e: AND, if having undergone radiotherapy or surgery for mCRC, had this completed >= 4 weeks before T0
  n_2e <- row2_spells[SV_FIRST_CRC_DIAGNOSIS == TRUE &
    SV_AGE_18_OR_OLDER == TRUE &
    SV_ECOG_STATUS %in% c(0, 1) &
    is.na(SV_DIAG_EXCL) &
    (SV_RT_EXCL == FALSE & SV_SX_EXCL == FALSE), data.table::uniqueN(person_id)]

  n_cohort <- dt[eligible_exposed == TRUE | eligible_control == TRUE, data.table::uniqueN(person_id)]
  n_exposed <- dt[eligible_exposed == TRUE, data.table::uniqueN(person_id)] # Final exposed total after full eligibility logic.
  n_control <- dt[eligible_control == TRUE, data.table::uniqueN(person_id)] # Final unexposed total after full eligibility logic.

  # Build shell-aligned row labels and values in final reporting order.
  table_1a <- data.table::data.table(
    Label = c(
      "All individuals diagnosed with mCRC (stage IV) between 01 January 2015 - 30 June 2015 or
      01 January 2021 - 30 June 2021, and registered in the NCR",
      "    AND started CapOx-B (capecitabine + oxaliplatin + bevacizumab) or CapOx (capecitabine + oxaliplatin)",
      "    AND started index or reference drugs within 180 days from mCRC diagnosis",
      "    AND are aged 18 >= at index",
      "    AND had an Eastern Cooperative Oncology Group (ECOG) Performance Status (PS) of 0 or 1",
      "    AND had no previous cancer diagnosis that could potentially have been treated with oxaliplatin and/or bevacizumab ",
      "    AND if having undergone radiotherapy or surgery for mCRC, had this completed ≥ 4 weeks before T0",
      "Total Eligible Cohort",
      "      Eligible CapOx-B",
      "      Eligible CapOx"
    ),
    N = c(
      .format_count(n),
      .format_count(n_1),
      .format_count(n_2a),
      .format_count(n_2b),
      .format_count(n_2c),
      .format_count(n_2d),
      .format_count(n_2e),
      .format_count(n_cohort),
      .format_count(n_exposed),
      .format_count(n_control)
    ),
    Percentage = c(
      NA_character_, # Row 1 percentage intentionally blank.
      "100",
      .pct(n_2a, n_1, digits),
      .pct(n_2b, n_1, digits),
      .pct(n_2c, n_1, digits),
      .pct(n_2d, n_1, digits),
      .pct(n_2e, n_1, digits),
      .pct(n_cohort, n_1, digits),
      .pct(n_exposed, n_cohort, digits),
      .pct(n_control, n_cohort, digits)
    ),
    Aux = NA_character_
  )

  table_1a <- .finalize_attrition_table(
  table_1a,
  return_na = return_na
)

if (debug) {
  return(list(
    table = table_1a,
    dropped_n2c_n2d_ids = dropped_n2c_n2d_ids,
    dropped_n2c_n2d = dropped_n2c_n2d
  ))
}

table_1a

}
