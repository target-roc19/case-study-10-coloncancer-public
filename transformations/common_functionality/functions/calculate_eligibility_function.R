calculate_eligibility <- function(d3_multivariate_episodes) {
  # Ensure we operate on a data.table for fast in-place updates
  dt <- data.table::as.data.table(d3_multivariate_episodes)

  # To logically evaluate eligibility, we need to ensure all relevant columns are in the correct format.
  # The following columns should be logical (TRUE/FALSE) for the eligibility criteria:
  logical_cols <- c(
    "SV_FIRST_CRC_DIAGNOSIS", "SV_CRC_DIAGNOSIS", "SV_STAGE4",
    "SV_DIAGNOSED_IN_2015", "SV_DIAGNOSED_IN_2021",
    "SV_AGE_18_OR_OLDER", "SV_RT_EXCL", "SV_SX_EXCL",
    "SV_CAPECITABINE", "SV_OXALIPLATIN", "SV_BEVACIZUMAB"
  )
  present_logical_cols <- intersect(logical_cols, names(dt))
  for (col in present_logical_cols) {
    # Normalize common textual missing encodings before coercion
    if (is.character(dt[[col]])) {
      tmp <- trimws(toupper(dt[[col]]))
      tmp[tmp %in% c("", "NULL", "NA")] <- NA
      dt[[col]] <- tmp
    }
    dt[[col]] <- as.logical(dt[[col]])
  }

  # Ensure ECOG status is numeric/integer for the `%in% c(0,1)` check
  if ("SV_ECOG_STATUS" %in% names(dt)) {
    # Normalize textual nulls then coerce to integer
    if (is.character(dt$SV_ECOG_STATUS)) {
      tmp <- trimws(toupper(dt$SV_ECOG_STATUS))
      tmp[tmp %in% c("", "NULL", "NA")] <- NA
      dt[, SV_ECOG_STATUS := tmp]
    }
    dt[, SV_ECOG_STATUS := as.integer(SV_ECOG_STATUS)]
  }

  # Normalize textual "NULL" placeholders in exclusion field so is.na() works
  if ("SV_DIAG_EXCL" %in% names(dt) && is.character(dt$SV_DIAG_EXCL)) {
    tmp <- trimws(toupper(dt$SV_DIAG_EXCL))
    tmp[tmp %in% c("", "NULL", "NA")] <- NA
    dt[, SV_DIAG_EXCL := tmp]
  }

  dt[, in_eligible_pop :=
    SV_FIRST_CRC_DIAGNOSIS == TRUE &
      SV_STAGE4 == TRUE &
      (SV_DIAGNOSED_IN_2015 == TRUE | SV_DIAGNOSED_IN_2021 == TRUE) &
      SV_AGE_18_OR_OLDER == TRUE &
      SV_ECOG_STATUS %in% c(0, 1) &
      is.na(SV_DIAG_EXCL) &
      SV_RT_EXCL == FALSE &
      SV_SX_EXCL == FALSE
  ]

  dt[, receives_exposure :=
    SV_OXALIPLATIN == TRUE &
      SV_BEVACIZUMAB == TRUE &
      SV_CAPECITABINE == TRUE]
  dt[, receives_control :=
    SV_OXALIPLATIN == TRUE &
      SV_CAPECITABINE == TRUE &
      SV_BEVACIZUMAB == FALSE]

  dt[, eligible_exposed := in_eligible_pop & receives_exposure]
  dt[, eligible_control := in_eligible_pop & receives_control]

  return(dt)
}
