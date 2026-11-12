#' Assemble the analysis data for outcome analyses
#'
#' @param d4_fup D4_StudyCohort_FUP  -- person_id, T0, group,
#'  "reason_censoring_1_3", "reason_censoring_2",   
#' "fup_length_days_1_3", "fup_length_days_2", "eventCount_1_3",  "eventCount_2"  
#' @param d4_iptw D4_StudyCohort_IPTW -- person_id, T0, p_score,
#'   include_after_trimming, trunc_iptw.#'
#' @return data.table with one row per person: person_id, group, fup_days, pyr,
#'   event_death, reason_censoring, p_score, include_after_trimming, ip_weight,
create_d4_studycohort_analysis <- function(d4_fup, d4_iptw, d4_cov) {
  fup <- data.table::as.data.table(d4_fup)
  iptw <- data.table::as.data.table(d4_iptw)
  cov <- data.table::as.data.table(d4_cov)

  # Align person_id and T0 types
  for (dt in list(fup, iptw, cov)) {
    dt[, `:=`(person_id = as.character(person_id), T0 = as.Date(T0))]
  }

  # Left join on the followed-up cohort: every FUP person, weights attached
  # where an IPTW row exists. Trimmed persons are kept (flagged by
  # include_after_trimming) and filtered per estimate downstream.
  lookup <- data.table::merge.data.table(
    fup[, .(
      person_id,
      T0,
      group,
      reason_censoring_1_3,
      reason_censoring_2,
      fup_length_days_1_3,
      fup_length_days_2,
      eventCount_1_3,
      eventCount_2
    )],
    iptw[, .(
      person_id,
      p_score,
      include_after_trimming,
      ip_weight = trunc_iptw
    )],
    by = "person_id",
    all.x = TRUE,
    all.y = FALSE
  )

# in cov, selet the matched patients (group == "exposed" or group == "control" and not NA) 
  cov <- cov[!is.na(group) & group %in% c("exposed", "control"), .(
    person_id,
    SV_AGE,
    SV_SEX,
    SV_ECOG_STATUS,
    SV_DIAGNOSED_IN_2015,
    SV_DIAGNOSED_IN_2021,
    SV_SITE_SIDEDNESS,
    SV_METASTASIS_LIVER,
    SV_METASTASIS_LUNG,
    SV_METASTASIS_PERITONEUM,
    SV_METASTASIS_OTHER,
    SV_METASTASIS_LYMPH,
    SV_METASTASIS_MULTIPLE,
    SV_MUTATION_BRAF,
    SV_MUTATION_RAS
  )]

# Then merge on lookup the covariates for the matched patients (selected above)
  lookup <- data.table::merge.data.table(
    lookup,
    cov,
    by = "person_id",
    all.x = TRUE,
    all.y = FALSE
  )

  data.table::setkey(lookup, person_id)
  lookup[]

}
