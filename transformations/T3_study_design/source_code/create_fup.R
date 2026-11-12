# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_fup")
  lm$start_capturing_prints()
}

## Config loading
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
config_pipeline <- picard::load_config("configuration/config_pipeline.yaml")
create_fup <- config_project$create_anchored_datasets
create_anchored_datasets <- config_project$create_anchored_datasets

# Output path for anchored variables in the Hive format
anchor_hive_dir <- create_anchored_datasets$output$anchoring_dir

# ----------------------------
# LOAD METADATA
# ----------------------------
# load anchoring window metadata which defines the variables to be anchored,
# their data type, and the time windows for anchoring
anchoring_window_metadata <- picard::load(
  file_path = create_anchored_datasets$input$metadata_dir,
  file_name = "anchoring_metadata.csv"
)

# ----------------------------
# Extract POST-EXPOSURE VARIABLES
# ----------------------------
fup <- anchoR::get_anchor_result(
  metadata = anchoring_window_metadata[fup == TRUE],
  anchor_hive_path = anchor_hive_dir,
  impute_missing = FALSE,
  only_date = TRUE,
  result_shape = "wide"
)

logger::log_info("Finalised: extracting post-exposure variables")


# Merge additional variables we need for the D4_StudyCohort_FUP dataset

# 1. End of study period (end_study_date)
logger::log_info(paste0(
  "Using end_study_date from config_values.yaml: ",
  config_values$end_study_date
))
end_study_date <- as.Date(config_values$end_study_date)
fup[, end_study_date := end_study_date]


# 1. End of data collection - pending for now


# 2. End observation in data (end_observation_data_d): T0 + 104 weeks
fup <- fup[, end_observation_data_d := T0 + 104 * 7]

# 3. group (D4_StudyCohort)
logger::log_info("Merging in group from D4_StudyCohort")
d4_study_cohort <- data.table::as.data.table(picard::load(
  file_path = create_anchored_datasets$input$cohort_dir,
  file_name = "D4_StudyCohort.Rds"
))
# d4_study_cohort <- d4_study_cohort[group != "unmatched"]
fup <- data.table::merge.data.table(
  x = fup,
  y = d4_study_cohort[, .(person_id, group, eligible_exposed, eligible_control)],
  by = "person_id",
  all.x = TRUE
)

# create a new 'group' variable where we use the eligible_exposed and eligible_control variables (this is needed for table 3)
fup[, group_all := data.table::fifelse(
  eligible_exposed,
  "exposed",
  data.table::fifelse(eligible_control, "control", NA_character_)
)]

# 4. end spell (end_spell from D3_SPELLS)
logger::log_info("Merging in loss_to_follow_up from D3_SPELLS$end_spell")
spells <- picard::load(
  file_path = config_project$outputs$dir_d3,
  file_name = "D3_SPELLS.parquet"
)[, .(person_id, end_spell)]
fup <- data.table::merge.data.table(
  x = fup,
  y = spells,
  by = "person_id",
  all.x = TRUE
)
data.table::setnames(fup, "end_spell", "loss_to_follow_up")

# 5. boot_id (which we don't have now, but when we have we will merge it from d4 study cohort)
# logger::log_info("Merging in boot_id from D4_StudyCohort")
# fup <- data.table::merge.data.table(
#  x = fup,
#  y = d4_study_cohort[, .(person_id, boot_id)],
#  by = "person_id",
#  all.x = TRUE
# )

# 6. death_date (D3_PERSONS)
logger::log_info("Merging in death_d from D3_PERSONS$death_date")
persons <- picard::load(
  file_path = config_project$outputs$dir_d3,
  file_name = "D3_PERSONS.parquet"
)[, .(person_id, death_date)]
fup <- data.table::merge.data.table(
  x = fup,
  y = persons,
  by = "person_id",
  all.x = TRUE
)
data.table::setnames(fup, "death_date", "death_d")


# 7. Intercurrent events columns
# Add 21 days to all end dates of the drugs
# For CAP we do this because the stop date is defined as the termination of the final cycle. If this information is lacking, which is the case #for capecitabine, the start date of the final cycle is registered as the stop date. 
fup[, CAPE_DISC := CAPE_DISC + 21]
# for OXA and BEV this corresponds to cycle duration.
fup[, OXA_DISC := OXA_DISC + 21]
fup[, BEV_DISC := BEV_DISC + 21]

# First create columns where the disc dates - T0 are in weeks
fup <- fup[, `:=`(
  disc_cap_weeks = as.numeric(CAPE_DISC - T0) / 7,
  disc_oxa_weeks = as.numeric(OXA_DISC - T0) / 7,
  disc_bev_weeks = as.numeric(BEV_DISC - T0) / 7
)]

# check if there are people with start and end date of drug on same day
n_disc_cap_same_day <- sum(fup$disc_cap_weeks == 0, na.rm = TRUE)
n_disc_oxa_same_day <- sum(fup$disc_oxa_weeks == 0, na.rm = TRUE)
n_disc_bev_same_day <- sum(fup$disc_bev_weeks == 0, na.rm = TRUE)

logger::log_info(paste0("Number of people who start and stop cap on t0: ", n_disc_cap_same_day))
logger::log_info(paste0("Number of people who start and stop oxa on t0: ", n_disc_oxa_same_day))
logger::log_info(paste0("Number of people who start and stop bev on t0: ", n_disc_bev_same_day))

# CAPOX group:

# 7.1: full premature discontinuation of CAPOX: (meaning both end dates are on the same date) --> can only happen between T0 and 18 weeks (126 #days) for both CAPOX and CAPOXB groups. This also takes into account the case that BEV_DISC is on the same date as CAPE_DISC and OXA_DISC. So we don't have to double count that case in the CAPOX-B group.
fup[, full_disc_capox_d := data.table::fifelse(
  !is.na(CAPE_DISC) & !is.na(OXA_DISC) &
    (CAPE_DISC == OXA_DISC) &
    (disc_cap_weeks >= 0 & disc_cap_weeks <= 18) &
    (disc_oxa_weeks >= 0 & disc_oxa_weeks <= 18),
  CAPE_DISC, # does not matter which one we take as they are all the same date
  as.Date(NA)
)]

# 7.2: partial premature discontinuation of CAP (partial_disc_cap_d): Date of the premature discontinuation of capecitabine between T0 and 48 weeks (336 days).
# for both groups to avoid repeat code, we will use the same code for both groups
fup[, partial_disc_cap_d := data.table::fifelse(
  !is.na(CAPE_DISC) & (disc_cap_weeks >= 0 & disc_cap_weeks <= 48),
  CAPE_DISC,
  as.Date(NA)
)]

# 7.3: partial premature discontinuation of OX (partial_disc_oxa_d): Date of the premature discontinuation of oxaliplatin between T0 and 18 weeks (126 days).
fup[, partial_disc_oxa_d := data.table::fifelse(
  !is.na(OXA_DISC) & (disc_oxa_weeks >= 0 & disc_oxa_weeks <= 18),
  OXA_DISC,
  as.Date(NA)
)]

# take the earliest of the two partial discontinuation dates (partial_disc_cap_d and partial_disc_oxa_d) given that they are not on the same date and use that for the FUP/IE calculations.
# for both groups to avoid repeat code, we will use the same code for both groups
fup[, capox_disc_earliest_d :=
  data.table::fcase(
    !is.na(partial_disc_cap_d) &
      !is.na(partial_disc_oxa_d) &
      partial_disc_cap_d == partial_disc_oxa_d,
    as.Date(NA),
    !is.na(partial_disc_cap_d) & !is.na(partial_disc_oxa_d),
    pmin(partial_disc_cap_d, partial_disc_oxa_d),
    !is.na(partial_disc_cap_d),
    partial_disc_cap_d,
    !is.na(partial_disc_oxa_d),
    partial_disc_oxa_d,
    default = as.Date(NA)
  )]

# combine full discontinuation and partial discontinuations, because only one of
# these can occur in the same individual and this is one of the distinct IEs
fup[, ie_disc_capox_d := data.table::fcoalesce(
  full_disc_capox_d,
  capox_disc_earliest_d
)]

fup[, ie_disc_capox := data.table::fcase(
  !is.na(full_disc_capox_d), "full",
  !is.na(capox_disc_earliest_d), "partial",
  default = NA_character_
)]

# 7.4: addition of B (treatment switch) 
fup[group_all == "control", treat_switch_d := data.table::fifelse(
  !is.na(SV_BEVACIZUMAB) & (SV_BEVACIZUMAB  > T0),
  SV_BEVACIZUMAB,
  as.Date(NA)
)]

# # 7.5: starting chemotherapy backbone (treatment switch) (for both capox and capoxv to avoid repeat code, we will use the same code for both groups)
chemo_drugs <- c("SV_CETUXIMAB", "SV_FLUOROUACIL", "SV_FOLINICACID", "SV_GIMERACIL", "SV_NIVOLUMAB", "SV_OTERACIL", "SV_PANITUMUMAB", "SV_PEMBROLIZUMAB", "SV_TEGAFUR", "SV_TIPIRACIL", "SV_TRIFLURIDINE", "SV_IRINOTECAN")
fup[, treat_switch_chemo_d := data.table::fifelse(
  rowSums(!is.na(.SD)) > 0,
  do.call(pmin, c(.SD, na.rm = TRUE)),
  as.Date(NA)
), .SDcols = chemo_drugs]

fup[, ie_switch_d := data.table::fcase(
  !is.na(treat_switch_d) & !is.na(treat_switch_chemo_d),
  pmin(treat_switch_d, treat_switch_chemo_d),
  !is.na(treat_switch_d),
  treat_switch_d,
  !is.na(treat_switch_chemo_d),
  treat_switch_chemo_d,
  default = as.Date(NA)
)]

fup[, ie_switch := data.table::fcase(
  !is.na(treat_switch_d) &
    (is.na(treat_switch_chemo_d) | treat_switch_d <= treat_switch_chemo_d),
  "b",
  !is.na(treat_switch_chemo_d),
  "chemo",
  default = NA_character_
)]

# CAPOX B group:

# 7.2: Discontinuation of B --> stop before 48 weeks (bev_disc_16_cycles_d)
fup[group_all == "exposed", bev_disc_16_cycles_d := data.table::fifelse(
  !is.na(BEV_DISC) &
    (disc_bev_weeks >= 0 & disc_bev_weeks <= 48),
  BEV_DISC,
  as.Date(NA)
)]

# update 3/9/2026: we will be counting any IEs before end of follow-up, not
# applying a hierarchy
# this is why we're counting ie_disc_b_d differently then originally planned
# I still
# only want to count bev disc if they're not bev AND cap/ox or full (capoxb dis)
# which means another drug is discontinued on the same date.

fup[, ie_disc_b_d := data.table::fifelse(
  !is.na(bev_disc_16_cycles_d) &
    (is.na(ie_disc_capox_d) | bev_disc_16_cycles_d != ie_disc_capox_d),
  bev_disc_16_cycles_d,
  as.Date(NA)
)]

# after discussion with Julia, Oisin and Daniala, we figured there are 3
# different types of partial discontinuation, depending on in which group you
# are, these will be reported as separate categories in Table 4
# in capox group this is discontinuation of cap OR ox
# in capoxb group this is:
# - discontinuation of b and cap OR b and ox on same day
# - discontinuation of cap OR ox
# - discontinuation of cap AND ox on same day
fup[, ie_partial_disc := data.table::fcase(
  # 2. Bevacizumab discontinued on same date as a partial CAPOX discontinuation
  # (only in capoxb group)
  ie_disc_capox == "partial" &
    !is.na(bev_disc_16_cycles_d) &
    bev_disc_16_cycles_d == ie_disc_capox_d,
  "b_and_cap_or_ox",
  
  # 1. Partial CAPOX discontinuation only
  # (happens in capox and capoxb group)
  ie_disc_capox == "partial",
  "cap_or_ox",
  
  # 3. Full CAPOX discontinuation in exposed group
  # (happens in capoxb group), in capox group this is categorised as a full disc
  group_all == "exposed" &
    ie_disc_capox == "full",
  "ox_and_cap",
  
  default = NA_character_
)]

fup[, ie_partial_disc_d := data.table::fifelse(
  ie_partial_disc %in% c("cap_or_ox", "b_and_cap_or_ox", "ox_and_cap"),
  ie_disc_capox_d,
  as.Date(NA)
)]

# adding date of partial discontinuations as an extra column to be used for
# for estimand 2
fup[, ie_partial_disc_est2_d := data.table::fifelse(
  ie_partial_disc %in% c("cap_or_ox", "ox_and_cap"),
  ie_partial_disc_d,
  as.Date(NA)
)]

# full discontinuations are discontinations of:
# - cap AND ox on same day in capox group
# - cap AND ox AND b on same day in capox b group
# please note we still don't count any b discontinuations if they occur after partial/
# full discontinuations of capox
fup[, ie_full_disc_d := data.table::fcase(
  # Full discontinuation in control group
  group_all == "control" &
    ie_disc_capox == "full",
  ie_disc_capox_d,
  
  # Full discontinuation in exposed group when bev is discontinued on the same date
  group_all == "exposed" &
    ie_disc_capox == "full" &
    !is.na(bev_disc_16_cycles_d) &
    bev_disc_16_cycles_d == ie_disc_capox_d,
  bev_disc_16_cycles_d,
  
  default = as.Date(NA)
)]

# adding date of full disc of capox in capox group (controls) as an extra column
# to be used for estimand 2 censoring date/reason
fup[, ie_full_disc_est2_d := data.table::fcase(
  # Full discontinuation in control group
  group_all == "control" &
    ie_disc_capox == "full",
  ie_disc_capox_d
)]

# check if there are people with start and end date of drug on same day
# 3/9/2026: this should not happen anymore because weve added 21 days to all
# drug end dates after discussion w Julia/IKNL/oncologist
n_disc_capox_t0_control <-
  sum(fup$disc_cap_weeks == 0 & fup$disc_oxa_weeks == 0 &
        fup$group_all == "control", na.rm = TRUE)
n_disc_capoxb_t0_exposed <-
  sum(fup$disc_bev_weeks == 0 &
        fup$group_all == "exposed" & !is.na(fup$ie_full_disc_d), na.rm = TRUE)

logger::log_info(paste0("Number of people in control group who start and stop capox on t0: ", n_disc_capox_t0_control))
logger::log_info(paste0("Number of people in exposed group who start and stop capoxb on t0: ", n_disc_capoxb_t0_exposed))

fup[, ie_local_treatment_d := data.table::fifelse(
  !is.na(SV_PROCEDURE),
  SV_PROCEDURE,
  as.Date(NA)
)]

# Before moving on to calculate the FUP, we first select the columns we will need:
fup <- fup[, .(
  person_id,
  T0,
  group,
  group_all,
  end_study_date,
  SV_DISEASE_PROGRESSION, # outcome variable
  SV_PROCEDURE, # surgery, ablation, radiotherapy, etc. (intercurrent event)
  death_d, # outcome variable
  end_observation_data_d,
  loss_to_follow_up,
  full_disc_capox_d, # full treatment discontinuation (intercurrent event)
  capox_disc_earliest_d, # partial treatment discontinuation (intercurrent event)
  treat_switch_d, # treatment switch to B (intercurrent event)
  treat_switch_chemo_d, # treatment switch to chemotherapy (intercurrent event)
  bev_disc_16_cycles_d, # treatment discontinuation of B - follow up continues regardless in all estimands
  ie_switch_d,
  ie_switch,
  ie_disc_capox_d,
  ie_disc_capox,
  ie_disc_b_d,
  ie_local_treatment_d,
  ie_partial_disc_d,
  ie_partial_disc,
  ie_partial_disc_est2_d,
  ie_full_disc_d,
  ie_full_disc_est2_d
)]


# 7. Administrative end of FUP: end_observation_data_d, loss_to_follow_up, end_study_date (if available), end_of_data_collection (if available)
logger::log_info("Computing administrative end of follow-up: end_observation_data_d, loss_to_follow_up, end_study_date (if available), end_of_data_collection (if available)")
fup[, general_end_fup := as.Date(
  do.call(pmin, c(.SD, na.rm = TRUE))
), .SDcols = c(
  "end_observation_data_d",
  "loss_to_follow_up",
  # "end_of_data_collection",
  "end_study_date"
)]

# 8. general_end_fup_1_3 (calculate the earliest of disease progression, death_d, end_observation_data_d, end_spell, end_of_data_collection, end_study_date)
logger::log_info("Computing the earliest of disease progression, death_d, end_observation_data_d, end_spell, end_of_data_collection, end_study_date for general_end_fup_1_3")
fup[, general_end_fup_1_3 := as.Date(
  do.call(pmin, c(.SD, na.rm = TRUE))
), .SDcols = c(
  "SV_DISEASE_PROGRESSION",
  "death_d",
  "end_observation_data_d",
  "loss_to_follow_up",
  # "end_of_data_collection",
  "end_study_date"
)]


# 9. general_end_fup_2
fup[, general_end_fup_2 := as.Date(
  do.call(pmin, c(.SD, na.rm = TRUE))
), .SDcols = c(
  "SV_DISEASE_PROGRESSION",
  "death_d",
  "end_observation_data_d",
  "loss_to_follow_up",
  # "end_of_data_collection",
  "end_study_date",
  "ie_full_disc_est2_d",
  "ie_partial_disc_est2_d",
  "treat_switch_d",
  "treat_switch_chemo_d",
  "SV_PROCEDURE"
)]


# 10. reason_censoring (identify which column contains the minimum date)
logger::log_info("Computing reason_censoring (which date determined the end of follow-up)")
fup[, reason_censoring := data.table::fcase(
  (!is.na(end_observation_data_d) & end_observation_data_d == general_end_fup), "end_observation_data_d",
  (!is.na(loss_to_follow_up) & loss_to_follow_up == general_end_fup), "loss_to_follow_up",
  # (!is.na(end_of_data_collection) & end_of_data_collection == general_end_fup), "end_of_data_collection",
  (!is.na(end_study_date) & end_study_date == general_end_fup), "end_study_date",
  default = NA_character_
)]

# 11. reason_censoring_1_3 (identify which column contains the minimum date)
logger::log_info("Computing reason_censoring_1_3 (which date determined the end of follow-up)")
fup[, reason_censoring_1_3 := data.table::fcase(
  (!is.na(SV_DISEASE_PROGRESSION) & SV_DISEASE_PROGRESSION == general_end_fup_1_3), "SV_DISEASE_PROGRESSION",
  (!is.na(death_d) & death_d == general_end_fup_1_3), "death_d",
  (!is.na(end_observation_data_d) & end_observation_data_d == general_end_fup_1_3), "end_observation_data_d",
  (!is.na(loss_to_follow_up) & loss_to_follow_up == general_end_fup_1_3), "loss_to_follow_up",
  (!is.na(end_study_date) & end_study_date == general_end_fup_1_3), "end_study_date",
  default = NA_character_
)]

# 12. reason_censoring_2
logger::log_info("Computing reason_censoring_2 (which date determined the end of follow-up)")
fup[, reason_censoring_2 := data.table::fcase(
  (!is.na(SV_DISEASE_PROGRESSION) & SV_DISEASE_PROGRESSION == general_end_fup_2), "SV_DISEASE_PROGRESSION",
  (!is.na(death_d) & death_d == general_end_fup_2), "death_d",
  (!is.na(end_observation_data_d) & end_observation_data_d == general_end_fup_2), "end_observation_data_d",
  (!is.na(loss_to_follow_up) & loss_to_follow_up == general_end_fup_2), "loss_to_follow_up",
  (!is.na(end_study_date) & end_study_date == general_end_fup_2), "end_study_date",
  (!is.na(ie_full_disc_est2_d) & ie_full_disc_est2_d == general_end_fup_2), "full_disc_capox_d", # old naming kept for compatibility: full discontinations are censored in capox group only, in capoxb group handled with treatment strat cos b is discontinued also
  (!is.na(ie_partial_disc_est2_d) & ie_partial_disc_est2_d == general_end_fup_2), "capox_disc_earliest_d", # old naming kept for compatibility: partial discontinuations are censored in capox + capox-b group if cap or ox are discontinued OR if in capox-b group cap and ox are discontinued
  (!is.na(treat_switch_d) & treat_switch_d == general_end_fup_2), "treat_switch_d",
  (!is.na(treat_switch_chemo_d) & treat_switch_chemo_d == general_end_fup_2), "treat_switch_chemo_d",
  (!is.na(SV_PROCEDURE) & SV_PROCEDURE == general_end_fup_2), "SV_PROCEDURE",
  default = NA_character_
)]


# 13. fup_length_days (calculate follow-up length in days from T0 to end of follow-up)
logger::log_info("Computing fup_length_days (days from T0 to end of follow-up)")
fup[, fup_length_days := as.integer(general_end_fup - T0)]

# 14. fup_length_days_1_3 (calculate follow-up length in days from T0 to end of follow-up)
logger::log_info("Computing fup_length_days_1_3 (days from T0 to end of follow-up)")
fup[, fup_length_days_1_3 := as.integer(general_end_fup_1_3 - T0)]

# 15. fup_length_days_2
logger::log_info("Computing fup_length_days_2 (days from T0 to end of follow-up)")
fup[, fup_length_days_2 := as.integer(general_end_fup_2 - T0)]

# if a person experienced local treatment first, we will set the follow-up length to 104 weeks (2 years) as per the protocol
fup[
  reason_censoring_2 == "SV_PROCEDURE",
  fup_length_days_2 := 104 * 7
]

fup[
  reason_censoring_2 == "SV_PROCEDURE",
  general_end_fup_2 := T0 + fup_length_days_2
]

# 16. eventCount_1_3 (1 if death or disease progression occurred, 0 otherwise)
logger::log_info("Computing eventCount_1_3 (event indicator: death or disease progression)")
fup[, eventCount_1_3 := as.integer(reason_censoring_1_3 %in% c( "death_d", "SV_DISEASE_PROGRESSION" ))]

# 17. eventCount_2
logger::log_info("Computing eventCount_2 (event indicator: death, disease progression)")
fup[, eventCount_2 := as.integer(
  reason_censoring_2 %in% c(
    "death_d",
    "SV_DISEASE_PROGRESSION"
  )
)]

### Checks for estimand 2
# Total number of people censored in Estimand 2 (--> premature discontinuation of ox/cap or treatment switch) AND
est2_cens <- fup[reason_censoring_2 %in% c("full_disc_capox_d", "capox_disc_earliest_d", "treat_switch_d", "treat_switch_chemo_d"), .N]
logger::log_info(paste0("Number of people censored in Estimand 2 (control/exposed/unmatched): ", est2_cens))

#the number of people who experience an Estimand 2 outcome after they are censored
est2_outcome_after_cens <- fup[reason_censoring_2 %in% c("full_disc_capox_d", "capox_disc_earliest_d", "treat_switch_d", "treat_switch_chemo_d") &
                                  #any data of death/diasease progression/procedure dates are after the censoring date
                                  ((!is.na(death_d) & death_d > general_end_fup_2) |
                                     (!is.na(SV_DISEASE_PROGRESSION) & SV_DISEASE_PROGRESSION > general_end_fup_2) |
                                     (!is.na(SV_PROCEDURE) & SV_PROCEDURE > general_end_fup_2)), 
                                  .N]
logger::log_info(paste0("Number of people who experience an Estimand 2 outcome after they are censored (control/exposed/unmatched): ", est2_outcome_after_cens))

# only for the matched
est2_cens_matched <- fup[reason_censoring_2 %in% c("full_disc_capox_d", "capox_disc_earliest_d", "treat_switch_d", "treat_switch_chemo_d") &
group %in% c("control", "exposed"), .N]
logger::log_info(paste0("Number of people censored in Estimand 2 (control/exposed): ", est2_cens_matched))

#the number of people who experience an Estimand 2 outcome after they are censored
est2_outcome_after_cens_matched <- fup[reason_censoring_2 %in% c("full_disc_capox_d", "capox_disc_earliest_d", "treat_switch_d", "treat_switch_chemo_d") &
                                  group %in% c("control", "exposed") &
                                  #any data of death/diasease progression/procedure dates are after the censoring date
                                  ((!is.na(death_d) & death_d > general_end_fup_2) |
                                     (!is.na(SV_DISEASE_PROGRESSION) & SV_DISEASE_PROGRESSION > general_end_fup_2) |
                                     (!is.na(SV_PROCEDURE) & SV_PROCEDURE > general_end_fup_2)), 
                                  .N]
logger::log_info(paste0("Number of people who experience an Estimand 2 outcome after they are censored (control/exposed): ", est2_outcome_after_cens_matched))



# Export one dataset for the matched + unmatched cohort
fup_all <- fup[, !("group")]
# rename group_all to group
data.table::setnames(fup_all, "group_all", "group")

# Export one dataset for the matched cohort only
fup <- fup[group != "unmatched", !("group_all")]

# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
    data = fup,
    file_path = create_anchored_datasets$output$dir,
    file_name = paste0(create_anchored_datasets$output$fup_file, ".parquet")
  )
picard::save(
    data = fup_all,
    file_path = create_anchored_datasets$output$dir,
    file_name = paste0(create_anchored_datasets$output$fup_file, "_before_matching.parquet")
  )

# ----------------------------
# CLEAN UP ENVIRONMENT
# ----------------------------
rm(anchoring_window_metadata, fup, fup_all,
   n_disc_capox_t0_control,
   n_disc_capoxb_t0_exposed,
   n_disc_cap_same_day,
   n_disc_oxa_same_day,
   n_disc_bev_same_day)
