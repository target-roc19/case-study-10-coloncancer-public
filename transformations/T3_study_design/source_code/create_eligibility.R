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
  lm$start_script("calculate_eligibility")
  lm$start_capturing_prints()
}

## Config loading ----
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_eligibility <- config_project$create_eligibility
dir_multi <- config_project$create_eligibility$d3_multivariate_episodes
dir_out <- config_project$create_eligibility$output
dir_functions <- config_project$create_eligibility$functions$dir_common

logger::log_info("Starting: Calculating eligibility")

# ----------------------------
# LOAD DATA
# ----------------------------
possible_batching_dir <- "data/D3_study_variables/D3_MULTIVARIATE_EPISODES"
if(dir.exists(possible_batching_dir)){
  d3_multivariate_episodes <- data.table::as.data.table(arrow::open_dataset(
    possible_batching_dir,
  ))
}else{
  d3_multivariate_episodes <- data.table::as.data.table(picard::load(
    file_path = dir_multi,
    file_name = "D3_MULTIVARIATE_EPISODES.parquet"
  ))
}

logger::log_info(paste0("Input rows of multivariate episodes:", nrow(d3_multivariate_episodes)))

# source functions
source(file.path(dir_functions, "calculate_eligibility_function.R"))

if(config_values$dummy_data_used){

  logger::log_info("adding duplicates to test eligibility")
  # Select 10 random patients
  set.seed(123) # for reproducibility
  dup_ids <- sample(unique(d3_multivariate_episodes$person_id), 10)

  # Create duplicated rows
  dup_rows_multivariate <- copy(
    d3_multivariate_episodes[person_id %in% dup_ids]
  )

  # Move start_episode by 1 day
  dup_rows_multivariate[, start_episode := start_episode + 1]

  # Append duplicates
  d3_multivariate_episodes <- rbindlist(
    list(d3_multivariate_episodes, dup_rows_multivariate),
    use.names = TRUE
  )
  
}
# ----------------------------
# ELIGIBILITY
# ----------------------------
d3_eligibility <- calculate_eligibility(d3_multivariate_episodes)

# If a participant is assigned to more than one treatment group, only the group with the earliest treatment episode is kept. This is done to avoid contamination of the treatment groups.

# Identify participants eligible for more than one arm (it should be only one)
multi_arm_ids <- d3_eligibility[
  eligible_exposed | eligible_control,
  .(
    n_arms = as.integer(any(eligible_exposed)) +
             as.integer(any(eligible_control))
  ),
  by = person_id
][n_arms > 1, person_id]

if (length(multi_arm_ids) > 0) { # if there are such patients, log the number of them
  logger::log_info(paste0("Participants eligible for more than one treatment group: ",  length(unique(multi_arm_ids))))

  # For those participants, find the earliest eligible T0
  keep_rows <- d3_eligibility[
    person_id %in% multi_arm_ids &
      (eligible_exposed | eligible_control),
    {
      earliest_t0 <- min(start_episode)

      earliest_rows <- .SD[start_episode == earliest_t0]

      # If both arms occur on the same earliest T0,
      # prioritize exposed
      if (any(earliest_rows$eligible_exposed)) {
        .I[which(earliest_rows$eligible_exposed)[1]]
      } else {
        .I[1]
      }
    },
    by = person_id
  ]$V1

  # Remove later eligible arm assignments for these participants
  d3_eligibility[
    person_id %in% multi_arm_ids &
    (eligible_exposed | eligible_control) &
    !seq_len(.N) %in% keep_rows,
    `:=`(
      eligible_exposed = FALSE,
      eligible_control = FALSE
    )
  ]

} else {
  logger::log_info("No participants eligible for more than one treatment group, eligibility is unaffected.")
}

# Patients with multiple eligible rows
dup_ids <- d3_eligibility[
  eligible_exposed | eligible_control,
  .N,
  by = person_id
][N > 1, person_id]

if (length(dup_ids) > 0) {

  # Log duplicated eligible rows
  dup_rows <- d3_eligibility[
    person_id %in% dup_ids &
    (eligible_exposed | eligible_control),
    .(
      person_id,
      start_episode,
      eligible_exposed,
      eligible_control
    )
  ][order(person_id, start_episode)]

  logger::log_info(
    paste0(
      "Found ",
      length(dup_ids),
      " participants with multiple eligible exposure/control episodes."
    )
  )

  logger::log_info(
    paste(
      dup_rows[
        ,
        paste0(
          "person_id=", person_id,
          ", start_episode=", start_episode,
          ", eligible_exposed=", eligible_exposed,
          ", eligible_control=", eligible_control
        )
      ],
      collapse = "\n"
    )
  )

  # Keep the earliest eligible row for each duplicated patient
  keep_rows <- d3_eligibility[
    person_id %in% dup_ids &
    (eligible_exposed | eligible_control)
  ][
    order(start_episode),
    .SD[1],
    by = person_id
  ]

  # Remove all rows for duplicated patients and add back the row to keep
  d3_eligibility <- rbindlist(
    list(
      d3_eligibility[!person_id %in% dup_ids],
      keep_rows
    ),
    use.names = TRUE
  )

  logger::log_info(
    paste0(
      "Retained earliest eligible episode for ",
      length(dup_ids),
      " participants."
    )
  )

} else {

  logger::log_info(
    "No participants with multiple eligible exposure/control episodes found."
  )

}

# Verification
remaining_dups <- d3_eligibility[
  eligible_exposed | eligible_control,
  .N,
  by = person_id
][N > 1]

logger::log_info(
  paste0(
    "Participants with multiple eligible exposure/control episodes after deduplication: ",
    nrow(remaining_dups)
  )
)

# CHECKS
logger::log_info("Finishing: Calculating eligibility")
logger::log_info(paste0("Eligible exposed ", nrow(d3_eligibility[d3_eligibility$eligible_exposed == TRUE, ])))
logger::log_info(paste0("Eligible exposed unique subjects ", length(unique(d3_eligibility$person_id[d3_eligibility$eligible_exposed == TRUE]))))
logger::log_info(paste0("Eligible control ", nrow(d3_eligibility[d3_eligibility$eligible_control == TRUE, ])))
logger::log_info(paste0("Eligible control unique subjects ", length(unique(d3_eligibility$person_id[d3_eligibility$eligible_control == TRUE]))))
logger::log_info(paste0("Output rows of eligibility: ", nrow(d3_eligibility)))

# print TRUE if there are any ids who received both exposure and control, FALSE otherwise
logger::log_info(paste0("Are there ids who received both exposure and control: ", d3_eligibility[, .(
  exposed_any = any(eligible_exposed == TRUE),
  control_any = any(eligible_control == TRUE)
), by = person_id][
  , any(exposed_any & control_any)
]))

# # print the person ids of patients who fullfill eligibility for both exposed and control
# overlap_ids <- d3_eligibility[, .(
#   exposed_any = any(eligible_exposed == TRUE),
#   control_any = any(eligible_control == TRUE)
# ), by = person_id][
#   exposed_any & control_any,
#   person_id
# ]

# logger::log_info(paste0("Person ids of patients who fullfill eligibility for both exposed and control: ", paste(overlap_ids, collapse = ", ")))

# # print the person ids of patients who were exposed to all 3 drugs across their history, but never received all 3 on the same episode, and those who received CapOx (control) on the same episode 
# dt_person <- d3_eligibility[, .(
#   drug1_any = any(SV_BEVACIZUMAB == TRUE, na.rm = TRUE),
#   drug2_any = any(SV_OXALIPLATIN == TRUE, na.rm = TRUE),
#   drug3_any = any(SV_CAPECITABINE == TRUE, na.rm = TRUE),

#   exposure_combo_any = any(receives_exposure == TRUE, na.rm = TRUE),
#   control_combo_any  = any(receives_control == TRUE, na.rm = TRUE)
# ), by = person_id]
# ids_all_three_exposure <- dt_person[
#   drug1_any & drug2_any & drug3_any & !exposure_combo_any,
#   person_id
# ]

# logger::log_info(paste0("Person ids who were exposed to all 3 drugs across their history,
# but never received all 3 on the same episode: ", paste(ids_all_three_exposure, collapse = ", ")))

# ids_all_three_control <- dt_person[
#   drug1_any & drug2_any & drug3_any & control_combo_any,
#   person_id
# ]

# logger::log_info(paste0("Person ids who were exposed to all 3 drugs across their history,
# but received CapOx (control) on the same episode: ", paste(ids_all_three_control, collapse = ", ")))

# ----------------------------
# SAVE OUTPUT
# ----------------------------
picard::save(
  data = d3_eligibility,
  file_path = dir_out,
  file_name = "D3_ELIGIBILITY.parquet"
)

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d3_eligibility, d3_multivariate_episodes)
