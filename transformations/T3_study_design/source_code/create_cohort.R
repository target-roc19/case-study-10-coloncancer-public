# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
library(data.table)

if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_cohort.R")
  lm$start_capturing_prints()
}


logger::log_info("Starting: D4_StudyCohort")

# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_cohort <- config_project$create_cohort.R
dir_d3 <- create_cohort$input$dir_d3
dir_d4 <- create_cohort$output$dir_d4
save_output <- create_cohort$output$save_output
dir_functions <- create_cohort$functions$dir_common

# ----------------------------
# LOAD DATA
# ----------------------------
logger::log_info("Reading D3_ELIGIBILITY")

# file extension is different when we will make a release
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) { # if dummy data are used
  file_path_eligibility <- file.path(
    "tests", "testthat", "data", "D4_StudyCohort",
    paste0("D3_ELIGIBILITY", file_extension)
  )
  d3_eligibility <- picard::load(file_path_eligibility, sep = ";")
} else { # for real data
  file_path_eligibility <- file.path( 
    dir_d3,
    paste0("D3_ELIGIBILITY", file_extension)
  )
  d3_eligibility <- picard::load(file_path_eligibility)
}


# source functions 
source(file.path(dir_functions, "match_on_cov_profile.R"))
# ----------------------------
# STUDY COHORT
# ----------------------------
# check the size of d3_eligibility
n_total <- nrow(d3_eligibility)
logger::log_info(paste("D3_ELIGIBILITY total rows:", n_total))

if (n_total == 0) {
  stop("D3_ELIGIBILITY is empty. Stopping the execution")
}

# Number of exposed
num_exp <- nrow(d3_eligibility[eligible_exposed == TRUE, ])
# Number of exposed patients
num_exp_pts <- length(unique(d3_eligibility[eligible_exposed == TRUE, ]$person_id))
# Number of controls
num_con <-  nrow(d3_eligibility[eligible_control == TRUE, ])
# Number of control patients
num_con_pts <- length(unique(d3_eligibility[eligible_control == TRUE, ]$person_id))

logger::log_info(paste("Numer of exposed:", num_exp))
logger::log_info(paste("Number of controls", num_con))
logger::log_info(paste("Number of exposed patients:", num_exp_pts))
logger::log_info(paste("Number of control patients:", num_con_pts))

# Select eligible exposed and controls
d3_eligibility <- d3_eligibility[
  eligible_exposed == TRUE | eligible_control == TRUE
]

# Checks the number of the unique eligible exposed and controls
unique_exp <- length(unique(d3_eligibility[eligible_exposed == TRUE, ]$person_id))
unique_con <- length(unique(d3_eligibility[eligible_control == TRUE, ]$person_id))

logger::log_info(paste("Unique eligible exposed:", unique_exp))
logger::log_info(paste("Unique eligible controls:", unique_con))

# If dummy data are used, group per person_id and select the earliest start episode
if (config_values$dummy_data_used){
  d3_eligibility <- d3_eligibility[, .SD[which.min(start_episode)], by = person_id]
  logger::log_info("Grouped by person_id and selected the earliest start episode for dummy data")
} else {
  # For real data, we assume that the D3_eligibility is already at person level with the earliest start episode
  logger::log_info("Assuming D3_ELIGIBILITY is already at person level with the earliest start episode")
}

# check if the matching variables have NA values
if (any(is.na(d3_eligibility$SV_ECOG_STATUS)) | 
    any(is.na(d3_eligibility$SV_METASTASIS_LIVER))) {
  logger::log_info("SV_ECOG_STATUS or SV_METASTASIS_LIVER contain NA values")
  stop("SV_ECOG_STATUS or SV_METASTASIS_LIVER contain NA values")
}

# Create the profile variable - all combinations between SV_ECOG_STATUS and SV_METASTASIS_LIVER
d3_eligibility[, profile := fcase(
  SV_ECOG_STATUS == 0 & SV_METASTASIS_LIVER == FALSE, "A",
  SV_ECOG_STATUS == 0 & SV_METASTASIS_LIVER == TRUE,  "B",
  SV_ECOG_STATUS == 1 & SV_METASTASIS_LIVER == FALSE, "C",
  SV_ECOG_STATUS == 1 & SV_METASTASIS_LIVER == TRUE,  "D"
  )]

tab_exp <- table(d3_eligibility$eligible_exposed, d3_eligibility$profile)
tab_con <- table(d3_eligibility$eligible_control, d3_eligibility$profile)

logger::log_info(paste("Eligible exposed by profile", paste(capture.output(tab_exp), collapse = " ")))
logger::log_info(paste("Eligible control by profile", paste(capture.output(tab_con), collapse = " ")))

# Profile counts
profile_counts <- d3_eligibility[, .N,
                                 by = .(profile)]
# Empty profiles
empty_profiles <- profile_counts[N == 0]

if (nrow(empty_profiles) > 0) {
  logger::log_warn(paste("Empty profiles detected:", paste(empty_profiles$profile, collapse = ",")))
}

# Decision making based on the number of exposed/controls
if (!config_values$dummy_data_used && num_exp <= 220 && num_con <= 220) {
  logger::log_warn("Insufficient exposed or control patients. Matching is not feasible")
  
  # already selected the eligible cases
  d4_study_cohort <- d3_eligibility
  
  d4_study_cohort[, group := ifelse(eligible_exposed, "exposed", "control")]
  
  logger::log_warn("Recommend using propensity score adjustment")
  
} else {
  logger::log_info("Proceeding with matched cohort construction")
  
  # already selected the eligible cases
  d4_study_cohort <- d3_eligibility
  
  # create the group variable
  d4_study_cohort[, group := ifelse(eligible_exposed, "exposed", "control")]
  
  # remove rows with empty group variable
  d4_study_cohort <- d4_study_cohort[!is.na(group)]
  
  # T0
  d4_study_cohort[, T0 := start_episode]
  
  # Apply the match_on_cov_profile variable
  d4_study_cohort_output <- match_on_cov_profile(d4_study_cohort, 
                                          seed = 123)
  # the d4 study cohort
  d4_study_cohort <- d4_study_cohort_output
}

logger::log_info(paste("D4_StudyCohort:", nrow(d4_study_cohort[d4_study_cohort$group %in% c("exposed", "control")]), "rows"))
logger::log_info(paste("D4_StudyCohort exposed:", nrow(d4_study_cohort[d4_study_cohort$group == "exposed"]), "rows"))
logger::log_info(paste("D4_StudyCohort control:", nrow(d4_study_cohort[d4_study_cohort$group == "control"]), "rows"))
logger::log_info(paste("Unmatched cohort:", nrow(d4_study_cohort[d4_study_cohort$group == "unmatched", ]), "rows"))
# Number of exposed patients
num_exp_pts <- length(unique(d4_study_cohort[group == "exposed", ]$person_id))
logger::log_info(paste("Number of exposed patients:", num_exp_pts))
# Number of control patients
num_con_pts <- length(unique(d4_study_cohort[group == "control", ]$person_id))
logger::log_info(paste("Number of control patients:", num_con_pts))
# Check if we have duplicated person_id in the study cohort
d4_duplicated <- d4_study_cohort[, .N, by = person_id][N > 1]
if (nrow(d4_duplicated) > 0) {
  logger::log_warn(paste("Duplicated person_id found:", paste(d4_duplicated$person_id, collapse = ",")))
}

# Save the D4_study_cohort
picard::save(
  data = d4_study_cohort,
  file_path = dir_d4,
  file_name = "D4_StudyCohort.Rds"
)

# clean environment
rm(d3_eligibility, d4_study_cohort_output, 
   d4_study_cohort, empty_profiles,
   profile_counts)

logger::log_info("Completed: create_cohort")
