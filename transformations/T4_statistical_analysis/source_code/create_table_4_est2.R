### Table 4 Follow-up and censoring reasons by treatment (Estimand 2)
### group (CapOx-B vs CapOx) for disease progression or death cohort in the NCR

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Table4_est2.R")
  lm$start_capturing_prints()
}

picard::load_config()

logger::log_info("Starting: Table 4 Follow-up, IE and censoring reasons (Est 2)")

# ----------------------------
# LOAD DATA
# ----------------------------
logger::log_info("Reading D4_StudyCohort (before matching)")

config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
Table4_config <- config_project$create_table_4.R

# Extract config parameters
dir_d4 <- Table4_config$input$dir_d4
dir_d5 <- Table4_config$output$dir_d5
save_output <- Table4_config$output$save_output

# Load the study cohort covariates
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) { # if dummy data are used
  file_path_fup <- file.path(
    "tests", "testthat", "data", "D4_StudyCohort",
    paste0("D4_StudyCohort_FUP_before_matching", file_extension)
  )
} else { # for real data
  file_path_fup <- file.path(
    dir_d4,
    paste0("D4_StudyCohortFup_before_matching", file_extension)
  )
}

fup <- picard::load(file_path_fup, sep = ";")

func_dir <- file.path("transformations", "common_functionality", "functions")
source(file.path(func_dir, "fup_ie_cens_tab.R"))
source(file.path(func_dir, "summary_functions.R"))
logger::log_info("functions sourced")

# ============================================================
# Create event indicators and dates
# ============================================================

logger::log_info("Creating event indicators and dates")
# End of follow-up
fup[, ie_end_date := general_end_fup_2]

# Intercurrent events
fup[, `:=`(
  # Discontinuation of bevacizumab
  # in CapOx-B group only
  table4_disc_b_d = data.table::fifelse(
    !is.na(ie_disc_b_d) & ie_disc_b_d <= ie_end_date,
    ie_disc_b_d,
    as.Date(NA)
  ),
  # Premature partial discontinuation of bevacizumab and capacitabine or oxaliplatin (CapOx-B only)
  table4_disc_b_and_cap_or_ox_d = data.table::fifelse(
    ie_partial_disc == "b_and_cap_or_ox" &
      ie_partial_disc_d <= ie_end_date,
    ie_partial_disc_d,
    as.Date(NA)
  ),
  # Premature full discontinuation of bevacizumab and capacitabine and oxaliplatin on same day (CapOx-B only)
  table4_full_disc_capoxb_d = data.table::fifelse(
    !is.na(ie_full_disc_d) &
      group == "exposed" &
      ie_full_disc_d <= ie_end_date,
    ie_full_disc_d,
    as.Date(NA)
  )
)]

# Outcomes
fup[, `:=`(
  table4_local_treatment_d = data.table::fifelse(
    reason_censoring_2 == "SV_PROCEDURE",
    general_end_fup_2,
    as.Date(NA)
  ),
  table4_progression_d = data.table::fifelse(
    reason_censoring_2 == "SV_DISEASE_PROGRESSION",
    general_end_fup_2,
    as.Date(NA)
  ),
  table4_death_d = data.table::fifelse(
    reason_censoring_2 == "death_d",
    general_end_fup_2,
    as.Date(NA)
  ),
  table4_progression_or_death = data.table::fifelse(
    reason_censoring_2 %in% c("SV_DISEASE_PROGRESSION", "death_d"),
    general_end_fup_2,
    as.Date(NA)
  )
)]

# Reasons for end of follow-up
fup[, `:=`(
  table4_ltfu_d = data.table::fifelse(
    reason_censoring_2 == "loss_to_follow_up",
    general_end_fup_2,
    as.Date(NA)
  ),
  table4_end_data_d = data.table::fifelse(
    reason_censoring_2 == "end_observation_data_d",
    general_end_fup_2,
    as.Date(NA)
  ),
  table4_end_study_d = data.table::fifelse(
    reason_censoring_2 == "end_study_date",
    general_end_fup_2,
    as.Date(NA)
  ),
  # table4_alt_cens2 = data.table::fifelse(
  #   reason_censoring_2 == "capox_disc_earliest_d",
  #   general_end_fup_2,
  #   as.Date(NA)
  # ),
  table4_disc_cap_or_ox_d = data.table::fifelse(
    ie_partial_disc == "cap_or_ox" &
      ie_partial_disc_d == general_end_fup_2,
    ie_partial_disc_d,
    as.Date(NA)
  ),
  table4_disc_cap_and_ox_d = data.table::fifelse(
    ie_partial_disc == "ox_and_cap" &
      ie_partial_disc_d == general_end_fup_2,
    ie_partial_disc_d,
    as.Date(NA)
  ),
  table4_full_disc_capox_d = data.table::fifelse(
    !is.na(ie_full_disc_d) &
      group == "control" &
      ie_full_disc_d <= ie_end_date,
    ie_full_disc_d,
    as.Date(NA)
  ),
  
  # Treatment switch: addition of bevacizumab
  table4_switch_b_d = data.table::fifelse(
    ie_switch == "b" &
      !is.na(ie_switch_d) &
      ie_switch_d <= ie_end_date,
    ie_switch_d,
    as.Date(NA)
  ),
  
  # Treatment switch: addition of chemotherapy backbone
  table4_switch_chemo_d = data.table::fifelse(
    ie_switch == "chemo" &
      !is.na(ie_switch_d) &
      ie_switch_d <= ie_end_date,
    ie_switch_d,
    as.Date(NA)
  )
)]

# ============================================================
# Create Table 4
# ============================================================
events <- list(
  "Person-months of follow-up" = "ie_end_date",
  "Premature treatment discontinuation of bevacizumab (CapOx-B only)" = "table4_disc_b_d",
  "Premature partial discontinuation of bevacizumab and capecitabine or oxaliplatin (CapOx-B only)" = "table4_disc_b_and_cap_or_ox_d",
  "Premature full discontinuation of bevacizumab and capacitabine and oxaliplatin on same day (CapOx-B only)" = "table4_full_disc_capoxb_d",
  "Local treatment" = "table4_local_treatment_d",
  "Disease progression" = "table4_progression_d",
  "Death" = "table4_death_d",
  "Disease progression or death" = "table4_progression_or_death",
  "Disenrollment or migration" = "table4_ltfu_d",
  "End of data availability (reached 104 weeks of follow-up)" = "table4_end_data_d",
  "End of study date" = "table4_end_study_d",
  "Premature partial discontinuation of capecitabine or oxaliplatin" = "table4_disc_cap_or_ox_d",
  "Premature partial discontinuation of capecitabine and oxaliplatin (CapOx-B only)" = "table4_disc_cap_and_ox_d",
  "Premature full discontinuation of capecitabine and oxaliplatin (CapOx only)" = "table4_full_disc_capox_d",
  "Treatment switch - addition of bevacizumab" = "table4_switch_b_d",
  "Treatment switch - chemotherapy" = "table4_switch_chemo_d"
)

group_levels <- c("Overall", unique(as.character(fup$group[!is.na(fup$group)])))

logger::log_info("Creating Table 4 for each treatment group")
# Build one long table with all groups (including Overall), then reshape wide.
table4_long <- data.table::rbindlist(
  lapply(group_levels, function(g) {
    data_group <- if (g == "Overall") fup[!is.na(group)] else fup[group == g]
    
    out <- data.table::rbindlist(
      lapply(names(events), function(x) {
        summarise_event(data_group, events[[x]], x)
      })
    )
    
    out[, group := g]
    out
  }),
  fill = TRUE
)

metrics <- setdiff(names(table4_long), c("event", "group"))

table4_long <- data.table::melt(
  table4_long,
  id.vars = c("event", "group"),
  measure.vars = metrics,
  variable.name = "metric",
  value.name = "value"
)

table4 <- data.table::dcast(
  table4_long,
  event ~ group + metric,
  value.var = "value"
)

# Keep event order from the input event definition.
table4[, event := factor(event, levels = names(events))]
data.table::setorder(table4, event)
table4[, event := as.character(event)]

# ----------------------------
# SAVE AND CLEAN ENVIRONMENT
# ----------------------------

picard::save(
  data = table4,
  file_path = dir_d5,
  file_name = "Table_4_est2.csv"
)

rm(fup, table4_long, table4, events, group_levels, metrics)

logger::log_info(c("Table 4 (Est 2) - SUCCEED!"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
