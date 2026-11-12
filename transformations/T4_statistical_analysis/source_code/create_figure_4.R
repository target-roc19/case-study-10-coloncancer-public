# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_figure_4.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Figure 4 - Matching statistics among exposed individuals by calendar week")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

dir_functions <- config_project$create_figure_4.R$functions$dir_common
create_figure_4 <- config_project$create_figure_4.R
dir_d4 <- config_project$create_figure_4.R$input$dir_d4
dir_d5 <- config_project$create_figure_4.R$output$dir_d5

# Load the Study Cohort
file_path_study_cohort <- file.path(dir_d4, "D4_StudyCohort.Rds")
d4_study_cohort <- picard::load(file_path_study_cohort)
logger::log_info("D4 StudyCohort loaded")

# ----------------------------
# LOAD FUNCTIONS
# ----------------------------
source(file.path(dir_functions, "supplemental_analysis_fun.R"))
logger::log_info("functions sourced")

# ----------------------------
# PREPARE DATA
# ----------------------------
logger::log_info(
  paste(
    "T0 class:",
    paste(class(d4_study_cohort$T0), collapse = ", ")
  )
)

# create week start variable for each individual based on T0
d4_study_cohort[, week_start := as.Date(
  lubridate::floor_date(
    as.Date(T0),
    unit = "week",
    week_start = 1
  )
)]


logger::log_info(paste(
  "week_start missing:" ,
  sum(is.na(d4_study_cohort$week_start))
))

logger::log_info(
  paste(
    "week_start range:",
    min(d4_study_cohort$week_start, na.rm = TRUE),
    "to",
    max(d4_study_cohort$week_start, na.rm = TRUE)
  )
)

logger::log_info(
  paste(
    "Unique week_start values:",
    data.table::uniqueN(d4_study_cohort$week_start)
  )
)

# start week: 
start_week <- as.Date(lubridate::floor_date(
  as.Date(config_values$start_study_date),
  unit = "week",
  week_start = 1
))
# end week:
end_week <- as.Date(lubridate::floor_date(
  as.Date(config_values$end_eligibility_date),
  unit = "week",
  week_start = 1
))

logger::log_info(
  paste(
    "Configured study weeks:",
    start_week,
    "to",
    end_week
  )
)

# Aggregate counts
matching_stats <- d4_study_cohort[, .(
  matched_exposed = sum(group == "exposed"),
  matched_control = sum(group == "control"),
  unmatched_exposed = sum(group == "unmatched" & eligible_exposed),
  unmatched_control = sum(group == "unmatched" & eligible_control)
), by = week_start]

logger::log_info(
  paste(
    "matching_stats contains",
    nrow(matching_stats),
    "rows before merge."
  )
)

logger::log_info(
  paste(
    "matching_stats week range:",
    min(matching_stats$week_start, na.rm = TRUE),
    "to",
    max(matching_stats$week_start, na.rm = TRUE)
  )
)

logger::log_info(
  paste(
    "Unique weeks in matching_stats:",
    data.table::uniqueN(matching_stats$week_start)
  )
)

logger::log_info("First 20 rows of matching_stats:")
logger::log_info(
  paste(
    capture.output(print(head(matching_stats, 20))),
    collapse = "\n"
  )
)

logger::log_info(paste(
  "Totals before merge:",
  "matched_exposed =", sum(matching_stats$matched_exposed),
  "matched_control =", sum(matching_stats$matched_control),
  "unmatched_exposed =", sum(matching_stats$unmatched_exposed),
  "unmatched_control =", sum(matching_stats$unmatched_control)
))

# Create complete sequence of weeks
all_weeks <- data.table::data.table(
  week_start = seq(
    from = start_week,
    to = end_week,
    by = "week"
  )
)

logger::log_info(
  paste(
    "all_weeks week_start class:",
    paste(class(all_weeks$week_start), collapse = ", ")
  )
)

# Select only the 2015 and 2021 years
all_weeks <- all_weeks[lubridate::year(week_start) %in% c(2015, 2021)]

logger::log_info(
  paste(
    "all_weeks contains",
    nrow(all_weeks),
    "weeks."
  )
)

logger::log_info(
  paste(
    "all_weeks range:",
    min(all_weeks$week_start),
    "to",
    max(all_weeks$week_start)
  )
)

logger::log_info(
  paste(
    "Number of overlapping weeks:",
    length(intersect(
      matching_stats$week_start,
      all_weeks$week_start
    )),
    "of",
    nrow(matching_stats),
    "weeks in matching_stats."
  )
)

# Merge the two data.tables to ensure all weeks are represented
matching_stats <- merge(
  all_weeks,
  matching_stats,
  by = "week_start",
  all.x = TRUE,
  sort = TRUE
)

logger::log_info(paste(
  "Rows after merge:",
  nrow(matching_stats)
))

logger::log_info("First 20 rows after merge:")
logger::log_info(
  paste(
    capture.output(print(head(matching_stats, 20))),
    collapse = "\n"
  )
)

logger::log_info(
  paste(
    "NA counts after merge:",
    "matched_exposed =",
    sum(is.na(matching_stats$matched_exposed)),
    "matched_control =",
    sum(is.na(matching_stats$matched_control)),
    "unmatched_exposed =",
    sum(is.na(matching_stats$unmatched_exposed)),
    "unmatched_control =",
    sum(is.na(matching_stats$unmatched_control))
  )
)

# Replace NAs with 0
count_cols <- c(
  "matched_exposed",
  "matched_control",
  "unmatched_exposed",
  "unmatched_control"
)

matching_stats[
  ,
  (count_cols) := lapply(
    .SD,
    function(x) data.table::fifelse(is.na(x), 0L, as.integer(x))
  ),
  .SDcols = count_cols
]

logger::log_info(
  paste(
    "Totals after replacing NAs:",
    "matched_exposed =", sum(matching_stats$matched_exposed),
    "matched_control =", sum(matching_stats$matched_control),
    "unmatched_exposed =", sum(matching_stats$unmatched_exposed),
    "unmatched_control =", sum(matching_stats$unmatched_control)
  )
)

# Create calendar week label
matching_stats[, calendar_week := sprintf(
  "%d-%02d",
  lubridate::isoyear(week_start),
  lubridate::isoweek(week_start)
)]


# Save the data used for the plot
file_path_matching_stats <- file.path(dir_d5, "D5_MatchingStats.Rds")
picard::save(matching_stats, file_path_matching_stats)

# ----------------------------
# CREATE FIGURE 4
# ----------------------------
# Create a bar plot of matching statistics among exposed individuals by calendar week for 2015
d4_study_cohort_2015 <- matching_stats[week_start >= as.Date("2015-01-01") & week_start < as.Date("2016-01-01")]
figure_4_2015 <- matching_bar_plot(d4_study_cohort_2015)
# Create the same one but for 2021
d4_study_cohort_2021 <- matching_stats[week_start >= as.Date("2021-01-01") & week_start < as.Date("2022-01-01")]
figure_4_2021 <- matching_bar_plot(d4_study_cohort_2021)

# ----------------------------
# SAVE OUTPUT
# ----------------------------
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_4_2015.png"), plot = figure_4_2015, width = 10, height = 8, dpi = 300)
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_4_2021.png"), plot = figure_4_2021, width = 10, height = 8, dpi = 300)

ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_4_2015.pdf"), plot = figure_4_2015, width = 10, height = 8, dpi = 300)
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_4_2021.pdf"), plot = figure_4_2021, width = 10, height = 8, dpi = 300)

logger::log_info("Saving: Figure 4")

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(matching_stats, d4_study_cohort, d4_study_cohort_2015, d4_study_cohort_2021, figure_4_2015, figure_4_2021)

logger::log_info("Completed: Figures 4 created")
