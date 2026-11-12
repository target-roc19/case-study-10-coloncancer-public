## Crude and adjusted hazard ratios (95% CI) for the effect of CapOx-B vs CapOx
## on disease progression or death (Estimand 2)

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_figure_8_9_unw.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Figure 8 - Schoenfeld residuals and Figure 9 - log log survival plot")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

dir_functions <- config_project$create_figure_8_9.R$functions$dir_common
create_figure_8_9 <- config_project$create_figure_8_9.R
dir_d4 <- config_project$create_figure_8_9.R$input$dir_d4
dir_d5 <- config_project$create_figure_8_9.R$output$dir_d5

# Load the Study Cohort FUP dataset
# for matched + unweighted cohort
file_path_fup <- file.path(dir_d4, "D4_StudyCohort_Analysis.parquet")
d4_fup <- picard::load(file_path_fup, sep = ";")
logger::log_info("D4 StudyCohortAnalysis loaded")

# ----------------------------
# LOAD FUNCTIONS
# ----------------------------
source(file.path(dir_functions, "outcome_analysis_fun.R"))
source(file.path(dir_functions, "supplemental_analysis_fun.R"))
logger::log_info("functions sourced")

# ----------------------------
# CREATE FIGURE 8
# ----------------------------
# For estimand 1:
figure_8_est1 <- plot_schoenfeld_unw(d4_fup, fup_days = "fup_length_days_1_3", event_count = "eventCount_1_3")

# Check that there are events
n_events <- sum(d4_fup$eventCount_2 == 1, na.rm = TRUE)

if (n_events == 0) {
  logger::log_error(
    "Cannot plot Schoenfeld residuals because there are no events in the dataset.",
    call. = FALSE
  )
} else {
  # For estimand 2:
  figure_8_est2 <- plot_schoenfeld_unw(d4_fup, fup_days = "fup_length_days_2", event_count = "eventCount_2")

  ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_8_est2_unw.png"), plot = figure_8_est2, width = 10, height = 8, dpi = 300)
  ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_8_est2_unw.pdf"), plot = figure_8_est2, width = 10, height = 8, dpi = 300)
  rm(figure_8_est2)
}

# ----------------------------
# CREATE FIGURE 9
# ----------------------------
# For estimand 1:
figure_9_est1 <- plot_loglog_unw(d4_fup, fup_days = "fup_length_days_1_3", event_count = "eventCount_1_3")

# For estimand 2:
figure_9_est2 <- plot_loglog_unw(d4_fup, fup_days = "fup_length_days_2", event_count = "eventCount_2")

# For estimand 3:
figure_9_est3 <- plot_loglog_weibull_unw(d4_fup, fup_days = "fup_length_days_1_3", event_count = "eventCount_1_3")
# ----------------------------
# SAVE OUTPUT
# ----------------------------
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_8_est1_unw.png"), plot = figure_8_est1, width = 10, height = 8, dpi = 300)
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_8_est1_unw.pdf"), plot = figure_8_est1, width = 10, height = 8, dpi = 300)

ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_9_est1_unw.pdf"), plot = figure_9_est1, width = 10, height = 8, dpi = 300)
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_9_est1_unw.png"), plot = figure_9_est1, width = 10, height = 8, dpi = 300)

ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_9_est2_unw.pdf"), plot = figure_9_est2, width = 10, height = 8, dpi = 300)
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_9_est2_unw.png"), plot = figure_9_est2, width = 10, height = 8, dpi = 300)

ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_9_est3_unw.pdf"), plot = figure_9_est3, width = 10, height = 8, dpi = 300)
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_9_est3_unw.png"), plot = figure_9_est3, width = 10, height = 8, dpi = 300)


logger::log_info("Saving: Figure 8 and Figure 9 for estimand 1, 2 and 3")

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d4_fup, figure_8_est1, figure_9_est1, figure_9_est2, figure_9_est3)

logger::log_info("Completed: Figures 8 and 9 created")
