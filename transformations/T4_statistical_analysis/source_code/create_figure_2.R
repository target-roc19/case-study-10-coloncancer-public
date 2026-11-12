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
  lm$start_script("create_figure_2.R")
  lm$start_capturing_prints()
}

logger::log_info(
  "Starting: Figure 2 - Cumulative incidence curves for the two treatment groups"
)


# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

dir_functions <- config_project$create_figure_2.R$functions$dir_common
create_figure_2 <- config_project$create_figure_2.R
dir_d4 <- config_project$create_figure_2.R$input$dir_d4
dir_d5 <- config_project$create_figure_2.R$output$dir_d5

# Load the Study Cohort Analysis dataset
# for the matched cohort with the IP weights attached
d4_analysis <- picard::load(
  file.path(dir_d4, "D4_StudyCohort_Analysis.parquet"),
  sep = ";"
)
logger::log_info("D4 StudyCohort Analysis loaded")

# for calculations before matching we will use the
d4_fup_unmatched <- picard::load(
  file.path(dir_d4, "D4_StudyCohortFUP_before_matching.parquet"),
  sep = ";"
)
logger::log_info("D4 StudyCohortFUP_before_matching loaded")

# If trimming is enabled, filter the analysis dataset to include only those persons who are included after trimming. Log the number of persons included after trimming.
n_before_trimming <- nrow(d4_analysis)
d4_analysis <- d4_analysis[include_after_trimming == TRUE]
logger::log_info(sprintf(
  "Figure 2.2 population: %d of %d persons included after trimming",
  nrow(d4_analysis),
  n_before_trimming
))
# ----------------------------
# LOAD FUNCTIONS
# ----------------------------
source(file.path(dir_functions, "CI_curves_fun.R"))
logger::log_info("functions sourced")

# ----------------------------
# FIGURE 2.1 & 2.2
# ----------------------------
# Figure 2.1 is the crude analysis (unmatched, unweighted),
# figure 2.2 the adjusted one (matched, IPT-weighted)
list_specs <- list(
  crude = list(
    dt = d4_fup_unmatched,
    weights = NULL,
    title = "Cumulative incidence (crude)"
  ),
  adjusted = list(
    dt = d4_analysis,
    weights = "ip_weight",
    title = "Cumulative incidence (adjusted, IPT-weighted)"
  )
)

list_figures <- list()

for (spec_name in names(list_specs)) {
  spec <- list_specs[[spec_name]]

  # Fit KM and prepare plotting data
  ci_input_data <- KM_fit_data(
    dt = spec$dt,
    time_col = "fup_length_days_1_3",
    event_col = "eventCount_1_3",
    group_col = "group",
    weights = spec$weights
  )

  if (nrow(ci_input_data) <= 1) {
    logger::log_warn(
      sprintf("No data available for %s", spec_name)
    )

    next
  }

  # Dynamic y-axis scaling (taken from 1026)
  scale_y <- pretty(
    c(0, max(ci_input_data$cumevent_ci_upb, na.rm = TRUE))
  )
  max_y <- max(scale_y)
  break_y <- scale_y[2] - scale_y[1]

  # Create and store figure
  list_figures[[spec_name]] <- create_km_with_input_data(
    data = ci_input_data,
    group_col = "group",
    time_col = "time",
    point_est_col = "cumevent",
    ci_lwb_col = "cumevent_ci_lwb",
    ci_upb_col = "cumevent_ci_upb",
    plot_title = spec$title,
    plot_x_axis_title = "Follow-up days",
    plot_y_axis_title = "Cumulative event probability",
    plot_y_lim_break = c(0, max_y, break_y)
  )
}

# ----------------------------
# SAVE OUTPUT
# ----------------------------
ggplot2::ggsave(
  filename = paste0(dir_d5, "/Figure_2_1.png"),
  plot = list_figures$crude,
  width = 10,
  height = 8,
  dpi = 300
)
ggplot2::ggsave(
  filename = paste0(dir_d5, "/Figure_2_1.pdf"),
  plot = list_figures$crude,
  width = 10,
  height = 8,
  dpi = 300
)
logger::log_info("Saving: Figure 2.1")

ggplot2::ggsave(
  filename = paste0(dir_d5, "/Figure_2_2.png"),
  plot = list_figures$adjusted,
  width = 10,
  height = 8,
  dpi = 300
)
ggplot2::ggsave(
  filename = paste0(dir_d5, "/Figure_2_2.pdf"),
  plot = list_figures$adjusted,
  width = 10,
  height = 8,
  dpi = 300
)
logger::log_info("Saving: Figure 2.2")

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d4_analysis, d4_fup_unmatched, n_before_trimming, list_specs, list_figures)

logger::log_info("Completed: Figures 2.1 and 2.2 created")
