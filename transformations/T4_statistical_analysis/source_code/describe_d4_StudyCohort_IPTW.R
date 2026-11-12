# ---------------------------------------------------------------
# Step: describe_d4_StudyCohort_IPTW
#
# Input:  D4_StudyCohort_IPTW
#         D4_StudyCohort_Covariates
#         <DEAP>_D5_PS_Coef.csv
#         psmodel_variables.csv
#         BaselineCharacteristics_metadata.csv
#
# Output: <DEAP>_D5_PS_density_plot.png
#         <DEAP>_D5_PS_frequency_table.csv
#         <DEAP>_D5_PS_distribution.csv
#         <DEAP>_D5_IPTW_distribution.csv
#         <DEAP>_D5_PS_model_parameters.csv
#         <DEAP>_D5_baseline_trimming_table.csv
#         <DEAP>_D5_weighting_asd.csv
#         <DEAP>_D5_weighting_asd_plot.png
#         <DEAP>_D5_IPTW_density_plot.png
#
# Diagnostics for propensity scores and IPT weights.
# ---------------------------------------------------------------

# --- Initial setup of the environment ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("describe_d4_StudyCohort_IPTW.R")
  lm$start_capturing_prints()
}

#library(ggplot2)

logger::log_info("[describe_d4_StudyCohort_IPTW] - START")

# --- Config loading ---
picard::load_config()

deap_name <- config_project$DEAP_configuration$name
cfg <- config_project$describe_d4_StudyCohort_IPTW
functions_dir <- cfg$functions$dir_common

# --- Source functions ---
source(file.path(functions_dir, "describe_iptw.R"))
source(file.path(functions_dir, "baseline_table_function.R"))

# --- Read inputs ---
logger::log_info("Reading inputs for PS diagnostics")

# Load the study cohort covariates
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) {
  # if dummy data are used
  file_path_covariates <- file.path(
    "tests",
    "testthat",
    "data",
    "D4_StudyCohort",
    paste0("D4_StudyCohort_Covariates", file_extension)
  )
} else {
  # for real data
  file_path_covariates <- file.path(
    cfg$input$dir_d4,
    paste0("D4_StudyCohortCovariates", file_extension)
  )
}

d4_studycovariates <- picard::load(file_path_covariates, sep = ";")
d4_iptw <- picard::load(
  file.path(cfg$input$dir_d4, "D4_StudyCohort_IPTW.parquet"),
  sep = ";"
)

# The two producers can emit person_id with different types (e.g. anchoR writes
# character, the IPTW step integer); align before any join.
data.table::setDT(d4_iptw)
data.table::setDT(d4_studycovariates)
d4_iptw[, person_id := as.character(person_id)]
d4_studycovariates[, person_id := as.character(person_id)]

d5_ps_coef <- picard::load(
  file.path(cfg$output$dir_d5, paste0(deap_name, "_D5_PS_Coef.csv"))
)
ps_variables <- picard::load(
  file.path(cfg$input$dir_metadata, "psmodel_variables.csv"),
  sep = ","
)
baseline_metadata <- picard::load(
  file.path(cfg$input$dir_metadata, "BaselineDescriptives_Metadata.csv"),
  sep = ","
)

ps_trim_bounds <- cfg$ps_trim_bounds
proposed_bounds_lower <- cfg$proposed_bounds_lower
proposed_bounds_upper <- cfg$proposed_bounds_upper
proposed_bounds <- c(proposed_bounds_lower, proposed_bounds_upper)
# ================================================================
# 1a. Plot densities of propensity score in control vs exposed group
#    - If trimming is applied: show before/after trimming, with
#      trim bounds on the "Before trimming" panel.
#    - If trimming is not applied: show the pre-trimming density
#      with the proposed trim bounds.
# ================================================================
logger::log_info("Creating PS density plot")

# Trimming is considered applied when ps_trim_bounds is specified.
trimming_applied <- !is.null(ps_trim_bounds)

stage_levels <- c("Before trimming", "After trimming")

# Create plotting data
plot_data <- if (trimming_applied) {
  data.table::rbindlist(list(
    data.table::copy(d4_iptw)[,
      stage := stage_levels[1]
    ],
    data.table::copy(d4_iptw)[
      include_after_trimming == TRUE
    ][,
      stage := stage_levels[2]
    ]
  ))
} else {
  data.table::copy(d4_iptw)[,
    stage := stage_levels[1]
  ]
}

# Create readable group labels
plot_data[,
  group_label := factor(
    group,
    levels = c("control", "exposed"),
    labels = c("Control", "Exposed")
  )
]

# Ensure stage order
plot_data[,
  stage := factor(stage, levels = stage_levels)
]

# Create base density plot
fig_ps_density <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = p_score)
) +
  ggplot2::geom_density(
    ggplot2::aes(
      color = group_label,
      fill = group_label
    ),
    linewidth = 0.8,
    alpha = 0.1
  ) +
  ggplot2::scale_fill_manual(
    values = c("darkblue", "darkred")
  ) +
  ggplot2::scale_color_manual(
    values = c("darkblue", "darkred")
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 1, 0.2)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 10),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::labs(
    x = "Propensity Score",
    y = "Density"
  )

# Add trim bounds
if (trimming_applied) {
  # Use the actual bounds used for trimming
  vline_data <- data.table::data.table(
    stage = factor(
      stage_levels[1],
      levels = stage_levels
    ),
    xintercept = ps_trim_bounds
  )

  fig_ps_density <- fig_ps_density +
    ggplot2::facet_wrap(
      ~stage,
      ncol = 1
    ) +
    ggplot2::geom_vline(
      data = vline_data,
      ggplot2::aes(xintercept = xintercept),
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.5
    )
} else {
  # Use the proposed bounds for the initial diagnostic plot
  vline_data <- data.table::data.table(
    xintercept = proposed_bounds
  )

  fig_ps_density <- fig_ps_density +
    ggplot2::geom_vline(
      data = vline_data,
      ggplot2::aes(xintercept = xintercept),
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.5
    )
}

# Save plot
ggplot2::ggsave(
  filename = paste0(
    deap_name,
    "_D5_PS_density_plot.png"
  ),
  path = cfg$output$dir_d5,
  plot = fig_ps_density,
  width = 18,
  height = if (trimming_applied) 18 else 10,
  dpi = 300,
  units = "cm"
)

logger::log_info(
  "PS density plot saved to D5_results/"
)

# ================================================================
# 1b. Propensity-score frequency table for trimming diagnostics
#    - Uses propensity scores before trimming
#    - PS bins have width 0.05
#    - Includes all bins from 0.00 to 0.95 for both groups
#    - Frequencies < 5 are masked as "<5"
#    - The smallest frequency >= 5 within each exposure group
#      is also masked to prevent inference by subtraction
# ================================================================
logger::log_info("Creating PS frequency table")

# Create propensity-score bins from the pre-trimming data
ps_frequency <- data.table::copy(d4_iptw)[
  !is.na(p_score),
  .(
    exposure = data.table::fifelse(
      group == "exposed",
      1L,
      0L
    ),
    p_score
  )
]

# Create propensity-score bins of width 0.05
ps_frequency[, bin := floor(p_score / 0.05) * 0.05]

# Ensure the upper boundary is 0.95
ps_frequency[
  bin > 0.95,
  bin := 0.95
]

# Avoid floating-point display issues
ps_frequency[, bin := round(bin, 2)]

# Calculate observed frequencies
ps_frequency <- ps_frequency[,
  .(frequency = .N),
  by = .(exposure, bin)
]


all_bins <- data.table::CJ(
  exposure = c(0L, 1L),
  bin = seq(0, 0.95, by = 0.05)
)

ps_frequency <- merge(
  all_bins,
  ps_frequency,
  by = c("exposure", "bin"),
  all.x = TRUE
)

# Replace missing frequencies with zero
ps_frequency[
  is.na(frequency),
  frequency := 0L
]

# Find the smallest frequency >= 5 within each exposure group.
# Zero frequencies are not considered for masking.
next_smallest_frequency <- ps_frequency[
  frequency >= 5,
  .(
    frequency_to_mask = min(frequency)
  ),
  by = exposure
]

# Add the frequency to be additionally masked
ps_frequency <- merge(
  ps_frequency,
  next_smallest_frequency,
  by = "exposure",
  all.x = TRUE
)

# Apply masking
ps_frequency[,
  frequency_numeric := frequency
]

# Create the final display column
ps_frequency[,
  frequency := as.character(frequency_numeric)
]

# Apply masking
ps_frequency[
  (frequency_numeric > 0 & frequency_numeric < 5) |
    frequency_numeric == frequency_to_mask,
  frequency := "<5"
]

# Remove helper columns
ps_frequency[,
  c("frequency_numeric", "frequency_to_mask") := NULL
]

data.table::setorder(
  ps_frequency,
  exposure,
  bin
)

logger::log_info("PS frequency table created")

# Save
picard::save(
  data = ps_frequency,
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_PS_frequency_table.csv")
)
logger::log_info("PS frequency table saved to D5_results/")
# ================================================================
# 2. Table to for description and diagnostics for propensity scores
# and stabilized IPT weights
# ================================================================
logger::log_info("Creating weight description table")

# describe_iptw() produces a summary table of the stabilized weights per group.
# Manually including the truncation per group
selected_iptw <- d4_iptw[include_after_trimming == TRUE]

trunc_cutoffs <- selected_iptw[,
  .(
    P1 = round(quantile(stabilized_iptw, 0.01), 4),
    P99 = round(quantile(stabilized_iptw, 0.99), 4)
  ),
  by = group
]

weight_table <- describe_iptw(
  p_matrix = selected_iptw,
  groups = c("control", "exposed"),
  stab_weight_col = "stabilized_iptw",
  custom_truncation = c(NA, NA),
  digits = 4
)

# Patch in per-group truncation cutoffs
weight_table[
  weight_table$labels == "If truncated, cut-offs",
  c("control_V1", "control_V2")
] <- trunc_cutoffs[group == "control", .(P1, P99)]
weight_table[
  weight_table$labels == "If truncated, cut-offs",
  c("exposed_V1", "exposed_V2")
] <- trunc_cutoffs[group == "exposed", .(P1, P99)]

picard::save(
  data = format_iptw_summary(weight_table),
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_IPTW_distribution.csv")
)
logger::log_info("IPTW distribution table saved to D5_results/")

# Per-group summary of propensity scores on the full (pre-trim) cohort.
ps_description <- describe_iptw(
  p_matrix = d4_iptw[!is.na(p_score)],
  groups = c("control", "exposed"),
  stab_weight_col = "p_score",
  custom_truncation = if (is.null(ps_trim_bounds)) {
    c(NA, NA)
  } else {
    ps_trim_bounds
  },
  digits = 4
)

picard::save(
  data = format_iptw_summary(ps_description),
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_PS_distribution.csv")
)
logger::log_info("PS distribution table saved to D5_results/")


# ================================================================
# 3. Table describing the parameters of the fitted propensity score
#    model in terms of odds ratio
# ================================================================
logger::log_info("Creating PS model parameters (OR) table")

ps_model_parameters <- format_ps_model_parameters(d5_ps_coef, ps_variables)

picard::save(
  data = ps_model_parameters,
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_PS_model_parameters.csv")
)
logger::log_info("PS model parameters table saved to D5_results/")


# ================================================================
# 3b. Baseline characteristics before vs after PS trimming + aSMD
# ================================================================
logger::log_info("Creating baseline before/after trimming table")

# Remove comorbidities as these are not yet present
baseline_metadata <- baseline_metadata[
  !var %in%
    c(
      "SV_COMORB_CVD",
      "SV_COMORB_HT",
      "SV_COMORB_DM",
      "SV_COMORB_HC",
      "SV_DIAGNOSED_IN_2015"
    )
]

baseline_trimming_table <- build_baseline_trimming_table(
  d4_covariates = d4_studycovariates,
  d4_iptw = d4_iptw,
  metadata = baseline_metadata
)

picard::save(
  data = baseline_trimming_table,
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_baseline_trimming_table.csv")
)
logger::log_info("Baseline trimming table saved to D5_results/")


# ================================================================
# 4. Weighted ASDs (using DescriptiveTable compute_asd)
# ================================================================
logger::log_info("Computing weighted ASDs")

# DescriptiveTable expects type NUM1/NUM2 instead of NUM
asd_metadata <- data.table::data.table(
  var = ps_variables$var,
  type = data.table::fifelse(
    ps_variables$vartype == "NUM",
    "NUM1",
    ps_variables$vartype
  )
)

# Merge covariates with truncated weights for the included population
cov_with_weights <- merge(
  d4_studycovariates,
  d4_iptw[include_after_trimming == TRUE, .(person_id, trunc_iptw)],
  by = "person_id"
)

CovariatesInformation <- cov_with_weights
metadata <- asd_metadata
referenceExtension <- "unweighted"
use_weights <- FALSE
group_names <- c("control", "exposed")

# delete group_unmatched column
cov_with_weights[, group_unmatched := NULL]

# Compute unweighted and weighted ASDs
asd_unweighted <- DescriptiveTable::compute_asd(
  CovariatesInformation = cov_with_weights,
  metadata = asd_metadata,
  referenceExtension = "unweighted",
  use_weights = FALSE,
  group_names = c("control", "exposed")
)

# in the DescriptiveTable::wtd.stddiff.binary() function, there is a
# hardcoded "CONTROL"/"EXPOSED" instead of using group_names (open PR #20),
# so TF or CAT return NaN for group labels "control"/"exposed"
cov_with_weights_upper <- data.table::copy(cov_with_weights)[,
  group := data.table::fifelse(group == "control", "CONTROL", "EXPOSED")
]

asd_weighted <- DescriptiveTable::compute_asd(
  CovariatesInformation = cov_with_weights_upper,
  metadata = asd_metadata,
  referenceExtension = "weighted",
  use_weights = "trunc_iptw",
  group_names = c("CONTROL", "EXPOSED")
)

# Combine and add labels
asd_combined <- merge(
  asd_unweighted,
  asd_weighted[, .(var, asd_weighted)],
  by = "var"
)
asd_combined <- merge(
  asd_combined,
  ps_variables[, .(var, label)],
  by = "var",
  all.x = TRUE
)

picard::save(
  data = asd_combined,
  file_path = cfg$output$dir_d5,
  file_name = paste0(deap_name, "_D5_weighting_asd.csv")
)
logger::log_info("Weighting ASD table saved to D5_results/")


# ================================================================
# 5. Plot of weighed vs unweighted ASDs
# ================================================================
logger::log_info("Creating ASD comparison plot")

asd_plot_data <- data.table::melt(
  asd_combined,
  id.vars = c("var", "type", "label"),
  measure.vars = c("asd_unweighted", "asd_weighted"),
  variable.name = "method",
  value.name = "asd"
)
asd_plot_data[, asd := as.numeric(asd)]
asd_plot_data[,
  method := factor(
    method,
    levels = c("asd_unweighted", "asd_weighted"),
    labels = c("Before weighting", "After weighting")
  )
]
asd_plot_data[, label := data.table::fifelse(is.na(label), var, label)]

# Order variables by unweighted ASD (ascending)
var_order <- asd_plot_data[method == "Before weighting"][order(asd), label]
asd_plot_data[, label := factor(label, levels = var_order)]

fig_asd <- ggplot2::ggplot(
  asd_plot_data[!is.na(asd)],
  ggplot2::aes(x = label, y = asd, color = method, shape = method)
) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_hline(
    yintercept = 0.1,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::scale_color_manual(values = c("darkblue", "darkblue")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 10),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::labs(x = NULL, y = "Absolute Standardised Difference")

ggplot2::ggsave(
  filename = paste0(deap_name, "_D5_weighting_asd_plot.png"),
  path = cfg$output$dir_d5,
  plot = fig_asd,
  width = 20,
  height = 14,
  dpi = 300,
  units = "cm"
)
logger::log_info("ASD comparison plot saved to D5_results/")


# ================================================================
# 6. Plot densities of stabilized IPT weights before vs after
#    truncation at P1/P99, by group, on the post-trim cohort.
# ================================================================
logger::log_info("Creating IPTW density plot")

iptw_stage_levels <- c("Before truncation", "After truncation at P1/P99")
iptw_plot_data <- data.table::melt(
  d4_iptw[include_after_trimming == TRUE],
  id.vars = c("person_id", "group"),
  measure.vars = c("stabilized_iptw", "trunc_iptw"),
  variable.name = "stage",
  value.name = "weight"
)
iptw_plot_data[,
  stage := factor(
    stage,
    levels = c("stabilized_iptw", "trunc_iptw"),
    labels = iptw_stage_levels
  )
]
iptw_plot_data[,
  group_label := factor(
    group,
    levels = c("control", "exposed"),
    labels = c("control", "exposed")
  )
]

fig_iptw_density <- ggplot2::ggplot(iptw_plot_data, ggplot2::aes(x = weight)) +
  ggplot2::geom_histogram(
    ggplot2::aes(color = group_label, fill = group_label),
    bins = 30,
    position = "identity",
    alpha = 0.4
  ) +
  ggplot2::scale_fill_manual(values = c("darkblue", "darkred")) +
  ggplot2::scale_color_manual(values = c("darkblue", "darkred")) +
  ggplot2::facet_wrap(~stage, ncol = 1, scales = "free_y") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 10),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::labs(x = "Stabilized IPT weight", y = "Frequency")

ggplot2::ggsave(
  filename = paste0(deap_name, "_D5_IPTW_density_plot.png"),
  path = cfg$output$dir_d5,
  plot = fig_iptw_density,
  width = 18,
  height = 18,
  dpi = 300,
  units = "cm"
)
logger::log_info("IPTW density plot saved to D5_results/")


# --- Cleanup ---
rm(
  d4_iptw,
  d4_studycovariates,
  d5_ps_coef,
  ps_variables,
  selected_iptw,
  plot_data,
  fig_ps_density,
  trunc_cutoffs,
  weight_table,
  ps_description,
  ps_model_parameters,
  baseline_metadata,
  baseline_trimming_table,
  asd_metadata,
  cov_with_weights,
  cov_with_weights_upper,
  asd_unweighted,
  asd_weighted,
  asd_combined,
  asd_plot_data,
  fig_asd,
  iptw_plot_data,
  fig_iptw_density
)

logger::log_info("[describe_d4_StudyCohort_IPTW] - SUCCEED")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
