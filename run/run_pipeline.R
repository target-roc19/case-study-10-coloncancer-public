#!/usr/bin/env Rscript

# ------------------------------------------------------------
# VAC4EU Semantic Harmonization Pipeline Runner (Master)
# Orchestrates all phases using the picard::pipeline class.
# ------------------------------------------------------------
if (TRUE) {
  renv::restore()
} else {
  install.packages(c('RPostgres', 'dplyr', 'renv', 'pak', 'clock'))
  pak::pkg_install("github::UMC-Utrecht-RWE/T2-DataManagementModel@v0.2.1test2")
  pak::pkg_install("github::UMC-Utrecht-RWE/episodeR@v0.0.3")
  # latest commit from the dev branch of picard
  pak::pkg_install(
    "github::UMC-Utrecht-RWE/picard@a95d3b2a360a6ffe2c9631bc5409bed90ad41116"
  )
  pak::pkg_install("github::UMC-Utrecht-RWE/anchoR@v1.5")
  pak::pkg_install("github::UMC-Utrecht-RWE/DescriptiveTable@v0.3.0")
  pak::pkg_install("github::UMC-Utrecht-RWE/CohortAnalytics@v0.0.1")
}

# --- Setup the environment ---
lm <- picard:::LoggerManager$new()
# picard::track_file_changes(path = "logs") # nolint
lm$configure(verbose = "Normal") # Low or High
logger::log_info("Loading master pipeline")

# ---- Instantiate master pipeline ----
logger::log_debug("Instantiating master pipeline class")
pipeline <- picard::pipeline$new(
  config_pipeline = "configuration/config_pipeline.yaml"
)
###############################################

# TO BE DELETED FOR DEPLOYMENT
# ensure common config_project.yaml depending on input
# Write the content to the output YAML file (overwrite if exists)
# Set configuration to sequential input or QC input --> only for developer!
input_file <- "configuration/config_project.yaml"
# output_file <- "configuration/config_project.yaml" # useful later #nolint
# yaml_content <- yaml::read_yaml(input_file) # useful later #nolint
# yaml::write_yaml(yaml_content, output_file) # useful later #nolint

###############################################

picard::load_config()
logger::log_debug("Running master pipeline")
pipeline$run()
