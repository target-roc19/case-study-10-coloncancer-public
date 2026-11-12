# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_figure_5.R")
  lm$start_capturing_prints()
}

logger::log_info("Starting: Creating Figure 5")

# ----------------------------
# LOAD DATA
# ----------------------------
# config parameters
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
dir_functions <- config_project$create_figure_5.R$functions$dir_common
create_figure_5 <- config_project$create_figure_5.R
dir_d4 <- config_project$create_figure_5.R$input$dir_d4
dir_metadata <- config_project$create_figure_5.R$input$dir_metadata
dir_d5 <- config_project$create_figure_5.R$output$dir_d5
save_output <- config_project$create_figure_5.R$output$save_output


# Load the study cohort covariates
file_extension <- ifelse(config_values$dummy_data_used, ".csv", ".parquet")

if (config_values$dummy_data_used) { # if dummy data are used
  file_path_covariates <- file.path(
    "tests", "testthat", "data", "D4_StudyCohort",
    paste0("D4_StudyCohort_Covariates", file_extension)
  )
} else { # for real data
  file_path_covariates <- file.path(
    dir_d4,
    paste0("D4_StudyCohortCovariates", file_extension)
  )
}

d4_covariates_unmatched <- picard::load(file_path_covariates, sep = ";")

# load the d4 studycohort analysis dataset
file_path_studycohort <- file.path(dir_d4, paste0("D4_StudyCohort_Analysis", file_extension))
d4_covariates_matched <- picard::load(file_path_studycohort, sep = ";")

# metadata - will always be a csv
file_path_metadata <- file.path(dir_metadata, "BaselineDescriptives_Metadata.csv")
table_metadata <- picard::load(file_path_metadata, sep = ",")

# Remove comorbidities as these are not yet present
table_metadata <- table_metadata[!var %in% c("SV_COMORB_CVD", "SV_COMORB_HT", "SV_COMORB_DM", "SV_COMORB_HC")]

logger::log_info("Metadata loaded")

# source functions
source(file.path(dir_functions, "supplemental_analysis_fun.R"))

logger::log_info("functions sourced")

#----------------------------
#  PLOT ASD
#----------------------------
# for the unmathed and unweighted dataset
# select groupp variable and age at index, sex, ECOG PS, site of primary tumour, topography of metastases, RAS mutation status, and BRAF mutation status.
cov_inf_unmatched <- d4_covariates_unmatched[, c("group_unmatched", "SV_AGE", "SV_SEX", "SV_ECOG_STATUS", "SV_SITE_SIDEDNESS", "SV_METASTASIS_LIVER", "SV_METASTASIS_LUNG", "SV_METASTASIS_PERITONEUM", "SV_METASTASIS_OTHER", "SV_METASTASIS_LYMPH", "SV_METASTASIS_MULTIPLE", "SV_MUTATION_BRAF", "SV_MUTATION_RAS", "SV_DIAGNOSED_IN_2015", 
"SV_DIAGNOSED_IN_2021")]
# rename group_unmatched to group for consistency
data.table::setnames(cov_inf_unmatched, "group_unmatched", "group")
# issue with DescriptiveTable asd function 
cov_inf_unmatched<- data.table::copy(cov_inf_unmatched)[,
  group := data.table::fifelse(group == "control", "CONTROL", "EXPOSED")
]

# same but for matched dataset
# include only people with include_trimming == TRUE for the matched and weighted analysis
d4_covariates_matched <- d4_covariates_matched[include_after_trimming == TRUE]

cov_inf_matched <- d4_covariates_matched[,
  c("group", "SV_AGE", "SV_SEX", "SV_ECOG_STATUS", "SV_SITE_SIDEDNESS", "SV_METASTASIS_LIVER", "SV_METASTASIS_LUNG", "SV_METASTASIS_PERITONEUM", "SV_METASTASIS_OTHER", "SV_METASTASIS_LYMPH", "SV_METASTASIS_MULTIPLE", "SV_MUTATION_BRAF", "SV_MUTATION_RAS", "SV_DIAGNOSED_IN_2015", "SV_DIAGNOSED_IN_2021", "ip_weight")
]
# issue with DescriptiveTable asd function 
cov_inf_matched<- data.table::copy(cov_inf_matched)[,
  group := data.table::fifelse(group == "control", "CONTROL", "EXPOSED")
]

# plot ASDs
figure_5_data <- plot_asd(
  dt_unmatched = cov_inf_unmatched,
  dt_matched = cov_inf_matched,
  metadata = table_metadata
)

figure_5 <- figure_5_data$figure_asd
asds_data <- figure_5_data$asds_combined

# in asds_data, round the ASD values to 3 decimal places
# as numeric
asds_data[, asd_matched := as.numeric(asd_matched)]
asds_data[, asd_matched := round(asd_matched, 3)]
# ----------------------------
# SAVE OUTPUT
# ----------------------------
ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_5.png"), plot = figure_5, width = 10, height = 8, dpi = 300)

ggplot2::ggsave(filename = paste0(dir_d5, "/Figure_5.pdf"), plot = figure_5, width = 10, height = 8, dpi = 300)

picard::save(
  data = asds_data,
  file_path = dir_d5,
  file_name = "Figure_5_ASD_Combined.csv"
)
# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d4_covariates_unmatched, d4_covariates_matched, table_metadata, cov_inf_unmatched, cov_inf_matched, figure_5)
