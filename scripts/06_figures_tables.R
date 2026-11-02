# 06_figures_tables.R
# Nigeria DHS Immunization Analysis
# Publication-ready figures and tables

source("scripts/00_setup.R")

# Load analysis data, survey design, and model outputs
analysis_data <- readRDS(file.path(project_root, "data", "processed", "analysis_data.rds"))
dhs_design <- readRDS(file.path(project_root, "data", "processed", "dhs_design.rds"))
model_zero_dose <- readRDS(file.path(project_root, "outputs", "models", "model_zero_dose.rds"))
model_conventional_dropout <- readRDS(file.path(project_root, "outputs", "models", "model_conventional_dropout.rds"))

# Recreate survey design with current data
dhs_design <- survey::svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~weight,
  data = analysis_data,
  nest = TRUE
)

# ------------------------------------------------------------
# TABLE 1: Sample characteristics
# ------------------------------------------------------------

table1_vars <- c(
  "survey_year",
  "wealth_quintile",
  "maternal_education",
  "residence",
  "region",
  "anc_attendance"
)

table1_list <- lapply(table1_vars, function(v) {
  f <- as.formula(paste0("~", v))
  tab <- survey::svytable(f, dhs_design)
  prop <- prop.table(tab) * 100

  data.frame(
    variable = v,
    category = names(tab),
    weighted_percent = round(as.numeric(prop), 1),
    stringsAsFactors = FALSE
  )
})

table1 <- dplyr::bind_rows(table1_list)

write.csv(
  table1,
  file.path(project_root, "outputs", "tables", "table1_sample_characteristics.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# TABLE 2: National prevalence estimates
# ------------------------------------------------------------

prev_by_year <- function(var_name) {
  survey::svyby(
    as.formula(paste0("~", var_name)),
    ~survey_year,
    dhs_design,
    survey::svymean,
    na.rm = TRUE,
    keep.var = FALSE
  )
}

zero_by_year <- prev_by_year("zero_dose")
dropout_by_year <- prev_by_year("dropout_prevalence")
conventional_by_year <- prev_by_year("conventional_dropout")

table2 <- data.frame(
  survey_year = c("2013", "2018", "2024"),
  zero_dose = round(as.numeric(zero_by_year[[2]]) * 100, 1),
  dropout_prevalence = round(as.numeric(dropout_by_year[[2]]) * 100, 1),
  conventional_dropout = round(as.numeric(conventional_by_year[[2]]) * 100, 1)
)

write.csv(
  table2,
  file.path(project_root, "outputs", "tables", "table2_national_trends.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# FIGURE 1: Trend in zero-dose prevalence
# ------------------------------------------------------------

fig1_data <- data.frame(
  survey_year = factor(c("2013", "2018", "2024"), levels = c("2013", "2018", "2024")),
  prevalence = round(as.numeric(zero_by_year[[2]]) * 100, 1)
)

fig1 <- ggplot2::ggplot(fig1_data, ggplot2::aes(x = survey_year, y = prevalence, group = 1)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 3) +
  ggplot2::labs(
    title = "Trend in Zero-Dose Prevalence",
    x = "Survey year",
    y = "Zero-dose prevalence (%)"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(project_root, "outputs", "figures", "figure1_zero_dose_trend.png"),
  plot = fig1,
  width = 7,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# FIGURE 2: Zero-dose prevalence by ANC attendance
# ------------------------------------------------------------

anc_levels <- c("0 visits", "1-3 visits", "4-7 visits", "8+ visits")

anc_design <- survey::svyby(
  ~zero_dose,
  ~anc_attendance,
  dhs_design,
  survey::svymean,
  na.rm = TRUE,
  keep.var = FALSE
)

fig2_data <- data.frame(
  anc_attendance = factor(anc_design$anc_attendance, levels = anc_levels),
  prevalence = round(as.numeric(anc_design[[2]]) * 100, 1)
)

fig2 <- ggplot2::ggplot(fig2_data, ggplot2::aes(x = anc_attendance, y = prevalence)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Zero-Dose Prevalence by Antenatal Care Attendance",
    x = "Antenatal care attendance",
    y = "Zero-dose prevalence (%)"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(project_root, "outputs", "figures", "figure2_anc_zero_dose.png"),
  plot = fig2,
  width = 7,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# TABLE 3: Model outputs
# ------------------------------------------------------------

zero_dose_results <- summary(model_zero_dose)$coefficients
conventional_results <- summary(model_conventional_dropout)$coefficients

zero_dose_results_df <- data.frame(
  term = rownames(zero_dose_results),
  estimate = zero_dose_results[, 1],
  std_error = zero_dose_results[, 2],
  p_value = zero_dose_results[, 4],
  row.names = NULL
)

conventional_results_df <- data.frame(
  term = rownames(conventional_results),
  estimate = conventional_results[, 1],
  std_error = conventional_results[, 2],
  p_value = conventional_results[, 4],
  row.names = NULL
)

write.csv(
  zero_dose_results_df,
  file.path(project_root, "outputs", "tables", "table3_zero_dose_model.csv"),
  row.names = FALSE
)

write.csv(
  conventional_results_df,
  file.path(project_root, "outputs", "tables", "table4_conventional_dropout_model.csv"),
  row.names = FALSE
)

cat("Figures and tables generated successfully.\n")