# =============================================================================
# NIGERIA DHS IMMUNIZATION ANALYSIS PROJECT
# Script : 09_descriptive_analysis.R
# Purpose: Survey-weighted Descriptive Analysis
# Version: 6.0
# =============================================================================

source("~/nigeria-dhs-immunization-analysis/scripts/00_setup.R")

cat("\n")
cat("==============================================================\n")
cat("DESCRIPTIVE ANALYSIS\n")
cat("==============================================================\n\n")

# =============================================================================
# LOAD FINAL ANALYSIS DATASET
# =============================================================================

cat("Loading final analysis dataset...\n\n")

analysis_data <-
  
  readRDS(
    
    file.path(
      
      paths$processed,
      
      "final_analysis_dataset.rds"
      
    )
    
  )

cat("✓ Final analysis dataset loaded.\n\n")

tibble(
  
  Metric = c(
    
    "Records",
    
    "Variables",
    
    "Survey rounds"
    
  ),
  
  Value = c(
    
    nrow(analysis_data),
    
    ncol(analysis_data),
    
    dplyr::n_distinct(
      analysis_data$survey_name
    )
    
  )
  
) %>%
  
  print()

cat("\n")

# =============================================================================
# VERIFY REQUIRED VARIABLES
# =============================================================================

required_variables <- c(
  
  "survey_name",
  
  "sample_weight",
  
  "cluster",
  
  "strata",
  
  "zero_dose",
  
  "dropout_prevalence",
  
  "conventional_dropout"
  
)

validation_table <-
  
  tibble(
    
    Variable = required_variables,
    
    Available = required_variables %in%
      names(analysis_data)
    
  )

print(validation_table)

cat("\n")

stopifnot(
  
  all(validation_table$Available)
  
)

cat("✓ Required variables verified.\n\n")

# =============================================================================
# CREATE SURVEY DESIGN
# =============================================================================

cat("Creating survey design...\n\n")

options(
  
  survey.lonely.psu = "adjust"
  
)

design <-
  
  survey::svydesign(
    
    ids = ~cluster,
    
    strata = ~strata,
    
    weights = ~sample_weight,
    
    data = analysis_data,
    
    nest = TRUE
    
  )

cat("✓ Survey design created.\n\n")

# =============================================================================
# ZERO-DOSE PREVALENCE
# =============================================================================

cat("Estimating zero-dose prevalence...\n\n")

zero_dose_results <-
  
  survey::svyby(
    
    ~zero_dose,
    
    ~survey_name,
    
    design,
    
    survey::svymean,
    
    vartype = "ci",
    
    na.rm = TRUE
    
  ) |>
  
  dplyr::transmute(
    
    survey_name,
    
    Prevalence = zero_dose * 100,
    
    Lower_CI = ci_l * 100,
    
    Upper_CI = ci_u * 100
    
  )

print(zero_dose_results)

cat("\n")

# =============================================================================
# DROPOUT PREVALENCE
# =============================================================================

cat("Estimating dropout prevalence...\n\n")

dropout_prev_results <-
  
  survey::svyby(
    
    ~dropout_prevalence,
    
    ~survey_name,
    
    design,
    
    survey::svymean,
    
    vartype = "ci",
    
    na.rm = TRUE
    
  ) |>
  
  dplyr::transmute(
    
    survey_name,
    
    Prevalence = dropout_prevalence * 100,
    
    Lower_CI = ci_l * 100,
    
    Upper_CI = ci_u * 100
    
  )

print(dropout_prev_results)

cat("\n")

# =============================================================================
# CONVENTIONAL DROPOUT
# =============================================================================

cat("Estimating conventional dropout...\n\n")

dropout_rate_results <-
  
  survey::svyby(
    
    ~conventional_dropout,
    
    ~survey_name,
    
    design,
    
    survey::svymean,
    
    vartype = "ci",
    
    na.rm = TRUE
    
  ) |>
  
  dplyr::transmute(
    
    survey_name,
    
    Prevalence = conventional_dropout * 100,
    
    Lower_CI = ci_l * 100,
    
    Upper_CI = ci_u * 100
    
  )

print(dropout_rate_results)

cat("\n")
# =============================================================================
# EXPORT RESULTS
# =============================================================================

cat("Exporting descriptive analysis tables...\n\n")

openxlsx::write.xlsx(
  
  list(
    
    Zero_Dose_Prevalence = zero_dose_results,
    
    Dropout_Prevalence = dropout_prev_results,
    
    Conventional_Dropout = dropout_rate_results
    
  ),
  
  file = file.path(
    
    paths$tables,
    
    "09_descriptive_analysis.xlsx"
    
  ),
  
  overwrite = TRUE
  
)

write.csv(
  
  zero_dose_results,
  
  file.path(
    
    paths$tables,
    
    "09_zero_dose_prevalence.csv"
    
  ),
  
  row.names = FALSE
  
)

write.csv(
  
  dropout_prev_results,
  
  file.path(
    
    paths$tables,
    
    "09_dropout_prevalence.csv"
    
  ),
  
  row.names = FALSE
  
)

write.csv(
  
  dropout_rate_results,
  
  file.path(
    
    paths$tables,
    
    "09_conventional_dropout.csv"
    
  ),
  
  row.names = FALSE
  
)

cat("✓ Descriptive analysis tables exported.\n\n")

# =============================================================================
# FINAL VALIDATION
# =============================================================================

cat("Running validation...\n\n")

stopifnot(
  
  nrow(zero_dose_results) ==
    dplyr::n_distinct(analysis_data$survey_name),
  
  nrow(dropout_prev_results) ==
    dplyr::n_distinct(analysis_data$survey_name),
  
  nrow(dropout_rate_results) ==
    dplyr::n_distinct(analysis_data$survey_name)
  
)

cat("✓ Validation passed.\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

summary_tbl <-
  
  tibble(
    
    Metric = c(
      
      "Records",
      
      "Variables",
      
      "Survey rounds",
      
      "Date"
      
    ),
    
    Value = c(
      
      nrow(analysis_data),
      
      ncol(analysis_data),
      
      dplyr::n_distinct(
        analysis_data$survey_name
      ),
      
      as.character(Sys.Date())
      
    )
    
  )

cat("==============================================================\n")
cat("DESCRIPTIVE ANALYSIS SUMMARY\n")
cat("==============================================================\n\n")

print(summary_tbl)

cat("\n")

cat("=============================\n")
cat("ZERO-DOSE PREVALENCE (%)\n")
cat("=============================\n\n")

print(zero_dose_results)

cat("\n")

cat("=============================\n")
cat("DROPOUT PREVALENCE (%)\n")
cat("=============================\n\n")

print(dropout_prev_results)

cat("\n")

cat("=============================\n")
cat("CONVENTIONAL DROPOUT (%)\n")
cat("=============================\n\n")

print(dropout_rate_results)

cat("\n")

cat("Output files\n")
cat("------------\n")

cat(
  
  "Workbook               : ",
  
  file.path(
    
    paths$tables,
    
    "09_descriptive_analysis.xlsx"
    
  ),
  
  "\n"
  
)

cat(
  
  "Zero-dose prevalence   : ",
  
  file.path(
    
    paths$tables,
    
    "09_zero_dose_prevalence.csv"
    
  ),
  
  "\n"
  
)

cat(
  
  "Dropout prevalence     : ",
  
  file.path(
    
    paths$tables,
    
    "09_dropout_prevalence.csv"
    
  ),
  
  "\n"
  
)

cat(
  
  "Conventional dropout   : ",
  
  file.path(
    
    paths$tables,
    
    "09_conventional_dropout.csv"
    
  ),
  
  "\n\n"
  
)

cat("==============================================================\n")
cat("DESCRIPTIVE ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("==============================================================\n")