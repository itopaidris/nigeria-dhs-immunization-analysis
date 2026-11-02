# =============================================================================
# LOAD FINAL ANALYSIS DATASET
# =============================================================================

cat("Loading final analysis dataset...\n\n")

analysis_data <-
  readRDS(
    here::here(
      "data",
      "processed",
      "final_analysis_dataset.rds"
    )
  )

cat("✓ Final analysis dataset loaded.\n\n")

print(
  tibble::tibble(
    Metric = c(
      "Records",
      "Variables",
      "Survey rounds"
    ),
    Value = c(
      nrow(analysis_data),
      ncol(analysis_data),
      dplyr::n_distinct(analysis_data$survey_name)
    )
  )
)

# =============================================================================
# MODEL VARIABLES
# =============================================================================

model_variables <- c(
  "zero_dose",
  "dropout_prevalence",
  "survey_year",
  "wealth_quintile",
  "maternal_education",
  "residence",
  "region",
  "anc_attendance"
)
# =============================================================================
# REQUIRED VARIABLES
# =============================================================================

required_variables <- c(
  "survey_name",
  "sample_weight",
  "cluster",
  "strata",
  "zero_dose",
  "dropout_prevalence",
  "conventional_dropout",
  "wealth_quintile",
  "maternal_education",
  "residence",
  "region",
  "anc_attendance"
)

validation <- tibble::tibble(
  Variable  = required_variables,
  Available = required_variables %in% names(analysis_data)
)

print(validation)

if (any(!validation$Available)) {
  
  stop(
    "Missing variables: ",
    paste(
      validation$Variable[!validation$Available],
      collapse = ", "
    )
  )
  
}
# =============================================================================
# CREATE SURVEY YEAR VARIABLE
# =============================================================================

analysis_data <- analysis_data %>%
  mutate(
    survey_year = case_when(
      survey_name == "NDHS 2013"    ~ "2013",
      survey_name == "NDHS 2018"    ~ "2018",
      survey_name == "NDHS 2023-24" ~ "2024",
      TRUE                          ~ NA_character_
    ),
    survey_year = factor(
      survey_year,
      levels = c("2013", "2018", "2024")
    )
  )

# =============================================================================
# VALIDATE SURVEY YEAR
# =============================================================================

if (any(is.na(analysis_data$survey_year))) {
  
  stop(
    "survey_year could not be created for one or more observations. ",
    "Please check the values of survey_name."
  )
  
}

cat("\n✓ Required variables verified.\n")
cat("✓ survey_year successfully created.\n\n")
# =============================================================================
# CREATE LABELLED FACTOR VARIABLES
# =============================================================================

cat("Creating labelled factor variables...\n\n")

model_data <-
  analysis_data %>%
  mutate(
    
    # -------------------------------------------------------------------------
    # Survey year
    # -------------------------------------------------------------------------
    survey_year = factor(
      as.character(survey_year),
      levels = c("2013", "2018", "2024")
    ),
    
    # -------------------------------------------------------------------------
    # Wealth quintile
    # -------------------------------------------------------------------------
    wealth_quintile = factor(
      as.character(wealth_quintile),
      levels = c(
        "Poorest",
        "Poorer",
        "Middle",
        "Richer",
        "Richest"
      )
    ),
    
    # -------------------------------------------------------------------------
    # Maternal education
    # -------------------------------------------------------------------------
    maternal_education = factor(
      as.character(maternal_education),
      levels = c(
        "No education",
        "Primary",
        "Secondary",
        "Higher"
      )
    ),
    
    # -------------------------------------------------------------------------
    # Residence
    # -------------------------------------------------------------------------
    residence = factor(
      as.character(residence),
      levels = c(
        "Urban",
        "Rural"
      )
    ),
    
    # -------------------------------------------------------------------------
    # Region
    # -------------------------------------------------------------------------
    region = factor(
      as.character(region),
      levels = c(
        "North Central",
        "North East",
        "North West",
        "South East",
        "South South",
        "South West"
      )
    ),
    
    # -------------------------------------------------------------------------
    # ANC attendance
    # -------------------------------------------------------------------------
    anc_attendance = factor(
      as.character(anc_attendance),
      levels = c(
        "None",
        "1-3 visits",
        "4+ visits"
      )
    )
    
  )

cat("✓ Factor variables created.\n\n")
# =============================================================================
# CHECK MISSING VALUES IN MODEL VARIABLES
# =============================================================================

model_variables <- c(
  "zero_dose",
  "dropout_prevalence",
  "survey_year",
  "wealth_quintile",
  "maternal_education",
  "residence",
  "region",
  "anc_attendance"
)

cat("Missing values by model variable:\n\n")

print(
  sapply(
    model_data[model_variables],
    function(x) sum(is.na(x))
  )
)

cat("\n")
# =============================================================================
# SET REFERENCE CATEGORIES
# =============================================================================

cat("Setting reference categories...\n\n")

model_data$survey_year <-
  stats::relevel(
    model_data$survey_year,
    ref = "2013"
  )

model_data$wealth_quintile <-
  stats::relevel(
    model_data$wealth_quintile,
    ref = "Richest"
  )

model_data$maternal_education <-
  stats::relevel(
    model_data$maternal_education,
    ref = "Higher"
  )

model_data$residence <-
  stats::relevel(
    model_data$residence,
    ref = "Urban"
  )

model_data$region <-
  stats::relevel(
    model_data$region,
    ref = "South East"
  )

model_data$anc_attendance <-
  stats::relevel(
    model_data$anc_attendance,
    ref = "4+ visits"
  )

cat("✓ Reference categories set.\n\n")

# =============================================================================
# VALIDATE MODEL DATASET
# =============================================================================

cat("Validating model dataset...\n\n")

model_variables <- c(
  "zero_dose",
  "dropout_prevalence",
  "survey_year",
  "wealth_quintile",
  "maternal_education",
  "residence",
  "region",
  "anc_attendance"
)

# Overall validation metrics
model_missing_values <- sum(
  is.na(model_data[, model_variables])
)

complete_model_records <- sum(
  complete.cases(model_data[, model_variables])
)

validation_summary <-
  tibble::tibble(
    Metric = c(
      "Records",
      "Variables",
      "Survey rounds",
      "Missing values across regression variables",
      "Complete cases for regression"
    ),
    Value = c(
      nrow(model_data),
      ncol(model_data),
      dplyr::n_distinct(model_data$survey_name),
      model_missing_values,
      complete_model_records
    )
  )

print(validation_summary)

# Missing values by variable
cat("\nMissing values by model variable:\n\n")

print(
  sapply(
    model_data[model_variables],
    function(x) sum(is.na(x))
  )
)

cat(
  "\nComplete cases for regression:\n",
  complete_model_records,
  "of",
  nrow(model_data),
  "\n\n"
)

# Factor level verification
cat("Checking factor levels...\n\n")

cat("Survey year:\n")
print(levels(model_data$survey_year))

cat("\nWealth quintile:\n")
print(levels(model_data$wealth_quintile))

cat("\nMaternal education:\n")
print(levels(model_data$maternal_education))

cat("\nResidence:\n")
print(levels(model_data$residence))

cat("\nRegion:\n")
print(levels(model_data$region))

cat("\nANC attendance:\n")
print(levels(model_data$anc_attendance))

cat("\n✓ Validation completed.\n\n")

# =============================================================================
# SAVE MODEL DATASET
# =============================================================================

cat("Saving regression-ready dataset...\n\n")

saveRDS(
  
  model_data,
  
  here::here(
    "data",
    "processed",
    "model_analysis_dataset.rds"
  )
  
)

cat("✓ Model dataset saved.\n\n")

# =============================================================================
# EXPORT VALIDATION REPORT
# =============================================================================

write.csv(
  
  validation_summary,
  
  here::here(
    "outputs",
    "tables",
    "11_model_dataset_validation.csv"
  ),
  
  row.names = FALSE
  
)

# =============================================================================
# SUMMARY
# =============================================================================
cat("==============================================================\n")
cat("MODEL DATASET SUMMARY\n")
cat("==============================================================\n\n")

print(validation_summary)

cat("\nReference Categories\n")
cat("--------------------\n")
cat("Survey year          : 2013\n")
cat("Wealth quintile      : Richest\n")
cat("Maternal education   : Higher\n")
cat("Residence            : Urban\n")
cat("Region               : South East\n")
cat("ANC attendance       : 4+ visits\n\n")

cat("Output files\n")
cat("------------\n")

cat(
  "Model dataset        : ",
  here::here(
    "data",
    "processed",
    "model_analysis_dataset.rds"
  ),
  "\n"
)

cat(
  "Validation report    : ",
  here::here(
    "outputs",
    "tables",
    "11_model_dataset_validation.csv"
  ),
  "\n\n"
)

cat("==============================================================\n")
cat("MODEL DATASET PREPARED SUCCESSFULLY\n")
cat("==============================================================\n")