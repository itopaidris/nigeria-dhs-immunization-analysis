# =============================================================================
# NIGERIA DHS IMMUNIZATION ANALYSIS PROJECT
#
# Script  : 08_prepare_analysis_dataset.R
# Purpose : Validate, Document and Freeze Final Analysis Dataset
#
# Manuscript:
# Zero-Dose Prevalence and Immunization Dropout as Complementary
# Indicators of Immunization System Performance in Nigeria:
# Analysis of NDHS 2013, 2018 and 2023–24
#
# Author  : Nuhu Idris Itopa
# Version : 8.0
# Date    : 02 November 2026
# =============================================================================

# =============================================================================
# PROJECT SETUP
# =============================================================================

source("~/nigeria-dhs-immunization-analysis/scripts/00_setup.R")

cat("\n")
cat("==============================================================\n")
cat("VALIDATE FINAL MANUSCRIPT ANALYSIS DATASET\n")
cat("==============================================================\n\n")

library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(openxlsx)

# =============================================================================
# CREATE OUTPUT DIRECTORIES
# =============================================================================

dir.create(
  here::here("outputs", "qc"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

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

# =============================================================================
# HARMONISE FACTOR VARIABLES
# =============================================================================

cat("Checking categorical variables...\n\n")

# -----------------------------------------------------------------------------
# Survey period
# -----------------------------------------------------------------------------

if ("survey_period" %in% names(analysis_data)) {
  
  analysis_data$survey_period <-
    
    factor(
      
      as.character(analysis_data$survey_period),
      
      levels = c(
        "2013",
        "2018",
        "2023-24"
      )
      
    )
  
}

# -----------------------------------------------------------------------------
# Residence
# -----------------------------------------------------------------------------

if ("residence" %in% names(analysis_data)) {
  
  analysis_data$residence <-
    
    factor(
      
      as.character(analysis_data$residence),
      
      levels = c(
        "Urban",
        "Rural"
      )
      
    )
  
}

# -----------------------------------------------------------------------------
# Wealth quintile
# -----------------------------------------------------------------------------

if ("wealth_quintile" %in% names(analysis_data)) {
  
  analysis_data$wealth_quintile <-
    
    factor(
      
      as.character(analysis_data$wealth_quintile),
      
      levels = c(
        "Poorest",
        "Poorer",
        "Middle",
        "Richer",
        "Richest"
      )
      
    )
  
}

# -----------------------------------------------------------------------------
# Maternal education
# -----------------------------------------------------------------------------

if ("maternal_education" %in% names(analysis_data)) {
  
  analysis_data$maternal_education <-
    
    factor(
      
      as.character(analysis_data$maternal_education),
      
      levels = c(
        "No education",
        "Primary",
        "Secondary",
        "Higher"
      )
      
    )
  
}

# -----------------------------------------------------------------------------
# Geopolitical region
# -----------------------------------------------------------------------------

if ("region" %in% names(analysis_data)) {
  
  region_values <- trimws(as.character(analysis_data$region))
  
  expected_levels <- c(
    "North Central",
    "North East",
    "North West",
    "South East",
    "South South",
    "South West"
  )
  
  # Convert numeric region codes to labels
  if (all(region_values %in% c("1", "2", "3", "4", "5", "6", NA))) {
    
    analysis_data$region <-
      factor(
        region_values,
        levels = c("1", "2", "3", "4", "5", "6"),
        labels = expected_levels
      )
    
  } else {
    
    # Preserve labelled regions while enforcing level order
    analysis_data$region <-
      factor(
        region_values,
        levels = expected_levels
      )
    
  }
  
}
# -----------------------------------------------------------------------------
# ANC attendance
# -----------------------------------------------------------------------------

if ("anc_attendance" %in% names(analysis_data)) {
  
  analysis_data$anc_attendance <-
    
    factor(
      
      trimws(as.character(analysis_data$anc_attendance)),
      
      levels = c(
        "None",
        "1-3 visits",
        "4+ visits"
      )
      
    )
  
}

cat("✓ Factor variables harmonised.\n\n")

# =============================================================================
# VERIFY FACTOR LEVELS
# =============================================================================

cat("Factor levels after harmonisation\n")
cat("----------------------------------------\n\n")

cat("Region\n")
print(levels(analysis_data$region))
cat("\n")

cat("ANC attendance\n")
print(levels(analysis_data$anc_attendance))
cat("\n")

cat("Survey period\n")
print(levels(analysis_data$survey_period))
cat("\n")

cat("Residence\n")
print(levels(analysis_data$residence))
cat("\n")

cat("Wealth quintile\n")
print(levels(analysis_data$wealth_quintile))
cat("\n")

cat("Maternal education\n")
print(levels(analysis_data$maternal_education))
cat("\n")
# =============================================================================
# DATASET SUMMARY
# =============================================================================

dataset_summary <-
  
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
    
  )

print(dataset_summary)

cat("\n")

# =============================================================================
# REQUIRED VARIABLES
# =============================================================================

required_variables <- c(
  
  "child_id",
  "survey_name",
  "survey_period",
  
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

availability_tbl <-
  
  tibble(
    
    Variable = required_variables,
    
    Available =
      
      required_variables %in%
      
      names(analysis_data)
    
  )

print(availability_tbl)

cat("\n")

if(any(!availability_tbl$Available)){
  
  stop(
    
    "Required manuscript variables are missing."
    
  )
  
}

cat("✓ Required variables verified.\n\n")

# =============================================================================
# END OF PART 1
# =============================================================================
# =============================================================================
# DATASET SUMMARY
# =============================================================================

dataset_summary <-
  
  tibble(
    
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

cat("Dataset summary\n")
cat("-----------------------------\n")

print(dataset_summary)

cat("\n")

# =============================================================================
# REQUIRED MANUSCRIPT VARIABLES
# =============================================================================

required_variables <- c(
  
  # Study identifiers
  "child_id",
  "survey_name",
  "survey_period",
  "analysis_year",
  
  # Survey design
  "sample_weight",
  "cluster",
  "strata",
  
  # Outcomes
  "zero_dose",
  "dropout_prevalence",
  "conventional_dropout",
  
  # Child characteristics
  "child_age_months",
  "child_age_group",
  "child_sex",
  
  # Maternal characteristics
  "maternal_education",
  "anc_attendance",
  
  # Household characteristics
  "wealth_quintile",
  "residence",
  "region"
  
)

# =============================================================================
# VERIFY REQUIRED VARIABLES
# =============================================================================

cat("Checking required variables...\n\n")

availability_tbl <-
  
  tibble(
    
    Variable = required_variables,
    
    Available =
      required_variables %in%
      names(analysis_data)
    
  )

print(availability_tbl)

cat("\n")

missing_variables <-
  
  availability_tbl %>%
  
  filter(!Available)

if(nrow(missing_variables) > 0){
  
  stop(
    "Missing required variables:\n",
    paste(missing_variables$Variable, collapse = ", ")
  )
  
}

cat("✓ All required variables verified.\n\n")

# =============================================================================
# DUPLICATE RECORDS
# =============================================================================

cat("Checking duplicate records...\n\n")

duplicate_child_ids <-
  
  analysis_data %>%
  
  count(
    survey_name,
    child_id,
    name = "Frequency"
  ) %>%
  
  filter(Frequency > 1)

duplicate_rows <-
  
  sum(
    duplicated(analysis_data)
  )

validation_summary <-
  
  tibble(
    
    Metric = c(
      "Repeated child IDs (within survey)",
      "Duplicate rows"
    ),
    
    Value = c(
      nrow(duplicate_child_ids),
      duplicate_rows
    )
    
  )

print(validation_summary)

cat("\n")

# -----------------------------------------------------------------------------
# Child IDs
# -----------------------------------------------------------------------------

if (nrow(duplicate_child_ids) == 0) {
  
  cat("✓ No repeated child IDs detected within survey rounds.\n")
  
} else {
  
  cat(
    "ℹ ",
    nrow(duplicate_child_ids),
    " repeated child IDs detected within survey rounds.\n",
    sep = ""
  )
  
  cat(
    "  This may be expected where a mother contributes more than one eligible child (e.g., multiple births).\n"
  )
  
}

# -----------------------------------------------------------------------------
# Duplicate rows
# -----------------------------------------------------------------------------

if (duplicate_rows == 0) {
  
  cat("✓ No duplicate rows detected.\n\n")
  
} else {
  
  cat(
    "⚠ ",
    duplicate_rows,
    " duplicate rows detected. Review before analysis.\n\n",
    sep = ""
  )
  
}
# =============================================================================
# VARIABLE CLASSES
# =============================================================================

cat("Checking variable classes...\n\n")

variable_classes <-
  
  tibble(
    
    Variable = names(analysis_data),
    
    Class =
      purrr::map_chr(
        analysis_data,
        ~class(.x)[1]
      )
    
  )

print(variable_classes)

cat("\n✓ Variable classes documented.\n\n")

# =============================================================================
# MISSING VALUES
# =============================================================================

cat("Assessing missing values...\n\n")

missing_summary <-
  
  tibble(
    
    Variable = names(analysis_data),
    
    Missing =
      purrr::map_int(
        analysis_data,
        ~sum(is.na(.x))
      )
    
  ) %>%
  
  mutate(
    
    Percent =
      round(
        Missing /
          nrow(analysis_data) *
          100,
        2
      )
    
  ) %>%
  
  arrange(desc(Missing))

print(missing_summary)

cat("\n✓ Missing-value assessment completed.\n\n")

# =============================================================================
# FACTOR LEVELS
# =============================================================================

cat("Checking factor levels...\n\n")

factor_variables <-
  
  names(
    
    analysis_data %>%
      
      select(where(is.factor))
    
  )

if(length(factor_variables)==0){
  
  cat("No factor variables detected.\n\n")
  
}else{
  
  for(v in factor_variables){
    
    cat("----------------------------------------\n")
    
    cat(v,"\n")
    
    cat("----------------------------------------\n")
    
    print(levels(analysis_data[[v]]))
    
    cat("\n")
    
  }
  
}

cat("✓ Factor-level validation completed.\n\n")
# =============================================================================
# SURVEY DISTRIBUTION
# =============================================================================

cat("Summarising survey distribution...\n\n")

survey_distribution <-
  
  analysis_data %>%
  
  count(
    survey_name,
    name = "Records"
  ) %>%
  
  mutate(
    
    Percent =
      round(
        Records /
          sum(Records) *
          100,
        1
      )
    
  )

print(survey_distribution)

cat("\n✓ Survey distribution completed.\n\n")

# =============================================================================
# VARIABLE CODEBOOK
# =============================================================================

cat("Generating variable codebook...\n\n")

variable_codebook <-
  
  tibble(
    
    Variable = names(analysis_data),
    
    Class =
      purrr::map_chr(
        analysis_data,
        ~ class(.x)[1]
      ),
    
    Missing =
      purrr::map_int(
        analysis_data,
        ~ sum(is.na(.x))
      ),
    
    Missing_Percent =
      round(
        purrr::map_dbl(
          analysis_data,
          ~ mean(is.na(.x))
        ) * 100,
        2
      )
    
  )

cat("✓ Variable codebook generated.\n\n")

# =============================================================================
# EXPORT DATASETS
# =============================================================================

cat("Exporting validated datasets...\n\n")

dir.create(
  here::here("logs"),
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  
  analysis_data,
  
  here::here(
    "data",
    "processed",
    "final_analysis_dataset.rds"
  )
  
)

write.csv(
  
  analysis_data,
  
  here::here(
    "data",
    "processed",
    "final_analysis_dataset.csv"
  ),
  
  row.names = FALSE
  
)

cat("✓ Final dataset exported.\n\n")

# =============================================================================
# CREATE QC WORKBOOK
# =============================================================================

cat("Creating QC workbook...\n\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Dataset Summary")
openxlsx::writeData(
  wb,
  "Dataset Summary",
  dataset_summary
)

openxlsx::addWorksheet(wb, "Variable Availability")
openxlsx::writeData(
  wb,
  "Variable Availability",
  availability_tbl
)

openxlsx::addWorksheet(wb, "Validation")
openxlsx::writeData(
  wb,
  "Validation",
  validation_summary
)

openxlsx::addWorksheet(wb, "Missing Values")
openxlsx::writeData(
  wb,
  "Missing Values",
  missing_summary
)

openxlsx::addWorksheet(wb, "Variable Classes")
openxlsx::writeData(
  wb,
  "Variable Classes",
  variable_classes
)

openxlsx::addWorksheet(wb, "Survey Distribution")
openxlsx::writeData(
  wb,
  "Survey Distribution",
  survey_distribution
)

openxlsx::addWorksheet(wb, "Variable Codebook")
openxlsx::writeData(
  wb,
  "Variable Codebook",
  variable_codebook
)

qc_file <-
  
  here::here(
    "logs",
    "08_prepare_analysis_dataset_QC.xlsx"
  )

openxlsx::saveWorkbook(
  
  wb,
  
  qc_file,
  
  overwrite = TRUE
  
)

cat("✓ QC workbook exported.\n\n")

# =============================================================================
# FINAL ASSERTIONS
# =============================================================================

stopifnot(
  
  nrow(analysis_data) > 0,
  
  ncol(analysis_data) > 0,
  
  all(required_variables %in% names(analysis_data))
  
)

cat("✓ Final validation checks passed.\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

summary_table <-
  
  tibble(
    
    Check = c(
      
      "Dataset loaded",
      "Required variables",
      "Duplicate check",
      "Missing-value assessment",
      "Variable classes",
      "Factor validation",
      "Survey distribution",
      "QC workbook",
      "Final dataset export"
      
    ),
    
    Status = "Completed"
    
  )

cat("==============================================================\n")
cat("FINAL DATASET VALIDATION SUMMARY\n")
cat("==============================================================\n\n")

print(summary_table)

cat("\nOutput files\n")
cat("------------\n")

cat(
  here::here(
    "data",
    "processed",
    "final_analysis_dataset.rds"
  ),
  "\n"
)

cat(
  here::here(
    "data",
    "processed",
    "final_analysis_dataset.csv"
  ),
  "\n"
)

cat(
  qc_file,
  "\n\n"
)

cat("==============================================================\n")
cat("FINAL MANUSCRIPT DATASET VALIDATED SUCCESSFULLY\n")
cat("==============================================================\n")