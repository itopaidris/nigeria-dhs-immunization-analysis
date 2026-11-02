# =============================================================================
# NIGERIA DHS IMMUNIZATION ANALYSIS PROJECT
# Script : 06_create_outcomes.R
# Purpose: Create Manuscript Outcome Variables
# Manuscript:
# Zero-Dose Prevalence and Immunization Dropout as Complementary
# Indicators of Immunization System Performance in Nigeria:
# Pooled NDHS 2013, 2018 and 2023–24
# Version: 6.0
# =============================================================================

source("~/nigeria-dhs-immunization-analysis/scripts/00_setup.R")

cat("\n")
cat("==============================================================\n")
cat("CREATE MANUSCRIPT OUTCOME VARIABLES\n")
cat("==============================================================\n\n")

# =============================================================================
# LOAD ANALYSIS POPULATION
# =============================================================================

analysis_data <-
  
  readRDS(
    
    file.path(
      
      paths$interim,
      
      "analysis_population.rds"
      
    )
    
  )

cat("✓ Analysis population loaded.\n\n")

# =============================================================================
# INITIAL DATASET SUMMARY
# =============================================================================

initial_summary <-
  
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

print(initial_summary)

cat("\n")

# =============================================================================
# VERIFY REQUIRED VARIABLES
# Manuscript-specific variables only
# =============================================================================

required_variables <- c(
  
  "child_id",
  
  "survey_name",
  
  "survey_period",
  
  "analysis_year",
  
  "sample_weight",
  
  "v001",
  
  "v021",
  
  "v022",
  
  "h3",     # Penta1 (DPT1)
  
  "h7"      # Penta3 (DPT3)
  
)

availability_tbl <-
  
  tibble(
    
    Variable = required_variables,
    
    Available =
      
      required_variables %in%
      
      names(analysis_data)
    
  )

print(availability_tbl)

assert_variables(
  
  analysis_data,
  
  required_variables
  
)

cat("\n✓ Required variables verified.\n\n")
# =============================================================================
# HELPER FUNCTION
# =============================================================================

vaccinated <- function(x){
  
  dplyr::case_when(
    
    x %in% c(1, 2, 3) ~ 1L,
    
    x == 0 ~ 0L,
    
    TRUE ~ NA_integer_
    
  )
  
}

cat("✓ Helper function created.\n\n")

# =============================================================================
# CREATE PENTAVALENT VACCINE INDICATORS
# =============================================================================

cat("Creating Pentavalent vaccine indicators...\n\n")

analysis_data <-
  
  analysis_data %>%
  
  mutate(
    
    penta1 = vaccinated(h3),
    
    penta3 = vaccinated(h7)
    
  )

cat("✓ Pentavalent vaccine indicators created.\n\n")

# =============================================================================
# CREATE MANUSCRIPT OUTCOME VARIABLES
# =============================================================================

cat("Creating manuscript outcome variables...\n\n")

analysis_data <-
  
  analysis_data %>%
  
  mutate(
    
    # ----------------------------------------------------------
    # OUTCOME 1
    # ZERO-DOSE PREVALENCE
    #
    # Definition:
    # Child did NOT receive Penta1
    #
    # Formula:
    # (No Penta1 / All children) × 100
    # ----------------------------------------------------------
    
    zero_dose = case_when(
      
      penta1 == 0 ~ 1L,
      
      penta1 == 1 ~ 0L,
      
      TRUE ~ NA_integer_
      
    ),
    
    # ----------------------------------------------------------
    # OUTCOME 2
    # DROPOUT PREVALENCE
    #
    # Definition:
    # Received Penta1 but NOT Penta3
    #
    # Denominator:
    # ALL children aged 12–23 months
    #
    # Formula:
    # (Penta1 yes & Penta3 no / All children) × 100
    # ----------------------------------------------------------
    
    dropout_prevalence = case_when(
      
      penta1 == 1 &
        penta3 == 0 ~ 1L,
      
      penta1 == 1 &
        penta3 == 1 ~ 0L,
      
      penta1 == 0 ~ 0L,
      
      TRUE ~ NA_integer_
      
    ),
    
    # ----------------------------------------------------------
    # OUTCOME 3
    # CONVENTIONAL IMMUNISATION DROPOUT
    #
    # Definition:
    # Among children receiving Penta1,
    # did not receive Penta3
    #
    # Denominator:
    # Penta1 recipients ONLY
    #
    # Formula:
    # (Penta1 yes & Penta3 no / Penta1 yes) × 100
    # ----------------------------------------------------------
    
    conventional_dropout = case_when(
      
      penta1 == 1 &
        penta3 == 0 ~ 1L,
      
      penta1 == 1 &
        penta3 == 1 ~ 0L,
      
      TRUE ~ NA_integer_
      
    )
    
  )

cat("✓ Manuscript outcome variables created.\n\n")
# =============================================================================
# VALIDATION
# =============================================================================

validation_tbl <-
  
  tibble(
    
    Variable = c(
      
      "zero_dose",
      
      "dropout_prevalence",
      
      "conventional_dropout"
      
    ),
    
    Missing = c(
      
      sum(is.na(analysis_data$zero_dose)),
      
      sum(is.na(analysis_data$dropout_prevalence)),
      
      sum(is.na(analysis_data$conventional_dropout))
      
    )
    
  )

print(validation_tbl)

cat("\n✓ Outcome validation completed.\n\n")

# =============================================================================
# OVERALL OUTCOME SUMMARY
# =============================================================================

outcome_summary <-
  
  tibble(
    
    Outcome = c(
      
      "Zero-dose prevalence",
      
      "Dropout prevalence",
      
      "Conventional dropout"
      
    ),
    
    Positive = c(
      
      sum(analysis_data$zero_dose == 1, na.rm = TRUE),
      
      sum(analysis_data$dropout_prevalence == 1, na.rm = TRUE),
      
      sum(analysis_data$conventional_dropout == 1, na.rm = TRUE)
      
    ),
    
    Percent = round(
      
      c(
        
        mean(analysis_data$zero_dose == 1, na.rm = TRUE),
        
        mean(analysis_data$dropout_prevalence == 1, na.rm = TRUE),
        
        mean(analysis_data$conventional_dropout == 1, na.rm = TRUE)
        
      ) * 100,
      
      1
      
    )
    
  )

print(outcome_summary)

cat("\n✓ Overall outcome summary created.\n\n")

# =============================================================================
# SURVEY-SPECIFIC OUTCOME SUMMARY
# =============================================================================

cat("Creating survey summaries...\n\n")

survey_summary <-
  
  analysis_data %>%
  
  group_by(
    
    survey_name
    
  ) %>%
  
  summarise(
    
    Records = n(),
    
    Zero_Dose_Prevalence = round(
      
      mean(
        
        zero_dose == 1,
        
        na.rm = TRUE
        
      ) * 100,
      
      1
      
    ),
    
    Dropout_Prevalence = round(
      
      mean(
        
        dropout_prevalence == 1,
        
        na.rm = TRUE
        
      ) * 100,
      
      1
      
    ),
    
    Conventional_Dropout = round(
      
      mean(
        
        conventional_dropout == 1,
        
        na.rm = TRUE
        
      ) * 100,
      
      1
      
    ),
    
    .groups = "drop"
    
  )

print(survey_summary)

cat("\n✓ Survey summaries created.\n\n")

# =============================================================================
# EXPORT QC WORKBOOK
# =============================================================================

export_workbook(
  
  sheets = list(
    
    "Dataset Summary" = initial_summary,
    
    "Variable Availability" = availability_tbl,
    
    "Validation" = validation_tbl,
    
    "Outcome Summary" = outcome_summary,
    
    "Survey Summary" = survey_summary
    
  ),
  
  file = "06_create_outcomes_report.xlsx"
  
)

cat("✓ QC workbook exported.\n\n")

# =============================================================================
# SAVE DATASET
# =============================================================================

saveRDS(
  
  analysis_data,
  
  file.path(
    
    paths$interim,
    
    "immunisation_outcomes.rds"
    
  )
  
)

cat("✓ Immunisation outcomes dataset saved.\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

final_summary <-
  
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

print(final_summary)

cat("\n")

cat(
  
  "Immunisation outcomes : ",
  
  file.path(
    
    paths$interim,
    
    "immunisation_outcomes.rds"
    
  ),
  
  "\n"
  
)

cat(
  
  "QC report             : ",
  
  file.path(
    
    paths$logs,
    
    "06_create_outcomes_report.xlsx"
    
  ),
  
  "\n\n"
  
)

cat("==============================================================\n")
cat("MANUSCRIPT OUTCOME VARIABLES CREATED SUCCESSFULLY\n")
cat("==============================================================\n")