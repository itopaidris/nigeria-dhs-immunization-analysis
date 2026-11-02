# =============================================================================
# NIGERIA DHS IMMUNIZATION ANALYSIS PROJECT
# Script : 07_create_covariates.R
# Purpose: Create Manuscript Covariates
# Version: 5.0
# =============================================================================

source("~/nigeria-dhs-immunization-analysis/scripts/00_setup.R")

cat("\n")
cat("==============================================================\n")
cat("PREPARING FINAL MANUSCRIPT ANALYSIS DATASET\n")
cat("==============================================================\n\n")

# =============================================================================
# LOAD MANUSCRIPT OUTCOME DATASET
# =============================================================================

analysis_data <-
  
  readRDS(
    
    file.path(
      
      paths$interim,
      
      "immunisation_outcomes.rds"
      
    )
    
  )

cat("✓ Covariate dataset loaded.\n\n")

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
# VARIABLES REQUIRED FOR THIS MANUSCRIPT
# =============================================================================

required_variables <- c(
  
  #------------------------------------------------------------
  # Identification
  #------------------------------------------------------------
  
  "child_id",
  "survey_name",
  "survey_period",
  "analysis_year",
  "sample_weight",
  
  "v001",
  "v021",
  "v022",
  
  #------------------------------------------------------------
  # Outcomes
  #------------------------------------------------------------
  
  "zero_dose",
  "dropout_prevalence",
  "conventional_dropout",
  
  #------------------------------------------------------------
  # Child variables
  #------------------------------------------------------------
  
  "b19",
  "b4",
  "bord",
  
  #------------------------------------------------------------
  # Maternal variables
  #------------------------------------------------------------
  
  "v012",
  "v106",
  "v501",
  
  #------------------------------------------------------------
  # Household variables
  #------------------------------------------------------------
  
  "v024",
  "v025",
  "v190",
  
  #------------------------------------------------------------
  # Health service variables
  #------------------------------------------------------------
  
  "m14",
  "m15",
  
  #------------------------------------------------------------
  # Survey weight
  #------------------------------------------------------------
  
  "v005"
  
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
# CREATE MANUSCRIPT COVARIATES
# =============================================================================

cat("Creating manuscript covariates...\n\n")

analysis_data <-
  
  analysis_data %>%
  
  mutate(
    
    # ----------------------------------------------------------
    # Survey year
    # ----------------------------------------------------------
    
    survey_year = factor(
      analysis_year,
      levels = c(2013, 2018, 2024),
      labels = c("2013", "2018", "2023-24")
    ),
    
    # ----------------------------------------------------------
    # Child age (months)
    # ----------------------------------------------------------
    
    child_age_months = b19,
    
    child_age_group = case_when(
      b19 >= 12 & b19 <= 17 ~ "12-17",
      b19 >= 18 & b19 <= 23 ~ "18-23",
      TRUE ~ NA_character_
    ),
    
    # ----------------------------------------------------------
    # Child sex
    # ----------------------------------------------------------
    
    child_sex = factor(
      b4,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    # ----------------------------------------------------------
    # Maternal education
    # ----------------------------------------------------------
    
    maternal_education = factor(
      v106,
      levels = c(0, 1, 2, 3),
      labels = c(
        "None",
        "Primary",
        "Secondary",
        "Higher"
      )
    ),
    
    # ----------------------------------------------------------
    # ANC attendance
    # Number of ANC visits (m14)
    # ----------------------------------------------------------
    
    anc_attendance = case_when(
      is.na(m14) ~ NA_character_,
      m14 == 0 ~ "None",
      m14 >= 1 & m14 <= 3 ~ "1-3 visits",
      m14 >= 4 ~ "4+ visits",
      TRUE ~ NA_character_
    ),
    
    # ----------------------------------------------------------
    # Household wealth
    # ----------------------------------------------------------
    
    wealth_quintile = factor(
      v190,
      levels = 1:5,
      labels = c(
        "Poorest",
        "Poorer",
        "Middle",
        "Richer",
        "Richest"
      )
    ),
    
    # ----------------------------------------------------------
    # Residence
    # ----------------------------------------------------------
    
    residence = factor(
      v025,
      levels = c(1, 2),
      labels = c(
        "Urban",
        "Rural"
      )
    ),
    
    # ----------------------------------------------------------
    # Region
    # ----------------------------------------------------------
    
    region = as.integer(v024),
    
     # ----------------------------------------------------------
    # Survey design variables
    # ----------------------------------------------------------
    
    cluster = v001,
    
    strata = v022
    
  )

cat("✓ Covariates created.\n\n")

# =============================================================================
# VALIDATE REQUIRED VARIABLES
# =============================================================================

cat("Selecting final manuscript variables...\n\n")

required_variables <- c(
  
  # IDs
  "child_id",
  "survey_name",
  "survey_period",
  "analysis_year",
  "survey_year",
  
  # Survey design
  "sample_weight",
  "cluster",
  "strata",
  
  # Outcomes
  "zero_dose",
  "dropout_prevalence",
  "conventional_dropout",
  
  # Child
  "child_age_months",
  "child_age_group",
  "child_sex",
  
  # Maternal
  "maternal_education",
  "anc_attendance",
  
  # Household
  "wealth_quintile",
  "residence",
  "region"
  
)

availability_tbl <-
  
  tibble(
    
    Variable = required_variables,
    
    Available = required_variables %in%
      names(analysis_data)
    
  )

print(availability_tbl)

cat("\n")

cat("Variables requested :", length(required_variables), "\n")
cat("Variables available :", sum(availability_tbl$Available), "\n")
cat("Variables missing   :", sum(!availability_tbl$Available), "\n\n")

if (any(!availability_tbl$Available)) {
  
  cat("Missing variables:\n")
  
  print(
    availability_tbl %>%
      filter(!Available)
  )
  
  stop("Required variables missing.")
  
}

cat("✓ All required variables available.\n\n")
# =============================================================================
# CREATE FINAL ANALYSIS DATASET
# =============================================================================

final_analysis_dataset <-
  
  analysis_data %>%
  
  select(
    
    all_of(required_variables)
    
  )

cat("✓ Final analysis dataset created.\n\n")

# =============================================================================
# DATASET SUMMARY
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
      
      nrow(final_analysis_dataset),
      
      ncol(final_analysis_dataset),
      
      n_distinct(final_analysis_dataset$survey_name),
      
      as.character(Sys.Date())
      
    )
    
  )

print(summary_tbl)

cat("\n")

# =============================================================================
# EXPORT QC REPORT
# =============================================================================

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Summary")
openxlsx::writeData(wb, "Summary", summary_tbl)

openxlsx::addWorksheet(wb, "Variable_Check")
openxlsx::writeData(wb, "Variable_Check", availability_tbl)

qc_file <-
  
  file.path(
    
    paths$logs,
    
    "07_final_analysis_dataset_report.xlsx"
    
  )

openxlsx::saveWorkbook(
  
  wb,
  
  qc_file,
  
  overwrite = TRUE
  
)

cat("✓ QC workbook exported.\n\n")

# =============================================================================
# SAVE FINAL ANALYSIS DATASET
# =============================================================================

output_file <-
  
  file.path(
    
    paths$processed,
    
    "final_analysis_dataset.rds"
    
  )

saveRDS(
  
  final_analysis_dataset,
  
  output_file
  
)

cat("✓ Final analysis dataset saved.\n\n")

cat("Final analysis dataset :", output_file, "\n")
cat("QC report             :", qc_file, "\n\n")

cat("==============================================================\n")
cat("FINAL MANUSCRIPT ANALYSIS DATASET CREATED SUCCESSFULLY\n")
cat("==============================================================\n")