# =============================================================================
# 10_equity_analysis.R
#
# Project : Nigeria DHS Immunization Analysis
# Purpose : Survey-weighted equity analysis
# Author  : Nuhu Idris Itopa
# Version : 5.0
# Date    : 31 October 2026
# =============================================================================

source("~/nigeria-dhs-immunization-analysis/scripts/00_setup.R")

cat("==============================================================\n")
cat("EQUITY ANALYSIS\n")
cat("==============================================================\n\n")

library(dplyr)
library(survey)
library(openxlsx)

options(survey.lonely.psu = "adjust")

# =============================================================================
# LOAD DATA
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

cat("\u2713 Final analysis dataset loaded.\n\n")

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
      dplyr::n_distinct(
        analysis_data$survey_name
      )
    )
  )
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
    "wealth_quintile",
    "maternal_education",
    "residence",
    "region",
    "anc_attendance"
  )
  
  validation <- tibble::tibble(
    Variable = required_variables,
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
  
  cat("\n✓ Required variables verified.\n\n")  
  # =============================================================================
  # SURVEY DESIGN
  # =============================================================================
  
  cat("Creating survey design...\n\n")
  
  survey_design <-
    
    survey::svydesign(
      
      ids = ~cluster,
      
      strata = ~strata,
      
      weights = ~sample_weight,
      
      data = analysis_data,
      
      nest = TRUE
      
    )
  
  cat("\u2713 Survey design created.\n\n")
  
  # =============================================================================
  # HELPER FUNCTION
  # =============================================================================
  
  convert_pct <- function(df, outcome){
    
    df %>%
      mutate(
        Prevalence = .data[[outcome]] * 100,
        Lower_CI = ci_l * 100,
        Upper_CI = ci_u * 100
      ) %>%
      select(
        1,
        Prevalence,
        Lower_CI,
        Upper_CI
      )
    
  }
  
  # =============================================================================
  # ZERO-DOSE EQUITY ANALYSIS
  # =============================================================================
  
  cat("Estimating zero-dose equity indicators...\n\n")
  
  zerodose_wealth <-
    survey::svyby(
      ~zero_dose,
      ~wealth_quintile,
      survey_design,
      survey::svymean,
      vartype="ci",
      na.rm=TRUE
    ) %>%
    convert_pct("zero_dose")
  
  zerodose_education <-
    survey::svyby(
      ~zero_dose,
      ~maternal_education,
      survey_design,
      survey::svymean,
      vartype="ci",
      na.rm=TRUE
    ) %>%
    convert_pct("zero_dose")
  
  zerodose_residence <-
    survey::svyby(
      ~zero_dose,
      ~residence,
      survey_design,
      survey::svymean,
      vartype="ci",
      na.rm=TRUE
    ) %>%
    convert_pct("zero_dose")
  
  zerodose_region <-
    survey::svyby(
      ~zero_dose,
      ~region,
      survey_design,
      survey::svymean,
      vartype="ci",
      na.rm=TRUE
    ) %>%
    convert_pct("zero_dose")
  
  zerodose_anc <-
    survey::svyby(
      ~zero_dose,
      ~anc_attendance,
      survey_design,
      survey::svymean,
      vartype="ci",
      na.rm=TRUE
    ) %>%
    convert_pct("zero_dose")
  
  cat("\u2713 Zero-dose equity analysis completed.\n\n")
  # =============================================================================
  # DROPOUT PREVALENCE EQUITY ANALYSIS
  # =============================================================================
  
  cat("Estimating dropout prevalence equity indicators...\n\n")
  
  dropout_wealth <-
    survey::svyby(
      ~dropout_prevalence,
      ~wealth_quintile,
      survey_design,
      survey::svymean,
      vartype = "ci",
      na.rm = TRUE
    ) %>%
    convert_pct("dropout_prevalence")
  
  dropout_education <-
    survey::svyby(
      ~dropout_prevalence,
      ~maternal_education,
      survey_design,
      survey::svymean,
      vartype = "ci",
      na.rm = TRUE
    ) %>%
    convert_pct("dropout_prevalence")
  
  dropout_residence <-
    survey::svyby(
      ~dropout_prevalence,
      ~residence,
      survey_design,
      survey::svymean,
      vartype = "ci",
      na.rm = TRUE
    ) %>%
    convert_pct("dropout_prevalence")
  
  dropout_region <-
    survey::svyby(
      ~dropout_prevalence,
      ~region,
      survey_design,
      survey::svymean,
      vartype = "ci",
      na.rm = TRUE
    ) %>%
    convert_pct("dropout_prevalence")
  
  dropout_anc <-
    survey::svyby(
      ~dropout_prevalence,
      ~anc_attendance,
      survey_design,
      survey::svymean,
      vartype = "ci",
      na.rm = TRUE
    ) %>%
    convert_pct("dropout_prevalence")
  
  cat("\u2713 Dropout prevalence equity analysis completed.\n\n")
  
  # =============================================================================
  # EXPORT RESULTS
  # =============================================================================
  
  cat("Exporting equity analysis tables...\n\n")
  
  # -----------------------------------------------------------------------------
  # Create Excel workbook
  # -----------------------------------------------------------------------------
  
  wb <- openxlsx::createWorkbook()
  
  tables <- list(
    
    zerodose_wealth      = zerodose_wealth,
    zerodose_education   = zerodose_education,
    zerodose_residence   = zerodose_residence,
    zerodose_region      = zerodose_region,
    zerodose_anc         = zerodose_anc,
    
    dropout_wealth       = dropout_wealth,
    dropout_education    = dropout_education,
    dropout_residence    = dropout_residence,
    dropout_region       = dropout_region,
    dropout_anc          = dropout_anc
    
  )
  
  # -----------------------------------------------------------------------------
  # Add worksheets
  # -----------------------------------------------------------------------------
  
  for (sheet in names(tables)) {
    
    openxlsx::addWorksheet(
      
      wb,
      
      sheetName = sheet
      
    )
    
    openxlsx::writeData(
      
      wb,
      
      sheet = sheet,
      
      x = tables[[sheet]]
      
    )
    
  }
  
  # -----------------------------------------------------------------------------
  # Save workbook
  # -----------------------------------------------------------------------------
  
  openxlsx::saveWorkbook(
    
    wb,
    
    here::here(
      
      "outputs",
      "tables",
      "10_equity_analysis.xlsx"
      
    ),
    
    overwrite = TRUE
    
  )
  
  # -----------------------------------------------------------------------------
  # Export CSV files
  # -----------------------------------------------------------------------------
  
  for (tbl in names(tables)) {
    
    readr::write_csv(
      
      tables[[tbl]],
      
      here::here(
        
        "outputs",
        "tables",
        paste0("10_", tbl, ".csv")
        
      )
      
    )
    
  }
  
  cat("✓ Excel workbook exported.\n")
  cat("✓ Individual CSV tables exported.\n\n")
  
  # -----------------------------------------------------------------------------
  # List exported files
  # -----------------------------------------------------------------------------
  
  exported_files <-
    
    list.files(
      
      here::here(
        
        "outputs",
        "tables"
        
      ),
      
      pattern = "^10_.*\\.(csv|xlsx)$",
      
      full.names = FALSE
      
    )
  
  cat("Files created\n")
  cat("-------------\n")
  
  cat(
    
    paste(exported_files, collapse = "\n"),
    
    "\n\n"
    
  )
  
  cat("✓ Equity analysis tables exported successfully.\n\n")  
  # =============================================================================
  # SUMMARY
  # =============================================================================
  
  cat("==============================================================\n")
  cat("EQUITY ANALYSIS SUMMARY\n")
  cat("==============================================================\n\n")
  
  summary_tbl <-
    tibble::tibble(
      Metric = c(
        "Records",
        "Variables",
        "Survey rounds",
        "Date"
      ),
      Value = c(
        nrow(analysis_data),
        ncol(analysis_data),
        dplyr::n_distinct(analysis_data$survey_name),
        as.character(Sys.Date())
      )
    )
  
  print(summary_tbl)
  
  cat("\nExample output:\n\n")
  
  print(zerodose_wealth)
  
  cat("\nOutput files\n")
  cat("------------\n")
  
  cat(
    "Workbook              : ",
    here::here(
      "outputs",
      "tables",
      "10_equity_analysis.xlsx"
    ),
    "\n"
  )
  
  cat(
    "CSV Tables            : outputs/tables/10_*.csv\n\n"
  )
  
  cat("==============================================================\n")
  cat("EQUITY ANALYSIS COMPLETED SUCCESSFULLY\n")
  cat("==============================================================\n")