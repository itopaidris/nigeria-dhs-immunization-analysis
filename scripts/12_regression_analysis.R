# =============================================================================
# 12_generate_tables.R
#
# Project : Nigeria DHS Immunization Analysis
# Purpose : Generate publication-ready manuscript tables
#
#           Zero-Dose Prevalence and Immunization Dropout as
#           Complementary Indicators of Immunization System
#           Performance in Nigeria:
#           Analysis of NDHS 2013–2024
#
# Author  : Nuhu Idris Itopa
# Version : 6.0
# Date    : 01 November 2026
# =============================================================================

source("~/nigeria-dhs-immunization-analysis/scripts/00_setup.R")

cat("==============================================================\n")
cat("GENERATE PUBLICATION TABLES\n")
cat("==============================================================\n\n")

library(dplyr)
library(tidyr)
library(survey)
library(srvyr)
library(openxlsx)
library(broom)
library(stringr)

options(survey.lonely.psu = "adjust")

# =============================================================================
# CREATE OUTPUT DIRECTORY
# =============================================================================

dir.create(
  here::here("outputs", "tables"),
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

print(
  tibble(
    Metric = c(
      "Records",
      "Variables",
      "Survey rounds"
    ),
    Value = c(
      nrow(analysis_data),
      ncol(analysis_data),
      n_distinct(analysis_data$survey_name)
    )
  )
)

# =============================================================================
# LOAD FITTED MODELS
# =============================================================================

cat("\nLoading fitted regression models...\n\n")

zero_model <-
  readRDS(
    here::here(
      "outputs",
      "models",
      "zero_dose_model.rds"
    )
  )

dropout_model <-
  readRDS(
    here::here(
      "outputs",
      "models",
      "dropout_model.rds"
    )
  )

cat("✓ Regression models loaded.\n\n")

# =============================================================================
# CREATE SURVEY DESIGN
# =============================================================================

cat("Creating survey design...\n\n")

survey_design <-
  
  analysis_data %>%
  
  as_survey_design(
    
    ids = cluster,
    
    strata = strata,
    
    weights = sample_weight,
    
    nest = TRUE
    
  )

cat("✓ Survey design created.\n\n")

# =============================================================================
# TABLE 1
# CHARACTERISTICS OF CHILDREN
# =============================================================================

cat("Generating Table 1...\n\n")

create_table1 <- function(variable, label){
  
  counts <-
    
    analysis_data %>%
    
    count(
      !!rlang::sym(variable),
      name = "n"
    )
  
  weighted <-
    
    survey_design %>%
    
    filter(
      !is.na(.data[[variable]])
    ) %>%
    
    group_by(
      !!rlang::sym(variable)
    ) %>%
    
    summarise(
      
      Percent = survey_mean(
        vartype = NULL,
        na.rm = TRUE
      )
      
    ) %>%
    
    mutate(
      
      Percent = round(
        Percent * 100,
        1
      )
      
    )
  
  left_join(
    
    counts,
    
    weighted,
    
    by = variable
    
  ) %>%
    
    transmute(
      
      Characteristic = label,
      
      Category = as.character(
        !!rlang::sym(variable)
      ),
      
      n,
      
      `Survey-weighted %` = Percent
      
    )
  
}

table1 <-
  
  bind_rows(
    
    create_table1(
      "survey_period",
      "Survey year"
    ),
    
    create_table1(
      "wealth_quintile",
      "Household wealth quintile"
    ),
    
    create_table1(
      "maternal_education",
      "Maternal education"
    ),
    
    create_table1(
      "residence",
      "Place of residence"
    ),
    
    create_table1(
      "region",
      "Geopolitical region"
    ),
    
    create_table1(
      "anc_attendance",
      "ANC attendance"
    )
    
  )

cat("✓ Table 1 generated.\n\n")
# =============================================================================
# TABLE 2A
# NATIONAL TRENDS
# =============================================================================

cat("Generating Table 2A...\n\n")

table2a <-
  
  survey_design %>%
  
  group_by(survey_period) %>%
  
  summarise(
    
    ZeroDose =
      survey_mean(
        zero_dose,
        vartype = "ci",
        na.rm = TRUE
      ),
    
    Dropout =
      survey_mean(
        dropout_prevalence,
        vartype = "ci",
        na.rm = TRUE
      ),
    
    Conventional =
      survey_mean(
        conventional_dropout,
        vartype = "ci",
        na.rm = TRUE
      )
    
  ) %>%
  
  mutate(
    
    `Survey year` = as.character(survey_period),
    
    `Zero-dose prevalence % (95% CI)` =
      sprintf(
        "%.1f (%.1f-%.1f)",
        ZeroDose * 100,
        ZeroDose_low * 100,
        ZeroDose_upp * 100
      ),
    
    `Dropout prevalence % (95% CI)` =
      sprintf(
        "%.1f (%.1f-%.1f)",
        Dropout * 100,
        Dropout_low * 100,
        Dropout_upp * 100
      ),
    
    `Conventional immunization dropout % (95% CI)` =
      sprintf(
        "%.1f (%.1f-%.1f)",
        Conventional * 100,
        Conventional_low * 100,
        Conventional_upp * 100
      )
    
  ) %>%
  
  select(
    `Survey year`,
    `Zero-dose prevalence % (95% CI)`,
    `Dropout prevalence % (95% CI)`,
    `Conventional immunization dropout % (95% CI)`
  )

cat("✓ Table 2A generated.\n\n")

# =============================================================================
# TABLE 2B
# SURVEY-WEIGHTED PREVALENCE BY CHARACTERISTICS
# =============================================================================

cat("Generating Table 2B...\n\n")

create_table2b <- function(variable, label) {
  
  survey_design %>%
    
    filter(
      !is.na(.data[[variable]])
    ) %>%
    
    group_by(
      !!rlang::sym(variable)
    ) %>%
    
    summarise(
      
      ZeroDose =
        survey_mean(
          zero_dose,
          vartype = NULL,
          na.rm = TRUE
        ),
      
      Dropout =
        survey_mean(
          dropout_prevalence,
          vartype = NULL,
          na.rm = TRUE
        )
      
    ) %>%
    
    transmute(
      
      Characteristic = label,
      
      Category =
        as.character(
          !!rlang::sym(variable)
        ),
      
      `Zero-dose (%)` =
        round(
          ZeroDose * 100,
          1
        ),
      
      `Dropout prevalence (%)` =
        round(
          Dropout * 100,
          1
        )
      
    )
  
}

table2b <-
  
  bind_rows(
    
    create_table2b(
      "residence",
      "Residence"
    ),
    
    create_table2b(
      "wealth_quintile",
      "Household wealth quintile"
    ),
    
    create_table2b(
      "maternal_education",
      "Maternal education"
    ),
    
    create_table2b(
      "anc_attendance",
      "ANC attendance"
    ),
    
    create_table2b(
      "region",
      "Geopolitical region"
    )
    
  )

cat("✓ Table 2B generated.\n\n")
# =============================================================================
# TABLE 3
# MULTIVARIABLE LOGISTIC REGRESSION
# =============================================================================

cat("Generating Table 3...\n\n")

extract_or <- function(model) {
  
  broom::tidy(
    model,
    conf.int = TRUE
  ) %>%
    
    filter(term != "(Intercept)") %>%
    
    mutate(
      
      AOR = exp(estimate),
      
      LCL = exp(conf.low),
      
      UCL = exp(conf.high),
      
      Result = sprintf(
        "%.2f (%.2f-%.2f)",
        AOR,
        LCL,
        UCL
      )
      
    ) %>%
    
    select(
      term,
      Result
    )
  
}

zero_results <- extract_or(zero_model)

dropout_results <- extract_or(dropout_model)

lookup <-
  
  tibble::tribble(
    
    ~term,                            ~Variable,                    ~Category,
    
    #----------------------------------------------------
    # Survey year
    # Reference = 2013
    #----------------------------------------------------
    
    "survey_year2018",                "Survey year",               "2018",
    "survey_year2023-24",             "Survey year",               "2023-24",
    
    #----------------------------------------------------
    # Household wealth quintile
    # Reference = Poorest
    #----------------------------------------------------
    
    "wealth_quintilePoorer",          "Household wealth quintile", "Poorer",
    "wealth_quintileMiddle",          "Household wealth quintile", "Middle",
    "wealth_quintileRicher",          "Household wealth quintile", "Richer",
    "wealth_quintileRichest",         "Household wealth quintile", "Richest",
    
    #----------------------------------------------------
    # Maternal education
    # Reference = No education
    #----------------------------------------------------
    
    "maternal_educationPrimary",      "Maternal education",        "Primary",
    "maternal_educationSecondary",    "Maternal education",        "Secondary",
    "maternal_educationHigher",       "Maternal education",        "Higher",
    
    #----------------------------------------------------
    # Place of residence
    # Reference = Urban
    #----------------------------------------------------
    
    "residenceRural",                 "Place of residence",        "Rural",
    
    #----------------------------------------------------
    # Geopolitical region
    # Reference = North Central
    #----------------------------------------------------
    
    "region2",                        "Geopolitical region",       "North East",
    "region3",                        "Geopolitical region",       "North West",
    "region4",                        "Geopolitical region",       "South East",
    "region5",                        "Geopolitical region",       "South South",
    "region6",                        "Geopolitical region",       "South West",
    
    #----------------------------------------------------
    # ANC attendance
    # Reference = 0 visits
    #----------------------------------------------------
    
    
    "anc_attendance1-3 visits", "ANC attendance", "1-3 visits",
    "anc_attendance4+ visits",  "ANC attendance", "4+ visits"
    )

table3 <-
  
  lookup %>%
  
  left_join(
    zero_results,
    by = "term"
  ) %>%
  
  rename(
    `Zero-dose AOR (95% CI)` = Result
  ) %>%
  
  left_join(
    dropout_results,
    by = "term"
  ) %>%
  
  rename(
    `Conventional Dropout AOR (95% CI)` = Result
  )

reference_rows <-
  
  tibble(
    
    Variable = c(
      "Survey year",
      "Household wealth quintile",
      "Maternal education",
      "Place of residence",
      "Geopolitical region",
      "ANC attendance"
    ),
    
    Category = c(
      "2013",
      "Poorest",
      "No education",
      "Urban",
      "North Central",
      "0 visits"
    ),
    
    `Zero-dose AOR (95% CI)` = "Reference",
    
    `Conventional Dropout AOR (95% CI)` = "Reference"
    
  )

table3 <-
  
  bind_rows(
    reference_rows,
    table3
  ) %>%
  
  mutate(
    
    Variable = factor(
      Variable,
      levels = c(
        "Survey year",
        "Household wealth quintile",
        "Maternal education",
        "Place of residence",
        "Geopolitical region",
        "ANC attendance"
      )
    )
    
  ) %>%
  
  arrange(
    Variable
  ) %>%
  
  mutate(
    Variable = as.character(Variable)
  )

cat("✓ Table 3 generated.\n\n")
# =============================================================================
# EXPORT MANUSCRIPT TABLES
# =============================================================================

cat("Exporting manuscript tables...\n\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Table1_Characteristics")
openxlsx::writeData(wb, "Table1_Characteristics", table1)

openxlsx::addWorksheet(wb, "Table2A_Trends")
openxlsx::writeData(wb, "Table2A_Trends", table2a)

openxlsx::addWorksheet(wb, "Table2B_Equity")
openxlsx::writeData(wb, "Table2B_Equity", table2b)

openxlsx::addWorksheet(wb, "Table3_Regression")
openxlsx::writeData(wb, "Table3_Regression", table3)

openxlsx::saveWorkbook(
  
  wb,
  
  here::here(
    "outputs",
    "tables",
    "Manuscript_Tables.xlsx"
  ),
  
  overwrite = TRUE
  
)

write.csv(
  table1,
  here::here("outputs","tables","Table1_Characteristics.csv"),
  row.names = FALSE
)

write.csv(
  table2a,
  here::here("outputs","tables","Table2A_National_Trends.csv"),
  row.names = FALSE
)

write.csv(
  table2b,
  here::here("outputs","tables","Table2B_Equity.csv"),
  row.names = FALSE
)

write.csv(
  table3,
  here::here("outputs","tables","Table3_Regression.csv"),
  row.names = FALSE
)

cat("✓ Manuscript tables exported.\n\n")
# =============================================================================
# SUMMARY
# =============================================================================

summary_table <-
  
  tibble(
    
    Table = c(
      "Table 1",
      "Table 2A",
      "Table 2B",
      "Table 3"
    ),
    
    Description = c(
      "Characteristics of study population",
      "National trends",
      "Equity analysis",
      "Multivariable logistic regression"
    ),
    
    Status = "Generated"
    
  )

cat("==============================================================\n")
cat("PUBLICATION TABLES SUMMARY\n")
cat("==============================================================\n\n")

print(summary_table)

cat("\nOutput folder\n")
cat("-------------\n")

cat(
  here::here(
    "outputs",
    "tables"
  ),
  "\n\n"
)

cat("Files created\n")
cat("-------------\n")

cat("• Manuscript_Tables.xlsx\n")
cat("• Table1_Characteristics.csv\n")
cat("• Table2A_National_Trends.csv\n")
cat("• Table2B_Equity.csv\n")
cat("• Table3_Regression.csv\n\n")

cat("==============================================================\n")
cat("PUBLICATION TABLES GENERATED SUCCESSFULLY\n")
cat("==============================================================\n")
