# =============================================================================
# Project : Nigeria DHS Immunization Analysis (2013–2024)
# Script  : 04_define_population.R
# Purpose : Define the analytical population for all downstream analyses
# Author  : Nuhu Itopa Idris
# Version : 5.0
# Updated : 2026-10-26
# =============================================================================

# =============================================================================
# INITIALISE PROJECT
# =============================================================================

source(
  here::here(
    "scripts",
    "00_setup.R"
  )
)

cat("\n")
cat("==============================================================\n")
cat("DEFINE ANALYTICAL POPULATION\n")
cat("==============================================================\n\n")

# =============================================================================
# LOAD HARMONISED DATASET
# =============================================================================

analysis_data <-
  
  readRDS(
    
    file.path(
      
      paths$interim,
      
      "pooled_stage1.rds"
      
    )
    
  )

cat("✓ Harmonised dataset loaded.\n\n")

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

# =============================================================================
# VERIFY REQUIRED VARIABLES
# =============================================================================

required_variables <- c(
  
  "child_id",
  
  "survey_name",
  
  "survey_period",
  
  "analysis_year",
  
  "v005",
  
  "v021",
  
  "v022",
  
  "b19"
  
)

availability <-
  
  tibble(
    
    Variable = required_variables,
    
    Available = required_variables %in% names(analysis_data)
    
  )

print(availability)

missing_variables <-
  
  setdiff(
    
    required_variables,
    
    names(analysis_data)
    
  )

if(length(missing_variables) > 0){
  
  stop(
    
    paste(
      
      "Missing required variables:",
      
      paste(
        
        missing_variables,
        
        collapse = ", "
        
      )
      
    ),
    
    call. = FALSE
    
  )
  
}

cat("\n✓ Required variables verified.\n\n")

# =============================================================================
# CREATE DHS SAMPLING WEIGHT
# =============================================================================

cat("Creating DHS sampling weights...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    sample_weight =
      
      v005 /
      
      config$weight_divisor
    
  )

cat("✓ Sampling weights created.\n\n")

# =============================================================================
# VERIFY SURVEY DESIGN VARIABLES
# =============================================================================

survey_design_check <-
  
  analysis_data |>
  
  dplyr::summarise(
    
    Missing_weight =
      
      sum(is.na(sample_weight)),
    
    Missing_v021 =
      
      sum(is.na(v021)),
    
    Missing_v022 =
      
      sum(is.na(v022))
    
  )

print(survey_design_check)

stopifnot(
  
  survey_design_check$Missing_weight == 0,
  
  survey_design_check$Missing_v021 == 0,
  
  survey_design_check$Missing_v022 == 0
  
)

cat("\n✓ Survey design variables verified.\n\n")
# =============================================================================
# DEFINE ANALYTICAL POPULATION
# Children aged 12–23 months
# =============================================================================

cat(
  
  "Selecting children aged ",
  
  config$age_min,
  
  "–",
  
  config$age_max,
  
  " months...\n\n",
  
  sep = ""
  
)

records_before <- nrow(analysis_data)

analysis_data <-
  
  analysis_data |>
  
  dplyr::filter(
    
    b19 >= config$age_min,
    
    b19 <= config$age_max
    
  )

records_after <- nrow(analysis_data)

cat("✓ Eligible analytical population selected.\n\n")

population_summary <-
  
  tibble(
    
    Stage = c(
      
      "Pooled harmonised dataset",
      
      paste0(
        
        "Children aged ",
        
        config$age_min,
        
        "–",
        
        config$age_max,
        
        " months"
        
      )
      
    ),
    
    Records = c(
      
      records_before,
      
      records_after
      
    )
    
  )

print(population_summary)

# =============================================================================
# POPULATION SUMMARY BY SURVEY
# =============================================================================

cat("\nSummarising analytical population...\n\n")

survey_population_summary <-
  
  analysis_data |>
  
  dplyr::group_by(
    
    survey_name
    
  ) |>
  
  dplyr::summarise(
    
    Records = dplyr::n(),
    
    Mean_Age = mean(b19),
    
    SD_Age = sd(b19),
    
    Min_Age = min(b19),
    
    Max_Age = max(b19),
    
    .groups = "drop"
    
  )

print(survey_population_summary)

# =============================================================================
# VALIDATE ANALYTICAL POPULATION
# =============================================================================

cat("\nRunning analytical population validation...\n\n")

stopifnot(
  
  nrow(analysis_data) > 0
  
)

stopifnot(
  
  min(analysis_data$b19, na.rm = TRUE) >= config$age_min,
  
  max(analysis_data$b19, na.rm = TRUE) <= config$age_max
  
)

cat("✓ Age eligibility validated.\n\n")

# =============================================================================
# VALIDATE CHILD IDENTIFIER
# =============================================================================

duplicate_child_ids <-
  
  analysis_data |>
  
  dplyr::count(
    
    child_id
    
  ) |>
  
  dplyr::filter(
    
    n > 1
    
  )

if(nrow(duplicate_child_ids) > 0){
  
  stop(
    
    paste(
      
      "Duplicate child_id values detected:",
      
      nrow(duplicate_child_ids)
      
    ),
    
    call. = FALSE
    
  )
  
}

cat("✓ Child identifiers validated.\n\n")

# =============================================================================
# ORGANISE DATASET
# =============================================================================

analysis_data <-
  
  analysis_data |>
  
  dplyr::relocate(
    
    child_id,
    
    survey_name,
    
    survey_period,
    
    analysis_year,
    
    sample_weight,
    
    v021,
    
    v022,
    
    b19,
    
    .before = 1
    
  )

cat("✓ Dataset organised.\n\n")

# =============================================================================
# FINAL DATASET SUMMARY
# =============================================================================

final_summary <-
  
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

print(final_summary)
# =============================================================================
# SAVE ANALYTICAL POPULATION
# =============================================================================

saveRDS(
  
  analysis_data,
  
  file.path(
    
    paths$interim,
    
    "analysis_population.rds"
    
  ),
  
  compress = "xz"
  
)

cat("✓ Analysis population saved.\n\n")

# =============================================================================
# EXPORT LOG WORKBOOK
# =============================================================================

openxlsx::write.xlsx(
  
  list(
    
    Initial_Summary = initial_summary,
    
    Population_Summary = population_summary,
    
    Survey_Population = survey_population_summary,
    
    Survey_Design_Check = survey_design_check,
    
    Final_Summary = final_summary
    
  ),
  
  file = file.path(
    
    paths$logs,
    
    "04_define_population.xlsx"
    
  ),
  
  overwrite = TRUE
  
)

cat("✓ Log workbook exported.\n\n")

# =============================================================================
# FINAL VALIDATION
# =============================================================================

stopifnot(
  
  file.exists(
    
    file.path(
      
      paths$interim,
      
      "analysis_population.rds"
      
    )
    
  )
  
)

saved_dataset <-
  
  readRDS(
    
    file.path(
      
      paths$interim,
      
      "analysis_population.rds"
      
    )
    
  )

stopifnot(
  
  nrow(saved_dataset) == nrow(analysis_data),
  
  ncol(saved_dataset) == ncol(analysis_data)
  
)

required_output_variables <- c(
  
  "child_id",
  
  "survey_name",
  
  "survey_period",
  
  "analysis_year",
  
  "sample_weight",
  
  "v021",
  
  "v022",
  
  "b19"
  
)

stopifnot(
  
  all(
    
    required_output_variables %in%
      
      names(saved_dataset)
    
  )
  
)

cat("✓ Validation passed.\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("==============================================================\n")
cat("ANALYTICAL POPULATION SUMMARY\n")
cat("==============================================================\n\n")

print(final_summary)

cat("\n")

print(survey_population_summary)

cat("\n")

print(survey_design_check)

cat("\n")

cat(
  
  "Analysis population :\n",
  
  file.path(
    
    paths$interim,
    
    "analysis_population.rds"
    
  ),
  
  "\n\n",
  
  sep = ""
  
)

cat(
  
  "Population report :\n",
  
  file.path(
    
    paths$logs,
    
    "04_define_population.xlsx"
    
  ),
  
  "\n\n",
  
  sep = ""
  
)

cat("==============================================================\n")
cat("ANALYTICAL POPULATION CREATED SUCCESSFULLY\n")
cat("==============================================================\n")