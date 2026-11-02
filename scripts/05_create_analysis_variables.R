# =============================================================================
# Project : Nigeria DHS Immunization Analysis (2013–2024)
# Script  : 05_create_analysis_variables.R
# Purpose : Create core analytical variables
# Author  : Nuhu Itopa Idris
# Version : 7.0
# Updated : 2026-10-31
# =============================================================================

# =============================================================================
# INITIALISE PROJECT
# =============================================================================

source(here::here("scripts", "00_setup.R"))

cat("\n")
cat("==============================================================\n")
cat("CREATE CORE ANALYTICAL VARIABLES\n")
cat("==============================================================\n\n")

# =============================================================================
# LOAD ANALYTICAL POPULATION
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
# INITIAL SUMMARY
# =============================================================================

initial_summary <-
  
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

print(initial_summary)

cat("\n")

# =============================================================================
# VERIFY RAW DHS VARIABLES
# =============================================================================

cat("Checking required raw DHS variables...\n\n")

required_variables <- c(
  
  # Identifiers
  "caseid",
  "survey_name",
  
  # Sampling
  "v005",
  "v001",
  "v002",
  "v003",
  "v021",
  "v022",
  
  # Geography
  "v024",
  "v025",
  
  # Maternal
  "v012",
  "v106",
  
  # Household
  "v190",
  
  # Child
  "b19",
  "b4",
  "bord",
  "b11",
  
  # ANC
  "m14",
  
  # Media
  "v157",
  "v158",
  "v159",
  
  # Insurance
  "v481"
  
)

assert_variables(
  
  analysis_data,
  
  required_variables
  
)

cat("✓ Raw variables verified.\n\n")

# =============================================================================
# CREATE IDENTIFIERS & METADATA
# =============================================================================

cat("Creating identifiers and metadata...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    child_id = caseid,
    
    sample_weight = v005 / config$weight_divisor,
    
    survey_period = dplyr::case_when(
      
      survey_name == "NDHS 2013" ~ "2013",
      
      survey_name == "NDHS 2018" ~ "2018",
      
      survey_name == "NDHS 2023-24" ~ "2023-24",
      
      TRUE ~ NA_character_
      
    ),
    
    analysis_year = dplyr::case_when(
      
      survey_name == "NDHS 2013" ~ 2013L,
      
      survey_name == "NDHS 2018" ~ 2018L,
      
      survey_name == "NDHS 2023-24" ~ 2024L,
      
      TRUE ~ NA_integer_
      
    ),
    
    survey_name = factor(
      
      survey_name,
      
      levels = config$survey_names
      
    ),
    
    survey_period = factor(
      
      survey_period,
      
      levels = c(
        "2013",
        "2018",
        "2023-24"
      )
      
    )
    
  )

cat("✓ Metadata created.\n\n")

# =============================================================================
# CREATE CHILD VARIABLES
# =============================================================================

cat("Creating child variables...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    child_age_months = b19,
    
    child_age_group = factor(
      
      dplyr::case_when(
        
        b19 >= 12 & b19 <= 17 ~ "12-17",
        
        b19 >= 18 & b19 <= 23 ~ "18-23",
        
        TRUE ~ NA_character_
        
      ),
      
      levels = c(
        "12-17",
        "18-23"
      )
      
    ),
    
    child_sex = factor(
      
      b4,
      
      levels = c(1, 2),
      
      labels = c(
        "Male",
        "Female"
      )
      
    )
    
  )

cat("✓ Child variables created.\n\n")

# =============================================================================
# CREATE MATERNAL VARIABLES
# =============================================================================

cat("Creating maternal variables...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    maternal_age = v012,
    
    maternal_age_group = factor(
      
      dplyr::case_when(
        
        v012 < 20 ~ "<20",
        
        dplyr::between(v012, 20, 34) ~ "20-34",
        
        v012 >= 35 ~ "35+",
        
        TRUE ~ NA_character_
        
      ),
      
      levels = c(
        "<20",
        "20-34",
        "35+"
      )
      
    ),
    
    maternal_education = factor(
      
      v106,
      
      levels = c(0, 1, 2, 3),
      
      labels = c(
        
        "No education",
        
        "Primary",
        
        "Secondary",
        
        "Higher"
        
      )
      
    )
    
  )

cat("✓ Maternal variables created.\n\n")
# =============================================================================
# CREATE HOUSEHOLD VARIABLES
# =============================================================================

cat("Creating household variables...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
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
    
    residence = factor(
      
      v025,
      
      levels = c(1, 2),
      
      labels = c(
        
        "Urban",
        "Rural"
        
      )
      
    ),
    
    region = factor(v024)
    
  )

cat("✓ Household variables created.\n\n")

# =============================================================================
# CREATE BIRTH VARIABLES
# =============================================================================

cat("Creating birth variables...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    birth_order = bord,
    
    birth_order_group = factor(
      
      dplyr::case_when(
        
        bord == 1 ~ "1",
        
        bord %in% 2:3 ~ "2-3",
        
        bord >= 4 ~ "4+",
        
        TRUE ~ NA_character_
        
      ),
      
      levels = c(
        
        "1",
        "2-3",
        "4+"
        
      )
      
    ),
    
    preceding_birth_interval = b11
    
  )

cat("✓ Birth variables created.\n\n")

# =============================================================================
# CREATE ANC VARIABLES
# =============================================================================

cat("Creating ANC variables...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    anc_visits = m14,
    
    anc_category = factor(
      
      dplyr::case_when(
        
        is.na(m14) ~ NA_character_,
        
        m14 == 0 ~ "0",
        
        dplyr::between(m14, 1, 3) ~ "1-3",
        
        dplyr::between(m14, 4, 7) ~ "4-7",
        
        m14 >= 8 ~ "8+",
        
        TRUE ~ NA_character_
        
      ),
      
      levels = c(
        
        "0",
        "1-3",
        "4-7",
        "8+"
        
      )
      
    ),
    
    anc4plus = factor(
      
      dplyr::case_when(
        
        is.na(m14) ~ NA_character_,
        
        m14 >= 4 ~ "Yes",
        
        m14 < 4 ~ "No",
        
        TRUE ~ NA_character_
        
      ),
      
      levels = c(
        
        "No",
        "Yes"
        
      )
      
    )
    
  )

cat("✓ ANC variables created.\n\n")

# =============================================================================
# CREATE MEDIA EXPOSURE
# =============================================================================

cat("Creating media exposure variables...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    media_exposure = factor(
      
      dplyr::case_when(
        
        (v157 > 0) |
          (v158 > 0) |
          (v159 > 0) ~ "Exposed",
        
        (v157 == 0) &
          (v158 == 0) &
          (v159 == 0) ~ "Not exposed",
        
        TRUE ~ NA_character_
        
      ),
      
      levels = c(
        
        "Not exposed",
        "Exposed"
        
      )
      
    )
    
  )

cat("✓ Media exposure created.\n\n")

# =============================================================================
# CREATE HEALTH INSURANCE VARIABLE
# =============================================================================

cat("Creating insurance variable...\n\n")

analysis_data <-
  
  analysis_data |>
  
  dplyr::mutate(
    
    insured = factor(
      
      dplyr::case_when(
        
        is.na(v481) ~ NA_character_,
        
        v481 == 1 ~ "Insured",
        
        v481 == 0 ~ "Not insured",
        
        TRUE ~ "Not insured"
        
      ),
      
      levels = c(
        
        "Not insured",
        "Insured"
        
      )
      
    )
    
  )

cat("✓ Insurance variable created.\n\n")

# =============================================================================
# CREATE ANALYSIS VARIABLE SUMMARY
# =============================================================================

cat("Creating analysis variable summary...\n\n")

analysis_variable_summary <-
  
  tibble::tibble(
    
    Variable = c(
      
      "child_id",
      "sample_weight",
      "survey_period",
      "analysis_year",
      "child_age_months",
      "child_age_group",
      "child_sex",
      "birth_order",
      "birth_order_group",
      "preceding_birth_interval",
      "maternal_age",
      "maternal_age_group",
      "maternal_education",
      "wealth_quintile",
      "residence",
      "region",
      "anc_visits",
      "anc_category",
      "anc4plus",
      "media_exposure",
      "insured"
      
    ),
    
    Missing = purrr::map_int(
      
      c(
        
        "child_id",
        "sample_weight",
        "survey_period",
        "analysis_year",
        "child_age_months",
        "child_age_group",
        "child_sex",
        "birth_order",
        "birth_order_group",
        "preceding_birth_interval",
        "maternal_age",
        "maternal_age_group",
        "maternal_education",
        "wealth_quintile",
        "residence",
        "region",
        "anc_visits",
        "anc_category",
        "anc4plus",
        "media_exposure",
        "insured"
        
      ),
      
      ~ sum(is.na(analysis_data[[.x]]))
      
    )
    
  ) |>
  
  dplyr::mutate(
    
    Missing_Percent = round(
      
      Missing / nrow(analysis_data) * 100,
      
      2
      
    )
    
  )

print(analysis_variable_summary)

cat("\n")
# =============================================================================
# VALIDATION
# =============================================================================

cat("Running validation...\n\n")

required_outputs <- c(
  
  "child_id",
  "sample_weight",
  "survey_name",
  "survey_period",
  "analysis_year",
  
  "child_age_months",
  "child_age_group",
  "child_sex",
  
  "birth_order",
  "birth_order_group",
  "preceding_birth_interval",
  
  "maternal_age",
  "maternal_age_group",
  "maternal_education",
  
  "wealth_quintile",
  "residence",
  "region",
  
  "anc_visits",
  "anc_category",
  "anc4plus",
  
  "media_exposure",
  "insured"
  
)

assert_variables(
  
  analysis_data,
  
  required_outputs
  
)

cat("✓ Validation passed.\n\n")

# =============================================================================
# SAVE ANALYSIS POPULATION
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
# CREATE CODEBOOK
# =============================================================================

codebook <-
  
  tibble::tibble(
    
    Variable = names(analysis_data),
    
    Class = purrr::map_chr(
      
      analysis_data,
      
      ~ class(.x)[1]
      
    ),
    
    Missing = purrr::map_int(
      
      analysis_data,
      
      ~ sum(is.na(.x))
      
    )
    
  ) |>
  
  dplyr::mutate(
    
    Missing_Percent = round(
      
      Missing /
        
        nrow(analysis_data) *
        
        100,
      
      2
      
    )
    
  )

openxlsx::write.xlsx(
  
  codebook,
  
  file.path(
    
    paths$codebooks,
    
    "05_analysis_variable_codebook.xlsx"
    
  ),
  
  overwrite = TRUE
  
)

cat("✓ Codebook exported.\n\n")

# =============================================================================
# EXPORT REPORT
# =============================================================================

openxlsx::write.xlsx(
  
  list(
    
    Dataset_Summary = initial_summary,
    
    Variable_Summary = analysis_variable_summary,
    
    Codebook = codebook
    
  ),
  
  file.path(
    
    paths$logs,
    
    "05_analysis_variables_report.xlsx"
    
  ),
  
  overwrite = TRUE
  
)

cat("✓ Analysis report exported.\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

final_summary <-
  
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
      
      dplyr::n_distinct(
        
        analysis_data$survey_name
        
      ),
      
      as.character(Sys.Date())
      
    )
    
  )

print(final_summary)

cat("\n")

cat(
  
  "Analysis population : ",
  
  file.path(
    
    paths$interim,
    
    "analysis_population.rds"
    
  ),
  
  "\n"
  
)

cat(
  
  "Codebook : ",
  
  file.path(
    
    paths$codebooks,
    
    "05_analysis_variable_codebook.xlsx"
    
  ),
  
  "\n"
  
)

cat(
  
  "QC Report : ",
  
  file.path(
    
    paths$logs,
    
    "05_analysis_variables_report.xlsx"
    
  ),
  
  "\n\n"
  
)

# =============================================================================
# CLEAN UP
# =============================================================================

rm(
  
  initial_summary,
  analysis_variable_summary,
  codebook,
  final_summary,
  required_variables,
  required_outputs
  
)

gc(verbose = FALSE)

# =============================================================================
# COMPLETION
# ==========================================================

cat("==============================================================\n")
cat("CORE ANALYSIS VARIABLES CREATED SUCCESSFULLY\n")
cat("==============================================================\n")