# =============================================================================
# Project : Nigeria DHS Immunization Analysis (2013–2024)
# Script  : 01_import.R
# Purpose : Import DHS KR datasets
# Author  : Nuhu Itopa Idris
# Version : 5.0
# =============================================================================

# =============================================================================
# INITIALISE PROJECT
# =============================================================================

source(here::here("scripts","00_setup.R"))

cat("\n")
cat("==============================================================\n")
cat("IMPORTING DHS DATASETS\n")
cat("==============================================================\n\n")

# =============================================================================
# INPUT FILES
# =============================================================================

input_files <- tibble::tibble(
  
  survey_name = c(
    "NDHS 2013",
    "NDHS 2018",
    "NDHS 2023-24"
  ),
  
  survey_year = c(
    2013,
    2018,
    2024
  ),
  
  filename = c(
    "NGKR6BFL.DTA",
    "NGKR7BFL.DTA",
    "NGKR8BFL.DTA"
  )
  
)

input_files <- input_files |>
  
  dplyr::mutate(
    
    full_path = file.path(
      paths$raw,
      filename
    )
    
  )

# =============================================================================
# VERIFY FILES
# =============================================================================

missing_files <-
  
  input_files |>
  
  dplyr::filter(
    !file.exists(full_path)
  )

if(nrow(missing_files) > 0){
  
  stop(
    
    paste(
      "Missing raw datasets:",
      paste(missing_files$filename,
            collapse = ", ")
    ),
    
    call. = FALSE
    
  )
  
}

cat("✓ All raw datasets located.\n\n")
# =============================================================================
# IMPORT DHS DATASETS
# =============================================================================

cat("Importing DHS datasets...\n\n")

import_dataset <- function(file, survey_name, survey_year){
  
  cat("Reading:", basename(file), "\n")
  
  data <- haven::read_dta(file)
  
  data <- data |>
    
    dplyr::mutate(
      
      survey_name = survey_name,
      
      survey_year = survey_year
      
    )
  
  cat(
    "  Records :", nrow(data),
    " Variables :", ncol(data),
    "\n\n"
  )
  
  data
  
}

datasets <-
  
  purrr::pmap(
    
    list(
      
      file = input_files$full_path,
      
      survey_name = input_files$survey_name,
      
      survey_year = input_files$survey_year
      
    ),
    
    import_dataset
    
  )

names(datasets) <-
  
  c(
    
    "NDHS2013",
    
    "NDHS2018",
    
    "NDHS2024"
    
  )

# =============================================================================
# IMPORT SUMMARY
# =============================================================================

import_summary <-
  
  purrr::imap_dfr(
    
    datasets,
    
    function(dat, id){
      
      tibble::tibble(
        
        Dataset = id,
        
        Survey = unique(dat$survey_name),
        
        Year = unique(dat$survey_year),
        
        Records = nrow(dat),
        
        Variables = ncol(dat)
        
      )
      
    }
    
  )

print(import_summary)

cat("\n")

# =============================================================================
# BASIC QUALITY CHECKS
# =============================================================================

required_variables <-
  
  c(
    
    "caseid",
    
    "v001",
    
    "v002",
    
    "v003",
    
    "v005",
    
    "b19"
    
  )

qc_report <-
  
  purrr::imap_dfr(
    
    datasets,
    
    function(dat, id){
      
      tibble::tibble(
        
        Dataset = id,
        
        Variable = required_variables,
        
        Present = required_variables %in% names(dat)
        
      )
      
    }
    
  )

print(qc_report)

cat("\n")
# =============================================================================
# SAVE IMPORTED DATASETS
# =============================================================================

cat("Saving imported datasets...\n\n")

saveRDS(
  datasets$NDHS2013,
  file.path(paths$interim, "2013_raw.rds"),
  compress = "xz"
)

saveRDS(
  datasets$NDHS2018,
  file.path(paths$interim, "2018_raw.rds"),
  compress = "xz"
)

saveRDS(
  datasets$NDHS2024,
  file.path(paths$interim, "2024_raw.rds"),
  compress = "xz"
)

cat("✓ Imported datasets saved.\n\n")

# =============================================================================
# EXPORT IMPORT REPORT
# =============================================================================

openxlsx::write.xlsx(
  
  list(
    
    Import_Summary = import_summary,
    
    Variable_Check = qc_report
    
  ),
  
  file = file.path(
    paths$logs,
    "01_import_report.xlsx"
  ),
  
  overwrite = TRUE
  
)

cat("✓ Import report exported.\n\n")

# =============================================================================
# FINAL VALIDATION
# =============================================================================

expected_files <- c(
  
  "2013_raw.rds",
  
  "2018_raw.rds",
  
  "2024_raw.rds"
  
)

saved_files <-
  
  file.exists(
    
    file.path(
      paths$interim,
      expected_files
    )
    
  )

stopifnot(all(saved_files))

cat("✓ Validation passed.\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("Imported datasets\n")
cat("-----------------\n")

cat(
  "2013 :",
  file.path(paths$interim, "2013_raw.rds"),
  "\n"
)

cat(
  "2018 :",
  file.path(paths$interim, "2018_raw.rds"),
  "\n"
)

cat(
  "2024 :",
  file.path(paths$interim, "2024_raw.rds"),
  "\n\n"
)

cat(
  "Import report:\n",
  file.path(paths$logs, "01_import_report.xlsx"),
  "\n\n"
)

cat("==============================================================\n")
cat("DHS DATA IMPORT COMPLETED SUCCESSFULLY\n")
cat("==============================================================\n")
