# =============================================================================
# Project : Nigeria DHS Immunization Analysis (2013–2024)
# Script  : 00_setup.R
# Purpose : Global project setup and configuration
# Author  : Nuhu Itopa Idris
# Version : 5.0
# =============================================================================

rm(list = ls())

# =============================================================================
# GLOBAL OPTIONS
# =============================================================================

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  digits = 4,
  width = 120,
  repos = c(CRAN = "https://cloud.r-project.org"),
  dplyr.summarise.inform = FALSE
)

# =============================================================================
# REQUIRED PACKAGES
# =============================================================================

required_packages <- c(
  
  # Data
  "tidyverse",
  "data.table",
  "janitor",
  "labelled",
  
  # Import
  "haven",
  "readxl",
  "writexl",
  "openxlsx",
  
  # Survey
  "survey",
  
  # Tables
  "gtsummary",
  "gt",
  "flextable",
  
  # Graphics
  "ggplot2",
  "patchwork",
  "scales",
  
  # Regression
  "broom",
  "broom.helpers",
  
  # Utilities
  "here",
  "fs",
  "glue",
  "stringr",
  "lubridate",
  "purrr"
)

# =============================================================================
# INSTALL MISSING PACKAGES
# =============================================================================

installed_packages <- rownames(installed.packages())

missing_packages <-
  setdiff(
    required_packages,
    installed_packages
  )

if(length(missing_packages) > 0){
  
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
  
}

# =============================================================================
# LOAD PACKAGES
# =============================================================================

invisible(
  
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
  
)

# =============================================================================
# PROJECT ROOT
# =============================================================================

project_root <- here::here()

if(project_root == getwd()){
  
  message("Project root detected.")
  
}

# =============================================================================
# PROJECT PATHS
# =============================================================================

paths <- list(
  
  # DATA
  
  raw             = here::here("data","raw"),
  
  interim         = here::here("data","interim"),
  
  processed       = here::here("data","processed"),
  
  # OUTPUTS
  
  outputs         = here::here("outputs"),
  
  datasets        = here::here("outputs","datasets"),
  
  tables          = here::here("outputs","tables"),
  
  figures         = here::here("outputs","figures"),
  
  models          = here::here("outputs","models"),
  
  codebooks       = here::here("outputs","codebooks"),
  
  reports         = here::here("outputs","reports"),
  
  # Backward compatibility
  
  output_datasets = here::here("outputs","datasets"),
  
  output_tables   = here::here("outputs","tables"),
  
  output_figures  = here::here("outputs","figures"),
  
  output_models   = here::here("outputs","models"),
  
  output_codebooks= here::here("outputs","codebooks"),
  
  output_reports  = here::here("outputs","reports"),
  
  # PROJECT
  
  logs            = here::here("logs"),
  
  metadata        = here::here("metadata"),
  
  manuscripts     = here::here("manuscripts"),
  
  references      = here::here("references"),
  
  cache           = here::here("cache"),
  
  qc              = here::here("quality_control")
  
)

# =============================================================================
# CREATE DIRECTORIES
# =============================================================================

purrr::walk(
  paths,
  fs::dir_create
)
# =============================================================================
# BACKWARD-COMPATIBLE DIRECTORY ALIASES
# =============================================================================

dir_raw        <- paths$raw
dir_interim    <- paths$interim
dir_processed  <- paths$processed

dir_outputs    <- paths$outputs
dir_datasets   <- paths$datasets
dir_tables     <- paths$tables
dir_figures    <- paths$figures
dir_models     <- paths$models
dir_codebooks  <- paths$codebooks
dir_reports    <- paths$reports

dir_logs       <- paths$logs
dir_metadata   <- paths$metadata
dir_cache      <- paths$cache
dir_qc         <- paths$qc
dir_manuscripts<- paths$manuscripts
dir_references <- paths$references
# =============================================================================
# PROJECT CONFIGURATION
# =============================================================================

config <- list(
  
  project_name   = "Nigeria DHS Immunization Analysis (2013–2024)",
  
  version        = "5.0",
  
  survey_names   = c(
    "NDHS 2013",
    "NDHS 2018",
    "NDHS 2023-24"
  ),
  
  survey_years   = c(
    2013,
    2018,
    2024
  ),
  
  age_min        = 12,
  
  age_max        = 23,
  
  weight_divisor = 1000000,
  
  random_seed    = 20261024
  
)

# =============================================================================
# ANALYSIS SETTINGS
# =============================================================================

analysis <- list(
  
  vaccinated_codes = c(1,2,3),
  
  unvaccinated_code = 0,
  
  missing_codes = c(
    7,8,9,
    97,98,99,
    997,998,999,
    9997,9998,9999
  )
  
)

# =============================================================================
# OUTPUT SETTINGS
# =============================================================================

output <- list(
  
  digits = 1,
  
  confidence_level = 0.95,
  
  percent_multiplier = 100
  
)

# =============================================================================
# GLOBAL GRAPHICS
# =============================================================================

ggplot2::theme_set(
  
  ggplot2::theme_minimal(
    base_size = 12
  )
  
)

# =============================================================================
# REPRODUCIBILITY
# =============================================================================

set.seed(config$random_seed)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

assert_variables <- function(data, variables){
  
  missing <-
    
    setdiff(
      variables,
      names(data)
    )
  
  if(length(missing)>0){
    
    warning(
      
      paste(
        "Missing variables:",
        paste(missing, collapse=", ")
      )
      
    )
    
  }
  
  invisible(missing)
  
}

# -----------------------------------------------------------------------------#

save_dataset <- function(data, filename){
  
  saveRDS(
    
    data,
    
    file.path(
      paths$interim,
      filename
    ),
    
    compress = "xz"
    
  )
  
}

# -----------------------------------------------------------------------------#

export_workbook <- function(sheets, filename){
  
  openxlsx::write.xlsx(
    
    sheets,
    
    file = file.path(
      paths$logs,
      filename
    ),
    
    overwrite = TRUE
    
  )
  
}

# -----------------------------------------------------------------------------#

recv <- function(x){
  
  dplyr::case_when(
    
    x %in% analysis$vaccinated_codes ~ 1L,
    
    x == analysis$unvaccinated_code ~ 0L,
    
    x %in% analysis$missing_codes ~ NA_integer_,
    
    TRUE ~ NA_integer_
    
  )
  
}

# -----------------------------------------------------------------------------#

log_message <- function(...){
  
  cat(...)
  
  cat("\n")
  
}

# =============================================================================
# PROJECT STATUS
# =============================================================================

folder_status <-
  
  tibble::tibble(
    
    Folder = names(paths),
    
    Exists = purrr::map_lgl(
      paths,
      dir.exists
    )
    
  )

# =============================================================================
# STARTUP MESSAGE
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat("      NIGERIA DHS IMMUNIZATION ANALYSIS PROJECT\n")
cat("==============================================================\n")

cat(
  "Project Root      :", project_root, "\n"
)

cat(
  "Date              :",
  format(Sys.Date(), "%Y-%m-%d"),
  "\n"
)

cat(
  "R Version         :",
  R.version.string,
  "\n"
)

cat(
  "Packages Loaded   :",
  length(required_packages),
  "\n"
)

cat("==============================================================\n\n")

cat("Project Configuration\n")
cat("---------------------\n")

cat(
  "Version           :",
  config$version,
  "\n"
)

cat(
  "Survey Years      :",
  paste(config$survey_years, collapse = ", "),
  "\n"
)

cat(
  "Age Range         :",
  config$age_min,
  "-",
  config$age_max,
  "months\n"
)

cat(
  "Weight Divisor    :",
  config$weight_divisor,
  "\n"
)

cat(
  "Random Seed       :",
  config$random_seed,
  "\n\n"
)

cat("Project successfully initialized.\n\n")