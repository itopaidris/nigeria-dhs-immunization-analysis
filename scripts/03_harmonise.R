# =============================================================================
# Project : Nigeria DHS Immunization Analysis (2013–2024)
# Script  : 03_harmonise.R
# Purpose : Harmonise DHS datasets across survey years
# Author  : Nuhu Itopa Idris
# Version : 5.0
# Updated : 2026-10-30
# =============================================================================

# =============================================================================
# INITIALISE PROJECT
# =============================================================================

source(here::here("scripts", "00_setup.R"))

cat("\n")
cat("==============================================================\n")
cat("HARMONISING DHS DATASETS\n")
cat("==============================================================\n\n")



# =============================================================================
# LOAD IMPORTED DATA
# =============================================================================

datasets <- list(
  
  NDHS2013 = readRDS(
    file.path(
      paths$interim,
      "2013_raw.rds"
    )
  ),
  
  NDHS2018 = readRDS(
    file.path(
      paths$interim,
      "2018_raw.rds"
    )
  ),
  
  NDHS2024 = readRDS(
    file.path(
      paths$interim,
      "2024_raw.rds"
    )
  )
  
)

variable_inventory <- readRDS(
  
  file.path(
    paths$interim,
    "variable_inventory.rds"
  )
  
)

cat("✓ Raw datasets loaded.\n\n")
cat("✓ Variable inventory loaded.\n\n")
# =============================================================================
# IDENTIFY COMMON VARIABLES
# =============================================================================

common_variables <-
  
  Reduce(
    intersect,
    purrr::map(
      datasets,
      names
    )
  )

cat("Common variables:", length(common_variables), "\n\n")

# =============================================================================
# HARMONISE SURVEY-SPECIFIC VARIABLES
# =============================================================================

cat("Harmonising survey-specific variables...\n\n")

harmonise_dataset <- function(data){
  
  # --------------------------------------------------------------------------
  # Ensure required variables exist
  # --------------------------------------------------------------------------
  
  required_variables <- c(
    
    # Survey identifiers
    "caseid",
    "v001",
    "v002",
    "v003",
    
    # Survey design
    "v005",
    "v021",
    "v022",
    
    # Geography
    "v024",
    "v025",
    
    # Socioeconomic variables
    "v106",
    "v190",
    
    # Maternal characteristics
    "v012",
    
    # Child characteristics
    "b4",
    "bord",
    "b11",
    
    # Child age variables
    "b19",
    "v008",
    "b3",
    
    # ANC
    "m14",
    
    # Vaccination variables
    "h2",
    "h3",
    "h5"
    
  )
  
  missing_variables <- setdiff(required_variables, names(data))
  
  if (length(missing_variables) > 0) {
    
    for (v in missing_variables) {
      
      data[[v]] <- NA
      
    }
    
  }
  
  # --------------------------------------------------------------------------
  # =============================================================================
  # HARMONISE SURVEY-SPECIFIC VARIABLES
  # =============================================================================
  
  cat("Harmonising survey-specific variables...\n\n")
  
  harmonise_dataset <- function(data){
    
    # --------------------------------------------------------------------------
    # Ensure required variables exist
    # --------------------------------------------------------------------------
    
    required_variables <- c(
      
      "caseid",
      "v001",
      "v002",
      "v003",
      "v005",
      "v008",
      "v021",
      "v022",
      "v024",
      "v025",
      "v106",
      "v190",
      "v012",
      "b3",
      "b4",
      "bord",
      "b11",
      "m14",
      "h2",
      "h3",
      "h5"
      
    )
    
    missing_variables <- setdiff(
      required_variables,
      names(data)
    )
    
    if(length(missing_variables) > 0){
      
      for(v in missing_variables){
        
        data[[v]] <- NA
        
      }
      
    }
    
    # --------------------------------------------------------------------------
    # Harmonise child age (months)
    # --------------------------------------------------------------------------
    # Create harmonised child age in months
    # NDHS 2013    : derive from Century Month Codes (v008 - b3)
    # NDHS 2018+   : use DHS child age variable (b19)
    # --------------------------------------------------------------------------
    
    if (unique(data$survey_name) == "NDHS 2013") {
      
      data$child_age_months <-
        suppressWarnings(
          as.numeric(data$v008) -
            as.numeric(data$b3)
        )
      
    } else {
      
      data$child_age_months <-
        suppressWarnings(
          as.numeric(data$b19)
        )
      
    }
    
    
    # --------------------------------------------------------------------------
    # Backward compatibility
    # Use harmonised age whenever b19 is unavailable
    # --------------------------------------------------------------------------
    
    if(!"b19" %in% names(data)){
      
      data$b19 <- data$child_age_months
      
    }
    
    # --------------------------------------------------------------------------
    # Standardise numeric variables
    # --------------------------------------------------------------------------
    
    numeric_variables <- c(
      
      "v001",
      "v002",
      "v003",
      "v005",
      "v008",
      "v021",
      "v022",
      "v024",
      "v025",
      "v106",
      "v190",
      "v012",
      "b3",
      "b19",
      "child_age_months",
      "b4",
      "bord",
      "b11",
      "m14",
      "h2",
      "h3",
      "h5"
      
    )
    
    for(v in intersect(numeric_variables, names(data))){
      
      data[[v]] <-
        suppressWarnings(
          as.numeric(data[[v]])
        )
      
    }
    
    data
    
  }
  
  # =============================================================================
  # APPLY HARMONISATION
  # =============================================================================
  
  datasets <-
    
    purrr::map(
      datasets,
      harmonise_dataset
    )
  
  cat("✓ Harmonisation completed.\n\n")
  
  # --------------------------------------------------------------------------
  # Standardise numeric variables
  # --------------------------------------------------------------------------
  
  numeric_variables <- c(
    
    "v001",
    "v002",
    "v003",
    "v005",
    "v021",
    "v022",
    "v024",
    "v025",
    "v106",
    "v190",
    "v012",
    "b19",
    "b4",
    "bord",
    "b11",
    "m14",
    "h2",
    "h3",
    "h5"
    
  )
  
  for(v in intersect(numeric_variables, names(data))){
    
    data[[v]] <-
      
      suppressWarnings(
        as.numeric(data[[v]])
      )
    
  }
  
  data
  
}

# =============================================================================
# APPLY HARMONISATION
# =============================================================================

datasets <-
  
  purrr::map(
    datasets,
    harmonise_dataset
  )

cat("✓ Harmonisation completed.\n\n")

# =============================================================================
# COMBINE ALL SURVEY DATASETS
# =============================================================================

cat("Combining survey datasets...\n")

# --------------------------------------------------------------------------
# Remove DHS value labels before pooling
# Prevents label conflicts across survey rounds
# --------------------------------------------------------------------------

datasets <-
  
  purrr::map(
    
    datasets,
    
    ~ dplyr::mutate(
      
      .x,
      
      dplyr::across(
        
        where(haven::is.labelled),
        
        haven::zap_labels
        
      )
      
    )
    
  )

# --------------------------------------------------------------------------
# Combine survey datasets
# --------------------------------------------------------------------------

harmonised_data <-
  
  dplyr::bind_rows(
    datasets
  )

if (nrow(harmonised_data) == 0) {
  
  stop(
    "Combined dataset contains zero rows.",
    call. = FALSE
  )
  
}

cat("✓ Surveys combined.\n\n")

# =============================================================================
# VERIFY DATASET
# =============================================================================

stopifnot(
  
  inherits(
    harmonised_data,
    c("data.frame", "tbl_df")
  )
  
)

cat("✓ Harmonised dataset available.\n\n")

# =============================================================================
# CREATE VARIABLE INVENTORY
# =============================================================================

cat("Creating variable inventory...\n")

variable_inventory <-
  
  tibble::tibble(
    
    Variable = names(harmonised_data),
    
    Class = purrr::map_chr(
      
      harmonised_data,
      
      ~ class(.x)[1]
      
    ),
    
    Missing = purrr::map_int(
      
      harmonised_data,
      
      ~ sum(is.na(.x))
      
    )
    
  ) |>
  
  dplyr::mutate(
    
    Missing_Percent = round(
      
      Missing / nrow(harmonised_data) * 100,
      
      2
      
    )
    
  )

# =============================================================================
# DATASET SUMMARY
# =============================================================================

dataset_summary <-
  
  tibble::tibble(
    
    Metric = c(
      "Records",
      "Variables",
      "Survey rounds"
    ),
    
    Value = c(
      
      nrow(harmonised_data),
      
      ncol(harmonised_data),
      
      dplyr::n_distinct(
        harmonised_data$survey_name
      )
      
    )
    
  )

print(dataset_summary)

cat("\n")

# =============================================================================
# SAVE HARMONISED DATASET
# =============================================================================

saveRDS(
  
  harmonised_data,
  
  file.path(
    paths$interim,
    "harmonised_dataset.rds"
  ),
  
  compress = "xz"
  
)

saveRDS(
  
  variable_inventory,
  
  file.path(
    paths$interim,
    "variable_inventory.rds"
  ),
  
  compress = "xz"
  
)

cat("✓ Harmonised dataset saved.\n")
cat("✓ Variable inventory saved.\n\n")
# =============================================================================
# EXPORT VARIABLE INVENTORY
# =============================================================================

openxlsx::write.xlsx(
  
  variable_inventory,
  
  file = file.path(
    
    paths$metadata,
    
    "variable_inventory.xlsx"
    
  ),
  
  overwrite = TRUE
  
)

cat("✓ Variable inventory exported.\n")

# =============================================================================
# EXPORT QC REPORT
# =============================================================================

openxlsx::write.xlsx(
  
  list(
    
    Dataset_Summary = dataset_summary,
    
    Variable_Inventory = variable_inventory
    
  ),
  
  file = file.path(
    
    paths$logs,
    
    "03_harmonisation_report.xlsx"
    
  ),
  
  overwrite = TRUE
  
)

cat("✓ Harmonisation report exported.\n\n")

# =============================================================================
# VALIDATION
# =============================================================================

required_variables <- c(
  
  "survey_name",
  
  "survey_year",
  
  "caseid",
  
  "v001",
  
  "v002",
  
  "v003",
  
  "v005"
  
)

assert_variables(
  
  harmonised_data,
  
  required_variables
  
)

cat("✓ Validation passed.\n\n")



# ====================================================================
# OUTPUT LOCATIONS
# =============================================================================

cat("Output files\n")
cat("--------------------------------------------------\n")

cat(
  
  "Harmonised dataset : ",
  
  file.path(
    
    paths$interim,
    
    "harmonised_dataset.rds"
    
  ),
  
  "\n"
  
)

cat(
  
  "Variable inventory : ",
  
  file.path(
    
    paths$metadata,
    
    "variable_inventory.xlsx"
    
  ),
  
  "\n"
  
)

cat(
  
  "QC report          : ",
  
  file.path(
    
    paths$logs,
    
    "03_harmonisation_report.xlsx"
    
  ),
  
  "\n\n"
  
)

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
      
      nrow(harmonised_data),
      
      ncol(harmonised_data),
      
      dplyr::n_distinct(
        
        harmonised_data$survey_name
        
      ),
      
      as.character(Sys.Date())
      
    )
    
  )

print(final_summary)

cat("\n")

# =============================================================================
# CLEAN UP
# =============================================================================

rm(
  
  datasets,
  
  dataset_summary,
  
  variable_inventory,
  
  required_variables,
  
  final_summary
  
)

gc(verbose = FALSE)

# =============================================================================
# COMPLETION
# =============================================================================

cat("==============================================================\n")
cat("VARIABLE HARMONISATION COMPLETED SUCCESSFULLY\n")
cat("==============================================================\n")