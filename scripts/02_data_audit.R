# =============================================================================
# Project : Nigeria DHS Immunization Analysis (2013–2024)
# Script  : 02_data_audit.R
# Purpose : Audit imported DHS datasets
# Author  : Nuhu Itopa Idris
# Version : 5.0
# =============================================================================

# =============================================================================
# INITIALISE PROJECT
# =============================================================================

source(here::here("scripts", "00_setup.R"))

cat("\n")
cat("==============================================================\n")
cat("AUDITING IMPORTED DHS DATASETS\n")
cat("==============================================================\n\n")

# =============================================================================
# LOAD IMPORTED DATASETS
# =============================================================================

datasets <- list(
  
  NDHS2013 = readRDS(
    file.path(paths$interim, "2013_raw.rds")
  ),
  
  NDHS2018 = readRDS(
    file.path(paths$interim, "2018_raw.rds")
  ),
  
  NDHS2024 = readRDS(
    file.path(paths$interim, "2024_raw.rds")
  )
  
)

cat("✓ Imported datasets loaded.\n\n")

# =============================================================================
# DATASET SUMMARY
# =============================================================================

dataset_summary <-
  
  purrr::imap_dfr(
    
    datasets,
    
    function(dat, id){
      
      tibble::tibble(
        
        Dataset = id,
        
        Records = nrow(dat),
        
        Variables = ncol(dat),
        
        Survey = unique(dat$survey_name),
        
        Year = unique(dat$survey_year)
        
      )
      
    }
    
  )

print(dataset_summary)

cat("\n")

# =============================================================================
# DUPLICATE CASEID CHECK
# =============================================================================

duplicate_report <-
  
  purrr::imap_dfr(
    
    datasets,
    
    function(dat, id){
      
      tibble::tibble(
        
        Dataset = id,
        
        Duplicate_CaseID =
          
          sum(duplicated(dat$caseid))
        
      )
      
    }
    
  )

print(duplicate_report)

cat("\n")
# =============================================================================
# VARIABLE INVENTORY
# =============================================================================

cat("Creating variable inventory...\n\n")

variable_inventory <-
  
  purrr::imap_dfr(
    
    datasets,
    
    function(dat, id){
      
      tibble::tibble(
        
        Dataset = id,
        
        Variable = names(dat),
        
        Class = purrr::map_chr(dat, ~class(.x)[1]),
        
        Label = purrr::map_chr(
          
          dat,
          
          ~{
            
            lbl <- attr(.x, "label")
            
            if(is.null(lbl)) "" else as.character(lbl)
            
          }
          
        )
        
      )
      
    }
    
  )

cat("✓ Variable inventory created.\n\n")

# =============================================================================
# MISSING VALUE SUMMARY
# =============================================================================

cat("Calculating missing values...\n\n")

missing_summary <-
  
  purrr::imap_dfr(
    
    datasets,
    
    function(dat, id){
      
      tibble::tibble(
        
        Dataset = id,
        
        Variable = names(dat),
        
        Missing = purrr::map_int(
          dat,
          ~sum(is.na(.x))
        ),
        
        Missing_Percent = purrr::map_dbl(
          
          dat,
          
          ~round(
            
            mean(is.na(.x))*100,
            
            2
            
          )
          
        )
        
      )
      
    }
    
  )

cat("✓ Missing value summary completed.\n\n")

# =============================================================================
# VARIABLE AVAILABILITY ACROSS SURVEYS
# =============================================================================

cat("Comparing variables across survey years...\n\n")

variable_presence <-
  
  purrr::reduce(
    
    list(
      
      tibble::tibble(
        
        Variable = names(datasets$NDHS2013),
        
        NDHS2013 = TRUE
        
      ),
      
      tibble::tibble(
        
        Variable = names(datasets$NDHS2018),
        
        NDHS2018 = TRUE
        
      ),
      
      tibble::tibble(
        
        Variable = names(datasets$NDHS2024),
        
        NDHS2024 = TRUE
        
      )
      
    ),
    
    dplyr::full_join,
    
    by = "Variable"
    
  ) |>
  
  dplyr::mutate(
    
    dplyr::across(
      
      starts_with("NDHS"),
      
      ~tidyr::replace_na(.x, FALSE)
      
    )
    
  )

cat("✓ Variable comparison completed.\n\n")

# =============================================================================
# COMMON VARIABLES
# =============================================================================

common_variables <-
  
  variable_presence |>
  
  dplyr::filter(
    
    NDHS2013 &
      NDHS2018 &
      NDHS2024
    
  )

cat(
  
  "Common variables across all surveys:",
  
  nrow(common_variables),
  
  "\n\n"
  
)
# =============================================================================
# SAVE METADATA
# =============================================================================

cat("Saving audit metadata...\n\n")

saveRDS(
  dataset_summary,
  file.path(paths$interim, "dataset_summary.rds"),
  compress = "xz"
)

saveRDS(
  variable_inventory,
  file.path(paths$interim, "variable_inventory.rds"),
  compress = "xz"
)

saveRDS(
  variable_presence,
  file.path(paths$interim, "variable_presence.rds"),
  compress = "xz"
)

cat("✓ Metadata saved.\n\n")

# =============================================================================
# EXPORT AUDIT WORKBOOK
# =============================================================================

openxlsx::write.xlsx(
  
  list(
    
    Dataset_Summary   = dataset_summary,
    
    Duplicate_Check   = duplicate_report,
    
    Variable_Presence = variable_presence,
    
    Missing_Summary   = missing_summary,
    
    Variable_Inventory= variable_inventory
    
  ),
  
  file = file.path(
    paths$logs,
    "02_data_audit_report.xlsx"
  ),
  
  overwrite = TRUE
  
)

cat("✓ Audit workbook exported.\n\n")

# =============================================================================
# VALIDATION
# =============================================================================

required_files <- c(
  
  "dataset_summary.rds",
  
  "variable_inventory.rds",
  
  "variable_presence.rds"
  
)

stopifnot(
  
  all(
    
    file.exists(
      
      file.path(
        paths$interim,
        required_files
      )
      
    )
    
  )
  
)

cat("✓ Validation passed.\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("Metadata created\n")
cat("-----------------------------\n")

cat(
  "Dataset summary : ",
  file.path(paths$interim, "dataset_summary.rds"),
  "\n"
)

cat(
  "Variable inventory : ",
  file.path(paths$interim, "variable_inventory.rds"),
  "\n"
)

cat(
  "Variable presence : ",
  file.path(paths$interim, "variable_presence.rds"),
  "\n\n"
)

cat(
  "Audit report : ",
  file.path(paths$logs, "02_data_audit_report.xlsx"),
  "\n\n"
)

cat("==============================================================\n")
cat("DATA AUDIT COMPLETED SUCCESSFULLY\n")
cat("==============================================================\n")