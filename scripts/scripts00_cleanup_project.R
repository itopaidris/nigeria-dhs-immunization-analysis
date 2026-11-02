# =============================================================================
# 00_cleanup_project.R
#
# Project : Nigeria DHS Immunization Analysis
# Purpose : Clean generated outputs before rebuilding the pipeline
# Author  : Nuhu Idris Itopa
# Version : 1.0
# Date    : 01 November 2026
# =============================================================================

source(here::here("scripts", "00_setup.R"))

cat("==============================================================\n")
cat("PROJECT CLEANUP\n")
cat("==============================================================\n\n")

library(fs)
library(here)
library(tibble)

# =============================================================================
# DIRECTORIES TO CLEAN
# =============================================================================

cleanup_dirs <- c(
  here("data", "processed"),
  here("outputs", "tables"),
  here("outputs", "figures"),
  here("outputs", "logs"),
  here("outputs", "codebooks"),
  here("outputs", "models"),
  here("outputs", "reports")
)

# =============================================================================
# CLEAN DIRECTORIES
# =============================================================================

cleanup_summary <-
  lapply(cleanup_dirs, function(dir_path) {
    
    if (!dir_exists(dir_path)) {
      dir_create(dir_path, recurse = TRUE)
      
      return(
        tibble(
          Directory = dir_path,
          Files_Removed = 0,
          Status = "Created"
        )
      )
    }
    
    files <- dir_ls(
      dir_path,
      recurse = TRUE,
      all = TRUE,
      type = c("file", "directory")
    )
    
    n_removed <- length(files)
    
    if (n_removed > 0) {
      file_delete(files[file_exists(files)])
      
      dirs <-
        files[dir_exists(files)]
      
      if (length(dirs) > 0) {
        
        dirs <-
          dirs[order(nchar(dirs), decreasing = TRUE)]
        
        walk(dirs, ~ try(dir_delete(.x), silent = TRUE))
      }
    }
    
    dir_create(dir_path, recurse = TRUE)
    
    tibble(
      Directory = dir_path,
      Files_Removed = n_removed,
      Status = "Cleaned"
    )
    
  })

cleanup_summary <-
  dplyr::bind_rows(cleanup_summary)

# =============================================================================
# SUMMARY
# =============================================================================

cat("Cleanup summary\n")
cat("------------------------------\n\n")

print(cleanup_summary)

cat("\n")

cat(
  "Total items removed:",
  sum(cleanup_summary$Files_Removed),
  "\n\n"
)

cat("==============================================================\n")
cat("PROJECT CLEANUP COMPLETED\n")
cat("==============================================================\n")