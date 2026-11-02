# =============================================================================
# 13_generate_figures.R
#
# Project : Nigeria DHS Immunization Analysis
# Purpose : Generate publication-quality figures for the manuscript
#
#           Zero-Dose Prevalence and Immunization Dropout as
#           Complementary Indicators of Immunization System
#           Performance in Nigeria:
#           Analysis of NDHS 2013–2024
#
# Author  : Nuhu Idris Itopa
# Version : 5.0
# Date    : 01 November 2026
# =============================================================================

source("~/nigeria-dhs-immunization-analysis/scripts/00_setup.R")

cat("==============================================================\n")
cat("GENERATE PUBLICATION FIGURES\n")
cat("==============================================================\n\n")

library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)
library(srvyr)
library(scales)
library(readr)

options(survey.lonely.psu = "adjust")

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
# CREATE OUTPUT DIRECTORY
# =============================================================================

dir.create(
  
  here::here(
    "outputs",
    "figures"
  ),
  
  recursive = TRUE,
  showWarnings = FALSE
  
)

# =============================================================================
# CREATE SURVEY DESIGN
# =============================================================================

cat("\nCreating survey design...\n\n")

survey_design <-
  
  analysis_data %>%
  
  as_survey_design(
    
    ids = cluster,
    
    strata = strata,
    
    weights = sample_weight,
    
    nest = TRUE
    
  )

cat("\u2713 Survey design created.\n\n")

# =============================================================================
# FIGURE 1
# NATIONAL TRENDS
# =============================================================================

cat("Generating Figure 1...\n\n")

trend_data <-
  
  survey_design %>%
  
  group_by(
    survey_period
  ) %>%
  
  summarise(
    
    zero_dose =
      survey_mean(
        zero_dose,
        vartype = "ci",
        na.rm = TRUE
      ),
    
    dropout =
      survey_mean(
        dropout_prevalence,
        vartype = "ci",
        na.rm = TRUE
      ),
    
    conventional =
      survey_mean(
        conventional_dropout,
        vartype = "ci",
        na.rm = TRUE
      )
    
  ) %>%
  
  mutate(
    
    zero_dose =
      zero_dose * 100,
    
    dropout =
      dropout * 100,
    
    conventional =
      conventional * 100
    
  )

trend_plot <-
  
  tibble(
    
    Survey = factor(
      c(
        "2013",
        "2018",
        "2023–24"
      ),
      levels = c(
        "2013",
        "2018",
        "2023–24"
      )
    ),
    
    `Zero-dose prevalence` =
      trend_data$zero_dose,
    
    `Dropout prevalence` =
      trend_data$dropout,
    
    `Conventional dropout rate` =
      trend_data$conventional
    
  ) %>%
  
  pivot_longer(
    
    -Survey,
    
    names_to = "Indicator",
    
    values_to = "Prevalence"
    
  )
# =============================================================================
# CREATE FIGURE 1
# =============================================================================

figure1 <-
  
  ggplot(
    
    trend_plot,
    
    aes(
      
      x = Survey,
      y = Prevalence,
      colour = Indicator,
      group = Indicator
      
    )
    
  ) +
  
  geom_line(
    
    linewidth = 1.5
    
  ) +
  
  geom_point(
    
    size = 5
    
  ) +
  
  geom_text(
    
    aes(
      label = sprintf("%.1f", Prevalence)
    ),
    
    vjust = -0.7,
    
    size = 5,
    
    fontface = "bold",
    
    show.legend = FALSE
    
  ) +
  
  scale_colour_manual(
    
    values = c(
      
      "Zero-dose prevalence" =
        "#2C7BB6",
      
      "Dropout prevalence" =
        "#1A9641",
      
      "Conventional dropout rate" =
        "#D7191C"
      
    )
    
  ) +
  
  labs(
    
    title =
      "Trends in Zero-Dose Prevalence and Immunization Dropout",
    
    subtitle =
      "Children aged 12–23 months, Nigeria DHS 2013–2024",
    
    x =
      "Survey year",
    
    y =
      "Prevalence (%)",
    
    colour = NULL
    
  ) +
  
  scale_y_continuous(
    
    limits = c(0, 55),
    
    breaks = seq(
      
      0,
      55,
      by = 5
      
    ),
    
    expand = expansion(
      
      mult = c(
        0,
        0.05
      )
      
    )
    
  ) +
  
  theme_bw(
    
    base_size = 16
    
  ) +
  
  theme(
    
    plot.title =
      element_text(
        
        face = "bold",
        
        size = 22,
        
        hjust = 0.5
        
      ),
    
    plot.subtitle =
      element_text(
        
        size = 16,
        
        hjust = 0.5
        
      ),
    
    axis.title =
      element_text(
        
        face = "bold",
        
        size = 18
        
      ),
    
    axis.text =
      element_text(
        
        size = 15
        
      ),
    
    legend.position =
      
      "bottom",
    
    legend.text =
      element_text(
        
        size = 15
        
      ),
    
    panel.grid.major =
      element_line(
        
        colour = "grey85"
        
      ),
    
    panel.grid.minor =
      element_blank()
    
  )

ggsave(
  
  filename = here::here(
    
    "outputs",
    "figures",
    "Figure1_Trends.png"
    
  ),
  
  plot = figure1,
  
  width = 12,
  
  height = 7,
  
  dpi = 600,
  
  bg = "white"
  
)

ggsave(
  
  filename = here::here(
    
    "outputs",
    "figures",
    "Figure1_Trends.pdf"
    
  ),
  
  plot = figure1,
  
  width = 12,
  
  height = 7
  
)

cat("✓ Figure 1 exported.\n\n")
# =============================================================================
# FIGURE 2
# MAGNITUDE OF ZERO-DOSE INEQUALITIES
# =============================================================================

cat("Generating Figure 2...\n\n")

# =============================================================================
# FIGURE 2
# MAGNITUDE OF ZERO-DOSE INEQUALITIES
# =============================================================================

cat("Generating Figure 2...\n\n")

# =============================================================================
# FIGURE 2
# MAGNITUDE OF ZERO-DOSE INEQUALITIES
# =============================================================================

cat("Generating Figure 2...\n\n")

equity_gap <-
  
  tibble(
    
    Characteristic = c(
      
      "ANC Attendance\n(No ANC vs ≥4 visits)",
      
      "Geopolitical Region\n(North West vs South East)",
      
      "Household Wealth\n(Poorest vs Richest)",
      
      "Maternal Education\n(No education vs Higher)",
      
      "Residence\n(Rural vs Urban)"
      
    ),
    
    Gap = c(
      
      59.0,
      
      53.8,
      
      60.4,
      
      62.7,
      
      29.8
      
    )
    
  ) %>%
  
  arrange(desc(Gap)) %>%
  
  mutate(
    
    Characteristic = factor(
      
      Characteristic,
      
      levels = Characteristic
      
    )
    
  )

figure2 <-
  
  ggplot(
    
    equity_gap,
    
    aes(
      
      x = Characteristic,
      
      y = Gap
      
    )
    
  ) +
  
  geom_col(
    
    fill = "#4C84B6",
    
    width = 0.70
    
  ) +
  
  geom_text(
    
    aes(
      
      label = sprintf("%.1f", Gap)
      
    ),
    
    vjust = -0.40,
    
    size = 6,
    
    fontface = "bold"
    
  ) +
  
  labs(
    
    title =
      "Absolute Equity Gaps in Zero-Dose Prevalence",
    
    subtitle =
      "Across selected determinants among children aged 12–23 months, Nigeria DHS 2013–2024",
    
    x = NULL,
    
    y = "Absolute equity gap (percentage points)"
    
  ) +
  
  scale_y_continuous(
    
    limits = c(0, 80),
    
    breaks = seq(0, 80, 10),
    
    expand = expansion(
      
      mult = c(0, 0.05)
      
    )
    
  ) +
  
  theme_bw(base_size = 18) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 24,
        hjust = 0.5
      ),
    
    plot.subtitle =
      element_text(
        size = 17,
        hjust = 0.5
      ),
    
    axis.title.y =
      element_text(
        face = "bold",
        size = 18
      ),
    
    axis.text.x =
      element_text(
        size = 15,
        angle = 15,
        hjust = 1,
        vjust = 1
      ),
    
    axis.text.y =
      element_text(
        size = 16
      ),
    
    panel.grid.major =
      element_line(
        colour = "grey88"
      ),
    
    panel.grid.minor =
      element_blank()
    
  )

ggsave(
  
  filename = here::here(
    
    "outputs",
    "figures",
    "Figure2_ZeroDose_Inequality.png"
    
  ),
  
  plot = figure2,
  
  width = 12,
  
  height = 8,
  
  dpi = 600,
  
  bg = "white"
  
)

ggsave(
  
  filename = here::here(
    
    "outputs",
    "figures",
    "Figure2_ZeroDose_Inequality.pdf"
    
  ),
  
  plot = figure2,
  
  width = 12,
  
  height = 8
  
)

cat("✓ Figure 2 exported.\n\n")

cat("✓ Figure 2 exported.\n\n")
# =============================================================================
# SUMMARY
# =============================================================================

cat("==============================================================\n")
cat("PUBLICATION FIGURES SUMMARY\n")
cat("==============================================================\n\n")

summary_table <-
  
  tibble(
    
    Figure = c(
      "Figure 1",
      "Figure 2"
    ),
    
    Description = c(
      "National trends in zero-dose, dropout prevalence and conventional dropout",
      "Magnitude of inequalities in zero-dose prevalence"
    ),
    
    Status = c(
      "Generated",
      "Generated"
    )
    
  )

print(summary_table)

cat("\nOutput folder\n")
cat("-------------\n")

cat(
  here::here(
    "outputs",
    "figures"
  ),
  "\n\n"
)

cat("Files created\n")
cat("-------------\n")

cat("• Figure1_Trends.png\n")
cat("• Figure1_Trends.pdf\n")
cat("• Figure2_ZeroDose_Inequality.png\n")
cat("• Figure2_ZeroDose_Inequality.pdf\n\n")

cat("==============================================================\n")
cat("PUBLICATION FIGURES GENERATED SUCCESSFULLY\n")
cat("==============================================================\n")