# ==============================================================================
# config.R: Project Settings and Master Data
# ==============================================================================

# 1. Environment & Libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(here)
library(data.table)
library(doParallel)
library(foreach)
library(scales)
library(reticulate)
library(DBI)
library(RSQLite)
library(tidyr)
library(patchwork)
library(doSNOW)
library(progress)
library(RColorBrewer)
library(akima)
library(metR)
library(scales)
library(grid)
if (requireNamespace("systemfonts", quietly = TRUE)) {
  library(systemfonts)
} else {
  warning("Optional package 'systemfonts' is not installed; continuing without it.",
          call. = FALSE)
}
library(eplusr)

# Python setup
use_python("/usr/bin/python3", required = TRUE)
pythermalcomfort <- import("pythermalcomfort")

path <- here()

# 1A. Phase 1: Canonical analysis output roots
analysis_root_candidate <- here("analysis")
analysis_root <- if (dir.exists(analysis_root_candidate)) analysis_root_candidate else here()

output_root <- file.path(analysis_root, "output")
output_data_root <- file.path(output_root, "data")
output_figures_root <- file.path(output_root, "figures")
output_logs_root <- file.path(output_root, "logs")

output_cache_dir <- file.path(output_data_root, "cache")
output_availability_dir <- file.path(output_data_root, "availability")
output_energy_dir <- file.path(output_data_root, "energy")
output_heatmap_data_dir <- file.path(output_data_root, "heatmap")

output_comfort_hours_dir <- file.path(output_figures_root, "comfort_hours")
output_poster_dir <- file.path(output_figures_root, "poster")
output_energy_figures_dir <- file.path(output_figures_root, "energy")
output_heatmap_figures_dir <- file.path(output_figures_root, "heatmap")
output_clomet_figures_dir <- file.path(output_figures_root, "clomet")

legacy_analysis_data_dir <- file.path(analysis_root, "data")
legacy_availability_dir <- file.path(legacy_analysis_data_dir, "idf", "21.Availability")
legacy_result_dir <- file.path(analysis_root, "Result")
legacy_energy_figures_dir <- file.path(legacy_result_dir, "Energy")
legacy_heatmap_data_dir <- file.path(legacy_result_dir, "Heatmap_SourceData")
legacy_heatmap_backup_data_dir <- file.path(legacy_heatmap_data_dir, "backup")
legacy_heatmap_figures_dir <- file.path(legacy_result_dir, "Heatmap_Combined_by_Group_2col")
legacy_clomet_figures_dir <- file.path(legacy_availability_dir, "_CLOMETmap_by_City_daytime")

ensure_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  dir_path
}

ensure_parent_dir <- function(file_path) {
  ensure_dir(dirname(file_path))
  file_path
}

canonical_output_dirs <- c(
  output_root,
  output_data_root,
  output_figures_root,
  output_logs_root,
  output_cache_dir,
  output_availability_dir,
  output_energy_dir,
  output_heatmap_data_dir,
  output_comfort_hours_dir,
  output_poster_dir,
  output_energy_figures_dir,
  output_heatmap_figures_dir,
  output_clomet_figures_dir
)

initialize_canonical_output_dirs <- function() {
  invisible(vapply(canonical_output_dirs, ensure_dir, character(1)))
}

resolve_path_with_fallbacks <- function(primary_path, fallback_paths = NULL) {
  candidate_paths <- c(primary_path, fallback_paths)
  candidate_paths <- unique(candidate_paths[!is.na(candidate_paths) & nzchar(candidate_paths)])

  for (candidate_path in candidate_paths) {
    if (file.exists(candidate_path)) {
      return(candidate_path)
    }
  }

  primary_path
}

resolve_canonical_or_legacy_path <- function(canonical_path, legacy_path = NULL, additional_paths = NULL) {
  resolve_path_with_fallbacks(canonical_path, c(legacy_path, additional_paths))
}

# 1B. Snapshot input roots and optional notebook diagnostics
source_idf_root <- file.path(
  analysis_root, "data", "idf", "11.setting_edit", "250812_DaytimeOnlyNV"
)
pmv_snapshot_root <- file.path(analysis_root, "R.Data", "260214_LowOutTemp20")
simulation_csv_snapshot_root <- file.path(
  analysis_root, "data", "idf", "12.cals", "260214_minOutTemp20_SETP"
)

show_setup_diagnostics <- FALSE
run_optional_input_check <- FALSE
diagnostic_city_name <- "Singapore"
diagnostic_temp_range <- 27:32

# EnergyPlus setup
ver <- "9.5.0"
idd <- use_idd(ver, download = "auto")

# 2. Constants
AREA_FT2 <- 21609.96
AREA_M2 <- AREA_FT2 * 0.092903
KBTU_TO_KWH <- 0.293071
set2_colors <- brewer.pal(n = 8, name = "Set2")
SET2_COLS <- set2_colors

# 3. City Master Data
city_climate_mapping <- data.frame(
  City = c(
    "Singapore", "KualaLumpur", "SantoDomingo", "Honolulu", "Freetown", "Guam", "Honiara",
    "Manila", "Dhaka", "Miami", "Jakarta", "Lagos",
    "Bangkok", "Mumbai", "Chennai", "RioDeJaneiro", "Darwin", "Kolkata", "Bengaluru", "Hyderabad"
  ),
  ClimateGroup = c(
    "Af", "Af", "Am", "Aw", "Am", "Af", "Af",
    "Am", "Aw", "Am", "Am", "Am",
    "Aw", "Aw", "Aw", "Aw", "Aw", "Aw", "Aw", "Aw"
  )
)

city_display_mapping <- data.frame(
  City = c("SantoDomingo", "KualaLumpur", "RioDeJaneiro"),
  CityDisplay = c("Santo Domingo", "Kuala Lumpur", "Rio de Janeiro"),
  stringsAsFactors = FALSE
)

south_asian_cities <- c("Mumbai", "Chennai", "Kolkata", "Bengaluru", "Hyderabad", "Dhaka")
city_climate_mapping <- city_climate_mapping %>%
  mutate(PlotGroup = case_when(
    City %in% south_asian_cities ~ "Aw (South Asia)",
    ClimateGroup == "Aw" ~ "Aw (Other)",
    TRUE ~ ClimateGroup
  ))

plot_groups <- c("Af", "Am", "Aw (South Asia)", "Aw (Other)")

combined_groups <- list(
  "Af+Am" = c("Af", "Am"),
  "Aw" = c("Aw (South Asia)", "Aw (Other)")
)

desired_order_afam <- c(
  "Guam", "Honiara", "KualaLumpur", "Singapore",
  "Freetown", "Jakarta", "Lagos", "Manila", "Miami", "SantoDomingo"
)

desired_order_aw <- c(
  "Bangkok", "Mumbai", "Chennai", "RioDeJaneiro", "Darwin",
  "Kolkata", "Bengaluru", "Hyderabad", "Honolulu", "Dhaka"
)

DESIRED_ORDER_AFAM <- desired_order_afam
DESIRED_ORDER_AW <- desired_order_aw

# 4. Optimal Temperatures
optimal_temps <- c(
  Guam = 29, Honiara = 30, KualaLumpur = 30, Singapore = 29, Freetown = 29,
  Jakarta = 30, Lagos = 30, Manila = 29, Miami = 29, SantoDomingo = 30,
  Bengaluru = 32, Chennai = 30, Dhaka = 30, Hyderabad = 32, Kolkata = 30,
  Mumbai = 30, Bangkok = 30, Darwin = 31, Honolulu = 30, RioDeJaneiro = 31
)
optimal_temps_df <- stack(optimal_temps)
colnames(optimal_temps_df) <- c("TargetTemp", "City")
city_temp_list <- tibble::deframe(optimal_temps_df[, c("City", "TargetTemp")])

city_data <- merge(city_climate_mapping, optimal_temps_df, by = "City")
city_data <- city_data %>%
  mutate(PlotGroup = case_when(
    City %in% south_asian_cities ~ "Aw (South Asia)",
    ClimateGroup == "Aw" ~ "Aw (Other)",
    TRUE ~ ClimateGroup
  ))

# 5. EPW Dictionary
epw_dict <- list(
  Bangkok = file.path(
    "~/localdir/analysis/data/epw/Bangkok/THA_CRG_Bangkok.Metropolis.484550_TMYx.2009-2023",
    "THA_CRG_Bangkok.Metropolis.484550_TMYx.2009-2023.epw"
  ),
  Bengaluru = file.path(
    "~/localdir/analysis/data/epw/Bengaluru/IND_KA_Bengaluru-Hindustan.AP.432960_TMYx.2004-2018",
    "IND_KA_Bengaluru-Hindustan.AP.432960_TMYx.2004-2018.epw"
  ),
  Chennai = file.path(
    "~/localdir/analysis/data/epw/Chennai/IND_TN_Chennai.Intl.AP.432790_TMYx.2009-2023",
    "IND_TN_Chennai.Intl.AP.432790_TMYx.2009-2023.epw"
  ),
  Darwin = file.path(
    "~/localdir/analysis/data/epw/Darwin/AUS_NT_Darwin_Intl_AP_941200_TMYx_2009-2023",
    "AUS_NT_Darwin_Intl_AP_941200_TMYx_2009-2023.epw"
  ),
  Dhaka = file.path(
    "~/localdir/analysis/data/epw/Dhaka/BGD_DH_Dhaka-Shahjalal.Intl.AP.419220_TMYx.2009-2023",
    "BGD_DH_Dhaka-Shahjalal.Intl.AP.419220_TMYx.2009-2023.epw"
  ),
  Freetown = file.path(
    "~/localdir/analysis/data/epw/Freetown/SLE_NO_Lungi.Intl.AP.618560_TMYx.2009-2023",
    "SLE_NO_Lungi.Intl.AP.618560_TMYx.2009-2023.epw"
  ),
  Guam = file.path(
    "~/localdir/analysis/data/epw/Guam/GUM_TM_Tamuning-Won.Pat.Intl.AP.912120_TMYx.2009-2023",
    "GUM_TM_Tamuning-Won.Pat.Intl.AP.912120_TMYx.2009-2023.epw"
  ),
  Honiara = file.path(
    "~/localdir/analysis/data/epw/Honiara/SLB_GU_Honiara.Intl.AP.915200_TMYx.2009-2023",
    "SLB_GU_Honiara.Intl.AP.915200_TMYx.2009-2023.epw"
  ),
  Honolulu = file.path(
    "~/localdir/analysis/data/epw/Honolulu/USA_HI_Honolulu-Inouye.Intl.AP.Oahu.911820_TMYx.2009-2023",
    "USA_HI_Honolulu-Inouye.Intl.AP.Oahu.911820_TMYx.2009-2023.epw"
  ),
  Hyderabad = file.path(
    "~/localdir/analysis/data/epw/Hyderabad/IND_TG_Hyderabad-Gandhi.Intl.AP.431285_TMYx.2009-2023",
    "IND_TG_Hyderabad-Gandhi.Intl.AP.431285_TMYx.2009-2023.epw"
  ),
  Jakarta = file.path(
    "~/localdir/analysis/data/epw/Jakarta/IDN_JW_Jakarta-Soekarno-Hatta.Intl.AP.967490_TMYx.2009-2023",
    "IDN_JW_Jakarta-Soekarno-Hatta.Intl.AP.967490_TMYx.2009-2023.epw"
  ),
  Kinshasa = file.path(
    "~/localdir/analysis/data/epw/Kinshasa/COD_KN_Kinshasa-Ndjili.Intl.AP.642100_TMYx.2009-2023",
    "COD_KN_Kinshasa-Ndjili.Intl.AP.642100_TMYx.2009-2023.epw"
  ),
  Kolkata = file.path(
    "~/localdir/analysis/data/epw/Kolkata/IND_WB_Kolkata-Bose.Intl.AP.428090_TMYx.2009-2023",
    "IND_WB_Kolkata-Bose.Intl.AP.428090_TMYx.2009-2023.epw"
  ),
  KualaLumpur = file.path(
    "~/localdir/analysis/data/epw/KualaLumpur/MYS_KL_Kuala.Lumpur-Subang-Abdul.Aziz.Shah.Intl.AP.486470_TMYx.2009-2023",
    "MYS_KL_Kuala.Lumpur-Subang-Abdul.Aziz.Shah.Intl.AP.486470_TMYx.2009-2023.epw"
  ),
  Lagos = file.path(
    "~/localdir/analysis/data/epw/Lagos/NGA_LA_Lagos-Muhammed.Intl.AP.652010_TMYx.2009-2023",
    "NGA_LA_Lagos-Muhammed.Intl.AP.652010_TMYx.2009-2023.epw"
  ),
  Manila = file.path(
    "~/localdir/analysis/data/epw/Manila/PHL_NCR_Manila.984250_TMYx.2009-2023",
    "PHL_NCR_Manila.984250_TMYx.2009-2023.epw"
  ),
  Miami = file.path(
    "~/localdir/analysis/data/epw/Miami/USA_FL_Miami.Intl.AP.722020_US.Normals.2006-2020",
    "USA_FL_Miami.Intl.AP.722020_US.Normals.2006-2020.epw"
  ),
  Mumbai = file.path(
    "~/localdir/analysis/data/epw/Mumbai/IND_MH_Mumbai-Shivaji.Intl.AP.430030_TMYx.2009-2023",
    "IND_MH_Mumbai-Shivaji.Intl.AP.430030_TMYx.2009-2023.epw"
  ),
  RioDeJaneiro = file.path(
    "~/localdir/analysis/data/epw/RioDeJaneiro/BRA_RJ_Rio.de.Janeiro-Galeao-Jobim.Intl.AP.837460_TMYx.2009-2023",
    "BRA_RJ_Rio.de.Janeiro-Galeao-Jobim.Intl.AP.837460_TMYx.2009-2023.epw"
  ),
  SantoDomingo = file.path(
    "~/localdir/analysis/data/epw/SantoDomingo/DOM_NC_Santo.Domingo.784860_TMYx.2009-2023",
    "DOM_NC_Santo.Domingo.784860_TMYx.2009-2023.epw"
  ),
  Singapore = file.path(
    "~/localdir/analysis/data/epw/Singapore/SGP_SINGAPORE-CHANGI-IAP_486980S_23",
    "SGP_SINGAPORE-CHANGI-IAP_486980S_23.epw"
  )
)

# Object names
natvent_list <- c(
  "NatVent_unit1_FrontRow_BottomFloor", "NatVent_unit2_FrontRow_BottomFloor", "NatVent_unit3_FrontRow_BottomFloor",
  "NatVent_unit1_BackRow_BottomFloor", "NatVent_unit2_BackRow_BottomFloor", "NatVent_unit3_BackRow_BottomFloor",
  "NatVent_unit1_FrontRow_MiddleFloor", "NatVent_unit2_FrontRow_MiddleFloor", "NatVent_unit3_FrontRow_MiddleFloor",
  "NatVent_unit1_BackRow_MiddleFloor", "NatVent_unit2_BackRow_MiddleFloor", "NatVent_unit3_BackRow_MiddleFloor",
  "NatVent_unit1_FrontRow_TopFloor", "NatVent_unit2_FrontRow_TopFloor", "NatVent_unit3_FrontRow_TopFloor",
  "NatVent_unit1_BackRow_TopFloor", "NatVent_unit2_BackRow_TopFloor", "NatVent_unit3_BackRow_TopFloor"
)

hybridvent_list <- c(
  "HybridVentilation_Control_unit1_FrontRow_BottomFloor", "HybridVentilation_Control_unit2_FrontRow_BottomFloor", "HybridVentilation_Control_unit3_FrontRow_BottomFloor",
  "HybridVentilation_Control_unit1_BackRow_BottomFloor", "HybridVentilation_Control_unit2_BackRow_BottomFloor", "HybridVentilation_Control_unit3_BackRow_BottomFloor",
  "HybridVentilation_Control_unit1_FrontRow_MiddleFloor", "HybridVentilation_Control_unit2_FrontRow_MiddleFloor", "HybridVentilation_Control_unit3_FrontRow_MiddleFloor",
  "HybridVentilation_Control_unit1_BackRow_MiddleFloor", "HybridVentilation_Control_unit2_BackRow_MiddleFloor", "HybridVentilation_Control_unit3_BackRow_MiddleFloor",
  "HybridVentilation_Control_unit1_FrontRow_TopFloor", "HybridVentilation_Control_unit2_FrontRow_TopFloor", "HybridVentilation_Control_unit3_FrontRow_TopFloor",
  "HybridVentilation_Control_unit1_BackRow_TopFloor", "HybridVentilation_Control_unit2_BackRow_TopFloor", "HybridVentilation_Control_unit3_BackRow_TopFloor"
)

### Monthly Tlow data is not part of the public sharing workflow.
tlow_csv_path <- NULL
tlow_master_df <- NULL
