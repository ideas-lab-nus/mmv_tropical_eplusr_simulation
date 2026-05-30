setwd("/workspaces/natural_vent_eplus/analysis")

source("config.R")
source("src/io/availability_summary_io.R")
source("src/metrics/availability_metrics.R")
source("src/pipelines/availability_pipeline.R")
source("src/plots/comfort_hours_plots.R")
source("src/plots/plot_annotations.R")

initialize_canonical_output_dirs()

test_city <- "Singapore"
test_velocity <- 1.2
test_temp_range <- 27:32
test_start_hour <- 9
test_end_hour <- 18
test_tz <- "UTC"
test_velocity_dir <- paste0("Comfort_Hours_Plots_v_max_", gsub("\\.", "_", as.character(test_velocity)))

test_output_root <- file.path(output_root, "test_pipeline")
test_data_root <- file.path(test_output_root, "data")
test_availability_root <- file.path(test_data_root, "availability")
test_cache_root <- file.path(test_data_root, "cache")
test_figures_root <- file.path(test_output_root, "figures")
test_comfort_hours_root <- file.path(test_figures_root, "comfort_hours")
test_comfort_hours_full_loop_root <- file.path(test_figures_root, "comfort_hours_full_loop")
test_logs_root <- file.path(test_output_root, "logs")
test_legacy_disabled_root <- file.path(test_output_root, "_no_legacy_fallback")

dir.create(test_availability_root, recursive = TRUE, showWarnings = FALSE)
dir.create(test_cache_root, recursive = TRUE, showWarnings = FALSE)
dir.create(test_comfort_hours_root, recursive = TRUE, showWarnings = FALSE)
dir.create(test_comfort_hours_full_loop_root, recursive = TRUE, showWarnings = FALSE)
dir.create(test_logs_root, recursive = TRUE, showWarnings = FALSE)
dir.create(test_legacy_disabled_root, recursive = TRUE, showWarnings = FALSE)

test_epw_dict <- epw_dict[names(epw_dict) %in% test_city]
if (length(test_epw_dict) != 1) {
  stop("Test city not found in epw_dict: ", test_city)
}

statuses <- vapply(test_temp_range, function(max_outdoor) {
  process_single_case_with_roots(
    max_outdoor = max_outdoor,
    country_name = test_city,
    max_velocity = test_velocity,
    start_hour = test_start_hour,
    end_hour = test_end_hour,
    tz = test_tz,
    pmv_root = pmv_snapshot_root,
    csv_root = simulation_csv_snapshot_root,
    output_base_dir = test_availability_root
  )
}, character(1))

status_df <- data.frame(
  City = test_city,
  MaxVelocity = test_velocity,
  MaxOutdoorTemp = test_temp_range,
  Status = statuses,
  stringsAsFactors = FALSE
)
write.csv(status_df, file.path(test_logs_root, "pipeline_status.csv"), row.names = FALSE)

comparison_rows <- lapply(test_temp_range, function(max_outdoor) {
  test_csv <- build_metrics_summary_path(
    city_name = test_city,
    max_outdoor = max_outdoor,
    start_hour = test_start_hour,
    end_hour = test_end_hour,
    max_velocity = test_velocity,
    base_dir = test_availability_root
  )
  reference_csv <- build_legacy_metrics_summary_path(
    city_name = test_city,
    max_outdoor = max_outdoor,
    start_hour = test_start_hour,
    end_hour = test_end_hour,
    max_velocity = test_velocity,
    base_dir = legacy_availability_dir
  )

  if (!file.exists(test_csv)) {
    stop("Missing test-generated summary: ", test_csv)
  }
  if (!file.exists(reference_csv)) {
    stop("Missing reference summary: ", reference_csv)
  }

  test_df <- read.csv(test_csv)
  reference_df <- read.csv(reference_csv)
  key_columns <- c("AverageAvailability", "TotalNVHours", "ActualOKHours", "ViolationHours")
  max_abs_diff <- max(abs(as.matrix(test_df[key_columns]) - as.matrix(reference_df[key_columns])), na.rm = TRUE)

  data.frame(
    City = test_city,
    MaxVelocity = test_velocity,
    MaxOutdoorTemp = max_outdoor,
    TestFile = test_csv,
    ReferenceFile = reference_csv,
    ColumnMatch = identical(names(test_df), names(reference_df)),
    RowCountMatch = nrow(test_df) == nrow(reference_df),
    NumericMatch = isTRUE(all.equal(test_df[key_columns], reference_df[key_columns], tolerance = 1e-10)),
    MaxAbsDiff = ifelse(is.finite(max_abs_diff), max_abs_diff, 0),
    stringsAsFactors = FALSE
  )
})

comparison_df <- dplyr::bind_rows(comparison_rows)
write.csv(comparison_df, file.path(test_logs_root, "summary_comparison.csv"), row.names = FALSE)

if (!all(comparison_df$ColumnMatch) || !all(comparison_df$RowCountMatch) || !all(comparison_df$NumericMatch)) {
  stop("Summary comparison failed for one or more temperature cases")
}

all_metrics_bh_test <- load_all_metrics_summary_business_hours(
  epw_dict = test_epw_dict,
  max_outdoor_range = test_temp_range,
  start_hour = test_start_hour,
  end_hour = test_end_hour,
  max_velocity = test_velocity,
  base_dir = test_availability_root,
  legacy_base_dir = test_legacy_disabled_root
) %>%
  add_violation_rate(window = "bh", start_hour = test_start_hour, end_hour = test_end_hour) %>%
  dplyr::left_join(city_climate_mapping, by = "City")

saveRDS(all_metrics_bh_test, file.path(test_cache_root, "all_metrics_bh_cache.rds"))

set2_colors <- RColorBrewer::brewer.pal(n = 3, name = "Set2")
desired_order_afam <- c(
  "Guam", "Honiara", "KualaLumpur", "Singapore",
  "Freetown", "Jakarta", "Lagos", "Manila", "Miami", "SantoDomingo"
)
desired_order_aw <- c(
  "Bangkok", "Mumbai", "Chennai", "RioDeJaneiro", "Darwin",
  "Kolkata", "Bengaluru", "Hyderabad", "Honolulu", "Dhaka"
)

metrics_for_velocity <- all_metrics_bh_test %>%
  dplyr::filter(MaxVelocity == test_velocity)

grp_afam <- metrics_for_velocity %>%
  dplyr::filter(PlotGroup %in% c("Af", "Am"))

figure_output_dir <- file.path(test_comfort_hours_root, test_velocity_dir)
generate_increment_plot(
  group_data = grp_afam,
  current_plot_group = "Af+Am",
  output_dir = figure_output_dir,
  max_v = test_velocity,
  city_order = desired_order_afam,
  city_display_mapping = city_display_mapping,
  city_climate_mapping = city_climate_mapping,
  set2_colors = set2_colors
)

expected_figure <- file.path(
  figure_output_dir,
  "Comfort_Hours_Increment_Group_Af+Am_v_max_1_2.png"
)
if (!file.exists(expected_figure)) {
  stop("Expected comfort-hours figure was not generated from test summaries")
}

full_city_epw_dict <- epw_dict[names(epw_dict) %in% city_climate_mapping$City]
all_metrics_bh_full_loop <- load_all_metrics_summary_business_hours(
  epw_dict = full_city_epw_dict,
  max_outdoor_range = test_temp_range,
  start_hour = test_start_hour,
  end_hour = test_end_hour,
  max_velocity = test_velocity,
  base_dir = test_availability_root,
  legacy_base_dir = legacy_availability_dir
) %>%
  add_violation_rate(window = "bh", start_hour = test_start_hour, end_hour = test_end_hour) %>%
  dplyr::left_join(city_climate_mapping, by = "City")

full_loop_figure_output_dir <- file.path(test_comfort_hours_full_loop_root, test_velocity_dir)

grp_afam_full_loop <- all_metrics_bh_full_loop %>%
  dplyr::filter(PlotGroup %in% c("Af", "Am"))

grp_aw_full_loop <- all_metrics_bh_full_loop %>%
  dplyr::filter(stringr::str_starts(PlotGroup, "Aw"))

generate_increment_plot(
  group_data = grp_afam_full_loop,
  current_plot_group = "Af+Am",
  output_dir = full_loop_figure_output_dir,
  max_v = test_velocity,
  city_order = desired_order_afam,
  city_display_mapping = city_display_mapping,
  city_climate_mapping = city_climate_mapping,
  set2_colors = set2_colors
)

generate_increment_plot(
  group_data = grp_aw_full_loop,
  current_plot_group = "Aw",
  output_dir = full_loop_figure_output_dir,
  max_v = test_velocity,
  city_order = desired_order_aw,
  city_display_mapping = city_display_mapping,
  city_climate_mapping = city_climate_mapping,
  set2_colors = set2_colors
)

expected_full_loop_figures <- c(
  file.path(full_loop_figure_output_dir, "Comfort_Hours_Increment_Group_Af+Am_v_max_1_2.png"),
  file.path(full_loop_figure_output_dir, "Comfort_Hours_Increment_Group_Aw_v_max_1_2.png")
)

missing_full_loop_figures <- expected_full_loop_figures[!file.exists(expected_full_loop_figures)]
if (length(missing_full_loop_figures) > 0) {
  stop("Missing full-loop comfort-hours figures: ", paste(missing_full_loop_figures, collapse = ", "))
}

readme_lines <- c(
  "# Comfort-Hours Pipeline Test",
  "",
  paste0("- City: ", test_city),
  paste0("- MaxVelocity: ", test_velocity),
  paste0("- Temperature loop: ", paste(test_temp_range, collapse = ", ")),
  paste0("- PMV root: ", pmv_snapshot_root),
  paste0("- CSV root: ", simulation_csv_snapshot_root),
  paste0("- Summary output root: ", test_availability_root),
  paste0("- Figure output root: ", figure_output_dir),
  paste0("- Expected figure: ", expected_figure),
  paste0("- Full-loop figure output root: ", full_loop_figure_output_dir),
  "- Full-loop figures:",
  paste0("  - ", expected_full_loop_figures[1]),
  paste0("  - ", expected_full_loop_figures[2]),
  "",
  "Outputs:",
  "- pipeline_status.csv",
  "- summary_comparison.csv",
  "- data/cache/all_metrics_bh_cache.rds",
  "- figures/comfort_hours/...",
  "- figures/comfort_hours_full_loop/...",
  "",
  "Comparison rule:",
  "- exact column names",
  "- exact row count",
  "- strict numeric equality with tolerance 1e-10 for key metrics"
)
writeLines(readme_lines, file.path(test_output_root, "README_pipeline_test.md"))

cat("PIPELINE_TEST_OK\n")
cat(test_city, "\n", sep = "")
cat(test_velocity, "\n", sep = "")
cat(expected_figure, "\n", sep = "")
cat(expected_full_loop_figures[1], "\n", sep = "")
cat(expected_full_loop_figures[2], "\n", sep = "")
