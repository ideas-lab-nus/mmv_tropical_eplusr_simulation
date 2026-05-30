# Thin Energy data-preparation pipeline for notebook-facing execution.

prepare_energy_plot_data <- function(epw_dict,
                                     all_metrics_bh,
                                     main_temp_range = 27:32,
                                     baseline_temp_range = 22,
                                     base_path = simulation_csv_snapshot_root,
                                     main_output_csv,
                                     baseline_output_csv,
                                     kbtu_to_kwh = KBTU_TO_KWH,
                                     area_m2 = AREA_M2,
                                     city_display_mapping = city_display_mapping) {
  total_site_energy_df <- calculate_total_site_energy_matrix(
    epw_dict = epw_dict,
    max_outdoor_range = main_temp_range,
    base_path = base_path,
    output_csv = main_output_csv
  )

  total_site_energy_baseline_df <- calculate_total_site_energy_matrix(
    epw_dict = epw_dict,
    max_outdoor_range = baseline_temp_range,
    base_path = base_path,
    output_csv = baseline_output_csv
  )

  energy_long <- build_energy_long_table(total_site_energy_df)
  energy_baseline_long <- build_energy_baseline_table(total_site_energy_baseline_df)
  energy_processed <- build_energy_processed_table(
    energy_long = energy_long,
    energy_baseline_long = energy_baseline_long,
    kbtu_to_kwh = kbtu_to_kwh,
    area_m2 = area_m2
  )
  violation_summary <- build_violation_summary_table(all_metrics_bh)
  master_data <- build_energy_master_data(
    energy_processed = energy_processed,
    violation_summary = violation_summary,
    city_display_mapping = city_display_mapping
  )

  list(
    total_site_energy_df = total_site_energy_df,
    total_site_energy_baseline_df = total_site_energy_baseline_df,
    energy_long = energy_long,
    energy_baseline_long = energy_baseline_long,
    energy_processed = energy_processed,
    violation_summary = violation_summary,
    master_data = master_data
  )
}
