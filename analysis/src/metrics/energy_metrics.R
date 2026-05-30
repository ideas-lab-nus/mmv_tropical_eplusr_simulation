# Energy metrics helpers for preparing Energy / Pareto plot inputs.

build_energy_long_table <- function(total_site_energy_df) {
  total_site_energy_df %>%
    tibble::rownames_to_column("City") %>%
    tidyr::pivot_longer(
      cols = -City,
      names_to = "Temperature",
      values_to = "Energy_kBtu"
    ) %>%
    dplyr::mutate(Temperature = as.numeric(Temperature))
}

build_energy_baseline_table <- function(total_site_energy_baseline_df, baseline_column = "22") {
  total_site_energy_baseline_df %>%
    tibble::rownames_to_column("City") %>%
    dplyr::rename(Baseline_Energy_kBtu = !!baseline_column)
}

build_energy_processed_table <- function(energy_long, energy_baseline_long, kbtu_to_kwh, area_m2) {
  energy_long %>%
    dplyr::left_join(energy_baseline_long, by = "City") %>%
    dplyr::mutate(
      EnergyIntensity_kWh_per_m2 = (Energy_kBtu * kbtu_to_kwh) / area_m2,
      Baseline_Intensity_kWh_per_m2 = (Baseline_Energy_kBtu * kbtu_to_kwh) / area_m2,
      EnergyReductionRate = (Baseline_Intensity_kWh_per_m2 - EnergyIntensity_kWh_per_m2) / Baseline_Intensity_kWh_per_m2
    )
}

build_violation_summary_table <- function(all_metrics_bh) {
  all_metrics_bh %>%
    dplyr::group_by(City, MaxOutdoorTemp, PlotGroup) %>%
    dplyr::summarise(
      MeanViolationRate = mean(ViolationRate, na.rm = TRUE),
      SD_ViolationRate = sd(ViolationRate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      LowerBound = MeanViolationRate - SD_ViolationRate,
      UpperBound = MeanViolationRate + SD_ViolationRate
    ) %>%
    dplyr::rename(Temperature = MaxOutdoorTemp)
}

build_energy_master_data <- function(energy_processed, violation_summary, city_display_mapping) {
  energy_processed %>%
    dplyr::left_join(violation_summary, by = c("City", "Temperature")) %>%
    dplyr::filter(!is.na(PlotGroup) & !is.na(EnergyReductionRate) & !is.na(MeanViolationRate)) %>%
    dplyr::left_join(city_display_mapping, by = "City") %>%
    dplyr::mutate(City = ifelse(!is.na(CityDisplay), CityDisplay, City))
}
