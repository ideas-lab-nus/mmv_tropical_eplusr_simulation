# Energy I/O helpers for parsing and persisting EnergyPlus table outputs.

calculate_total_site_energy_matrix <- function(epw_dict, max_outdoor_range = 27:32,
                                              base_path = simulation_csv_snapshot_root,
                                              output_csv) {

  city_list <- names(epw_dict)

  energy_matrix <- matrix(NA, nrow = length(city_list), ncol = length(max_outdoor_range))
  rownames(energy_matrix) <- city_list
  colnames(energy_matrix) <- as.character(max_outdoor_range)

  for (city in city_list) {
    cat("=== Processing city:", city, "===\n")

    for (max_outdoor in max_outdoor_range) {
      case_label <- paste0("MaxOutdoor_", max_outdoor)
      file_path <- file.path(base_path, city, case_label, "model_updatedTable.csv")

      if (!file.exists(file_path)) {
        cat("[Skip] File not found for", city, case_label, "\n")
        next
      }

      lines <- readLines(file_path)
      target_line <- grep("Total Site Energy", lines, value = TRUE)

      if (length(target_line) == 0) {
        cat("[Warning] 'Total Site Energy' not found for", city, case_label, "\n")
        next
      }

      parts <- strsplit(target_line[1], ",")[[1]]
      total_site_energy <- as.numeric(parts[3])

      energy_matrix[city, as.character(max_outdoor)] <- total_site_energy
      cat("  ->", case_label, ": Total Site Energy =", total_site_energy, "\n")
    }
  }

  output_csv <- ensure_parent_dir(output_csv)
  write.csv(energy_matrix, file = output_csv)
  cat("[Saved] Total Site Energy matrix saved to ", output_csv, "\n", sep = "")

  as.data.frame(energy_matrix)
}
