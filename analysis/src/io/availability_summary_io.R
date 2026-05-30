build_metrics_velocity_suffix <- function(max_velocity = NULL) {
  if (!is.null(max_velocity)) {
    paste0("_v_max_", gsub("\\.", "", as.character(max_velocity)))
  } else {
    ""
  }
}

build_metrics_business_hour_suffix <- function(start_hour = 9, end_hour = 18) {
  paste0("_bh_", start_hour, "_", end_hour)
}

build_metrics_summary_filename <- function(max_outdoor,
                                           start_hour = 9, end_hour = 18,
                                           max_velocity = NULL) {
  case_label <- paste0("MaxOutdoor_", max_outdoor)
  velocity_suffix <- build_metrics_velocity_suffix(max_velocity)
  business_hour_suffix <- build_metrics_business_hour_suffix(start_hour, end_hour)

  paste0(
    "Metrics_Summary_by_MetClo_",
    case_label,
    business_hour_suffix,
    velocity_suffix,
    ".csv"
  )
}

build_metrics_summary_path <- function(city_name, max_outdoor,
                                       start_hour = 9, end_hour = 18,
                                       max_velocity = NULL,
                                       base_dir = output_availability_dir) {
  summary_filename <- build_metrics_summary_filename(
    max_outdoor = max_outdoor,
    start_hour = start_hour,
    end_hour = end_hour,
    max_velocity = max_velocity
  )

  file.path(base_dir, city_name, summary_filename)
}

build_legacy_metrics_summary_path <- function(city_name, max_outdoor,
                                              start_hour = 9, end_hour = 18,
                                              max_velocity = NULL,
                                              base_dir = legacy_availability_dir) {
  build_metrics_summary_path(
    city_name = city_name,
    max_outdoor = max_outdoor,
    start_hour = start_hour,
    end_hour = end_hour,
    max_velocity = max_velocity,
    base_dir = base_dir
  )
}

resolve_metrics_summary_path <- function(city_name, max_outdoor,
                                         start_hour = 9, end_hour = 18,
                                         max_velocity = NULL,
                                         base_dir = output_availability_dir,
                                         legacy_base_dir = legacy_availability_dir) {
  canonical_path <- build_metrics_summary_path(
    city_name = city_name,
    max_outdoor = max_outdoor,
    start_hour = start_hour,
    end_hour = end_hour,
    max_velocity = max_velocity,
    base_dir = base_dir
  )
  legacy_path <- build_legacy_metrics_summary_path(
    city_name = city_name,
    max_outdoor = max_outdoor,
    start_hour = start_hour,
    end_hour = end_hour,
    max_velocity = max_velocity,
    base_dir = legacy_base_dir
  )

  resolve_canonical_or_legacy_path(canonical_path, legacy_path)
}

load_all_metrics_summary_business_hours <- function(epw_dict, max_outdoor_range,
                                                    start_hour = 9, end_hour = 18,
                                                    max_velocity = NULL,
                                                    base_dir = output_availability_dir,
                                                    legacy_base_dir = legacy_availability_dir) {
  all_results_list <- list()

  for (city_name in names(epw_dict)) {
    for (max_outdoor in max_outdoor_range) {
      file_path <- resolve_metrics_summary_path(
        city_name = city_name,
        max_outdoor = max_outdoor,
        start_hour = start_hour,
        end_hour = end_hour,
        max_velocity = max_velocity,
        base_dir = base_dir,
        legacy_base_dir = legacy_base_dir
      )

      if (file.exists(file_path)) {
        tmp <- read.csv(file_path)
        tmp$City <- city_name
        tmp$MaxOutdoorTemp <- max_outdoor
        tmp$MaxVelocity <- ifelse(!is.null(max_velocity), max_velocity, 2.0)

        all_results_list[[length(all_results_list) + 1]] <- tmp
      }
    }
  }

  if (!length(all_results_list)) {
    if (!is.null(max_velocity)) {
      warning(paste("No data found for max_velocity =", max_velocity))
    } else {
      warning("No data found for the original scenario (max_velocity = NULL)")
    }
    return(NULL)
  }

  dplyr::bind_rows(all_results_list) %>%
    dplyr::select(
      City, MaxOutdoorTemp, MaxVelocity, MET, CLO,
      AverageAvailability, TotalNVHours, ActualOKHours, ViolationHours
    )
}
