# Availability pipeline worker for one city / temperature / velocity case.

process_single_case_with_roots <- function(max_outdoor, country_name, max_velocity, start_hour, end_hour, tz,
                                           pmv_root, csv_root, output_base_dir,
                                           csv_filename = "model_updated.csv",
                                           col_setpoint = "LIVING_UNIT1_FRONTROW_BOTTOMFLOOR:Zone Thermostat Cooling Setpoint Temperature [C](TimeStep)") {

  load_dir_pmv <- file.path(pmv_root, country_name)
  save_dir_metrics <- ensure_dir(file.path(output_base_dir, country_name))

  effective_max_velocity <- if (is.null(max_velocity)) 2.0 else max_velocity
  velocity_suffix <- if (!is.null(max_velocity)) paste0("_v_max_", gsub("\\.", "", as.character(max_velocity))) else ""
  business_hour_suffix <- paste0("_bh_", start_hour, "_", end_hour)

  case_label <- paste0("MaxOutdoor_", max_outdoor)
  pmv_file <- file.path(load_dir_pmv, paste0("PMV_result_", case_label, ".RData"))

  if (!file.exists(pmv_file)) return(paste("SKIP (No PMV):", case_label))

  temp_env <- new.env()
  load(pmv_file, envir = temp_env)
  if (!exists("all_pmv_data_for_case", envir = temp_env)) return(paste("SKIP (No Data obj):", case_label))
  all_data <- get("all_pmv_data_for_case", envir = temp_env)

  target_year <- format(Sys.Date(), "%Y")
  if (length(all_data) > 0) {
    for (i in seq_along(all_data)) {
      if (is.data.frame(all_data[[i]]) && nrow(all_data[[i]]) > 0) {
        target_year <- format(all_data[[i]]$datetime[1], "%Y")
        break
      }
    }
  }

  csv_path <- file.path(csv_root, country_name, case_label, csv_filename)
  if (!file.exists(csv_path)) return(paste("SKIP (No CSV):", case_label))

  df_csv <- read.csv(csv_path, check.names = FALSE)
  colnames(df_csv)[1] <- "datetime"
  if (nrow(df_csv) > 288) df_csv <- df_csv[-c(1:288), ]

  cooling_setpoint_df <- df_csv %>%
    mutate(
      dt_str = iconv(datetime, from = "", to = "UTF-8"),
      dt_str_year = paste0(target_year, "/", dt_str)
    ) %>%
    transmute(
      datetime = as.POSIXct(dt_str_year, format = "%Y/%m/%d  %H:%M:%S", tz = tz),
      value = .data[[col_setpoint]]
    ) %>%
    drop_na()

  vals <- cooling_setpoint_df$value
  valid_vals <- vals[vals >= 20 & vals <= 40]
  if (length(valid_vals) > 0) detected_setpoint <- max(valid_vals) else detected_setpoint <- 32.0
  MaxIndoorTemp <- detected_setpoint

  temp_case_summary <- data.frame()

  for (element_name in names(all_data)) {
    PMV_list <- all_data[[element_name]]

    Availability_list <- calculate_availability_optimized(
      PMV_list, cooling_setpoint_df, MaxIndoorTemp, 0.5,
      start_hour, end_hour, FALSE, tz,
      min_velocity = 0.1, max_velocity = effective_max_velocity
    )

    all_room <- combine_availability_single(Availability_list)

    if (nrow(all_room) > 0) {
      avg_avail <- mean(all_room$All_rooms_availability, na.rm = TRUE)
      violation_stats <- calculate_violation_hours(
        all_room, cooling_setpoint_df, MaxIndoorTemp,
        start_hour, end_hour, FALSE, tz
      )
    } else {
      avg_avail <- 0
      violation_stats <- list(TotalNVHours = 0, ActualOKHours = 0, ViolationHours = 0)
    }

    met_val <- as.numeric(stringr::str_match(element_name, "met_([0-9.]+)_")[, 2])
    clo_val <- as.numeric(stringr::str_match(element_name, "clo_([0-9.]+)")[, 2])

    temp_case_summary <- rbind(temp_case_summary, data.frame(
      MET = met_val, CLO = clo_val,
      AverageAvailability = avg_avail,
      TotalNVHours = violation_stats$TotalNVHours,
      ActualOKHours = violation_stats$ActualOKHours,
      ViolationHours = violation_stats$ViolationHours
    ))
  }

  fname <- paste0("Metrics_Summary_by_MetClo_", case_label, business_hour_suffix, velocity_suffix)
  save(temp_case_summary, file = file.path(save_dir_metrics, paste0(fname, ".RData")))
  write.csv(temp_case_summary, file = file.path(save_dir_metrics, paste0(fname, ".csv")), row.names = FALSE)

  return(paste0("DONE: ", case_label))
}

process_single_case <- function(max_outdoor, country_name, max_velocity, start_hour, end_hour, tz) {
  process_single_case_with_roots(
    max_outdoor = max_outdoor,
    country_name = country_name,
    max_velocity = max_velocity,
    start_hour = start_hour,
    end_hour = end_hour,
    tz = tz,
    pmv_root = "R.Data",
    csv_root = path.expand("~/localdir/analysis/data/idf/12.cals"),
    output_base_dir = output_availability_dir
  )
}
