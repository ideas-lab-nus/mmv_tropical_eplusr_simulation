# Availability metrics helpers used by the analysis notebook and worker pipeline.

add_violation_rate <- function(df,
                               window = c("24h", "bh"),
                               start_hour = 9, end_hour = 18,
                               hours_per_year_24h = 8640) {
  window <- match.arg(window)
  denom <- if (window == "24h") {
    hours_per_year_24h
  } else {
    (end_hour - start_hour) * 365
  }
  df %>%
    dplyr::mutate(ViolationRate = ViolationHours / denom)
}

filter_by_business_hours <- function(df, start_hour = 9, end_hour = 18, include_end = FALSE, tz = "UTC") {
  dt <- df$datetime
  if (!inherits(dt, "POSIXct")) stop("datetime column must be POSIXct")
  attr(dt, "tzone") <- tz
  hr <- as.integer(format(dt, "%H"))
  if (include_end) {
    keep <- hr >= start_hour & hr <= (end_hour - 1) | (hr == end_hour & format(dt, "%M") == "00")
  } else {
    keep <- hr >= start_hour & hr < end_hour
  }
  df[keep, , drop = FALSE]
}

calculate_availability_optimized <- function(PMV_list, cooling_setpoint_df, MaxIndoorTemp,
                                             threshold = 0.5,
                                             start_hour = 9, end_hour = 18, include_end = FALSE, tz = "UTC",
                                             min_velocity = 0.1, max_velocity = 2.0) {
  if (is.null(max_velocity)) max_velocity <- 2.0
  lapply(seq_along(PMV_list), function(i) {
    pmv_df <- PMV_list[[i]]
    df_merged <- merge(pmv_df, cooling_setpoint_df, by = "datetime", all.x = TRUE)
    df_merged <- filter_by_business_hours(df_merged, start_hour, end_hour, include_end, tz)

    if (nrow(df_merged) == 0) return(data.frame(datetime = pmv_df$datetime[0]))

    all_pmv_cols <- grep("^PMV_vr_", names(pmv_df), value = TRUE)
    velocities <- as.numeric(sub("^PMV_vr_", "", all_pmv_cols))

    pmv_cols <- all_pmv_cols[!is.na(velocities) & velocities >= min_velocity & velocities <= max_velocity]

    if (length(pmv_cols) == 0) {
      ava_df <- data.frame(datetime = df_merged$datetime)
    } else {
      ava_df <- df_merged %>%
        mutate(across(
          all_of(pmv_cols),
          ~ ifelse(!is.na(.data$value) & abs(.data$value - MaxIndoorTemp) < 0.5,
                   as.integer(.x > -threshold & .x < threshold), 0),
          .names = "ava_{.col}_t05"
        )) %>%
        select(datetime, starts_with("ava_"))
    }
    ava_df
  })
}

combine_availability_single <- function(avail_list) {
  avail_list <- avail_list[!vapply(avail_list, function(x) is.null(x) || nrow(x) == 0, logical(1))]
  if (length(avail_list) == 0) return(data.frame(datetime = as.POSIXct(character()), All_rooms_availability = numeric()))

  room_series <- lapply(seq_along(avail_list), function(i) {
    df <- avail_list[[i]]
    if (ncol(df) <= 1) return(NULL)
    vals <- apply(df[, -1, drop = FALSE], 1, function(x) {
      if (all(is.na(x))) 0L else max(x, na.rm = TRUE)
    })
    data.frame(datetime = df$datetime, value = as.numeric(vals))
  })

  keep <- !vapply(room_series, is.null, logical(1))
  room_series <- room_series[keep]
  if (!length(room_series)) return(data.frame(datetime = as.POSIXct(character()), All_rooms_availability = numeric()))

  merged <- Reduce(function(a, b) merge(a, b, by = "datetime", all = FALSE), room_series)
  merged$All_rooms_availability <- rowMeans(merged[, -1, drop = FALSE], na.rm = TRUE)
  merged
}

calculate_violation_hours <- function(all_room_df, cooling_setpoint_df, MaxIndoorTemp,
                                      start_hour = 9, end_hour = 18, include_end = FALSE, tz = "UTC") {
  df_merged <- merge(all_room_df, cooling_setpoint_df, by = "datetime", all.x = TRUE)
  df_merged <- filter_by_business_hours(df_merged, start_hour, end_hour, include_end, tz)

  nv_active_rows <- abs(df_merged$value - MaxIndoorTemp) < 0.5
  total_nv_hours <- sum(nv_active_rows, na.rm = TRUE)
  actual_ok_hours <- sum(df_merged$All_rooms_availability[nv_active_rows], na.rm = TRUE)

  list(TotalNVHours = total_nv_hours, ActualOKHours = actual_ok_hours, ViolationHours = total_nv_hours - actual_ok_hours)
}
