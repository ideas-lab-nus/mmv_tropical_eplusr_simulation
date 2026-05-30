generate_increment_plot <- function(group_data, current_plot_group, output_dir, max_v, city_order,
                                    city_display_mapping,
                                    city_climate_mapping,
                                    set2_colors,
                                    facet_ncol = 2,
                                    plot_width = 8,
                                    plot_height = NULL,
                                    y_limits = c(0, 3500),
                                    y_breaks = NULL) {
  valid_cities_in_order <- city_order[city_order %in% unique(group_data$City)]

  if (length(valid_cities_in_order) == 0) {
    cat(paste("  > SKIPPING group", current_plot_group, "- no data to plot after filtering.\n"))
    return(NULL)
  }

  group_data <- group_data %>%
    dplyr::mutate(City = factor(City, levels = valid_cities_in_order)) %>%
    dplyr::arrange(City)

  labels_df <- group_data %>%
    dplyr::distinct(City) %>%
    dplyr::left_join(city_display_mapping, by = "City") %>%
    dplyr::mutate(CityDisplay = dplyr::coalesce(CityDisplay, as.character(City))) %>%
    dplyr::left_join(
      city_climate_mapping %>% dplyr::select(City, ClimateGroup),
      by = "City"
    ) %>%
    dplyr::mutate(
      CityLabel = ifelse(
        is.na(ClimateGroup),
        CityDisplay,
        paste0(CityDisplay, " (", ClimateGroup, ")")
      )
    ) %>%
    dplyr::mutate(City = factor(City, levels = valid_cities_in_order))

  cat(paste(
    "  > Found", length(valid_cities_in_order), "cities to plot in specific order.\n"
  ))

  summary_data <- group_data %>%
    dplyr::group_by(City, MaxOutdoorTemp) %>%
    dplyr::summarise(
      Mean_Comfortable   = mean(ActualOKHours, na.rm = TRUE),
      Mean_Uncomfortable = mean(TotalNVHours, na.rm = TRUE) - Mean_Comfortable,
      Comfort_IQR_lower  = stats::quantile(ActualOKHours, 0.25, na.rm = TRUE),
      Comfort_IQR_upper  = stats::quantile(ActualOKHours, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(City = factor(City, levels = valid_cities_in_order))

  increment_data <- summary_data %>%
    dplyr::group_by(City) %>%
    dplyr::arrange(MaxOutdoorTemp, .by_group = TRUE) %>%
    dplyr::mutate(
      Comfort_Increment = Mean_Comfortable - dplyr::lag(Mean_Comfortable),
      Comfort_Label_Y   = Mean_Comfortable / 2,
      Comfort_Increment_Label = dplyr::if_else(
        !is.na(Comfort_Increment),
        sprintf("%+.0f", Comfort_Increment),
        NA_character_
      ),
      Uncomfortable_Increment = Mean_Uncomfortable - dplyr::lag(Mean_Uncomfortable),
      Uncomfortable_Label_Y   = Mean_Comfortable + (Mean_Uncomfortable / 2),
      Uncomfortable_Increment_Label = dplyr::if_else(
        !is.na(Uncomfortable_Increment),
        sprintf("%+.0f", Uncomfortable_Increment),
        NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(Comfort_Increment)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(City = factor(City, levels = valid_cities_in_order))

  plot_data <- summary_data %>%
    dplyr::select(City, MaxOutdoorTemp, Mean_Comfortable, Mean_Uncomfortable) %>%
    tidyr::pivot_longer(
      cols = c(Mean_Comfortable, Mean_Uncomfortable),
      names_to = "MetricType",
      values_to = "MeanHours"
    ) %>%
    dplyr::mutate(
      City = factor(City, levels = valid_cities_in_order),
      MetricType = factor(
        MetricType,
        levels = c("Mean_Uncomfortable", "Mean_Comfortable"),
        labels = c("Violation Hours", "Comfortable Hours")
      )
    )

  panel_labels <- summary_data %>%
    dplyr::group_by(City) %>%
    dplyr::summarise(
      x_pos = mean(range(MaxOutdoorTemp, na.rm = TRUE)),
      y_pos = y_limits[2] - ((y_limits[2] - y_limits[1]) * 0.03),
      .groups = "drop"
    ) %>%
    dplyr::left_join(labels_df, by = "City") %>%
    dplyr::mutate(City = factor(City, levels = valid_cities_in_order))

  x_breaks_all <- sort(unique(summary_data$MaxOutdoorTemp))

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = MaxOutdoorTemp, y = MeanHours, fill = MetricType)
  ) +
    ggplot2::geom_col(position = "stack", alpha = 0.8, width = 0.7) +
    ggplot2::geom_errorbar(
      data = summary_data,
      ggplot2::aes(x = MaxOutdoorTemp, ymin = Comfort_IQR_lower, ymax = Comfort_IQR_upper),
      inherit.aes = FALSE,
      width = 0.1, color = "black", size = 0.5
    ) +
    ggplot2::geom_text(
      data = increment_data,
      ggplot2::aes(x = MaxOutdoorTemp, y = Comfort_Label_Y, label = Comfort_Increment_Label),
      inherit.aes = FALSE,
      hjust = 1, nudge_x = -0.15,
      color = "black", fontface = "italic", size = 3.5
    ) +
    ggplot2::geom_text(
      data = increment_data,
      ggplot2::aes(x = MaxOutdoorTemp, y = Uncomfortable_Label_Y, label = Uncomfortable_Increment_Label),
      inherit.aes = FALSE,
      hjust = 1, nudge_x = -0.15,
      color = "black", fontface = "italic", size = 3.5
    ) +
    ggplot2::geom_text(
      data = panel_labels,
      ggplot2::aes(x = x_pos, y = y_pos, label = CityLabel),
      inherit.aes = FALSE,
      hjust = 0.5, vjust = 1,
      size = 4, fontface = "bold"
    ) +
    ggplot2::facet_wrap(~ City, ncol = facet_ncol, scales = "free_x") +
    ggplot2::scale_fill_manual(
      values = c(
        "Comfortable Hours" = set2_colors[1],
        "Violation Hours"   = set2_colors[2]
      ),
      breaks = c("Comfortable Hours", "Violation Hours")
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks_all) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = y_limits,
      breaks = y_breaks,
      labels = scales::comma
    ) +
    ggplot2::labs(
      title = NULL, subtitle = NULL,
      x = "Switch-over outdoor temperature (\u2103)",
      y = "Hours"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    )

  num_rows <- ceiling(length(valid_cities_in_order) / facet_ncol)
  if (is.null(plot_height)) {
    plot_height <- 1 + (num_rows * 2)
  }

  velocity_suffix <- paste0("_v_max_", gsub("\\.", "_", as.character(max_v)))
  safe_group_name <- gsub("[ ()]", "", current_plot_group)
  file_name <- paste0(
    "Comfort_Hours_Increment_Group_",
    safe_group_name, velocity_suffix, ".png"
  )

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ggplot2::ggsave(
    filename = file.path(output_dir, file_name),
    plot = p,
    width = plot_width,
    height = plot_height,
    dpi = 300
  )

  cat(paste("  > Saved plot to:", file.path(output_dir, file_name), "\n"))
}
