default_energy_temperature_shapes <- c(16, 17, 15, 18, 8, 3)

default_energy_plot_palette <- RColorBrewer::brewer.pal(n = 8, name = "Set2")

plot_energy_by_group <- function(
  data_subset,
  group_name,
  palette = default_energy_plot_palette,
  temperature_shapes = default_energy_temperature_shapes,
  x_breaks = seq(27, 32, by = 1),
  y_limits = c(60, 88),
  y_breaks = seq(60, 80, by = 10),
  x_label = NULL,
  y_label = NULL,
  base_size = 11,
  line_alpha = 0.9,
  line_width = 0.7,
  point_size = 1.8,
  point_alpha = 0.9,
  legend_position = "none",
  legend_nrow = 1,
  panel_grid_major_color = "grey94",
  panel_grid_minor = FALSE,
  title_size = 11,
  title_face = "bold",
  title_hjust = 0.5,
  title_margin_bottom = 5,
  show_y_axis = TRUE,
  show_x_axis = TRUE,
  inpanel_legend = TRUE,
  inpanel_legend_args = list(
    x_range = c(27, 32),
    y_range = c(60, 88),
    n_per_row = 2,
    x_pad_left = 0.3,
    row_gap = 2.4,
    text_size = 3.5
  )
) {
  p <- ggplot(data_subset, aes(x = Temperature, y = EnergyIntensity_kWh_per_m2, color = City, group = City)) +
    geom_line(alpha = line_alpha, size = line_width) +
    geom_point(aes(shape = as.factor(Temperature)), size = point_size, alpha = point_alpha) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks, oob = scales::squish) +
    scale_color_manual(values = palette) +
    scale_shape_manual(values = temperature_shapes) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = legend_position,
      panel.grid.minor = if (panel_grid_minor) element_line() else element_blank(),
      panel.grid.major = element_line(color = panel_grid_major_color),
      plot.title = element_text(
        hjust = title_hjust,
        size = title_size,
        face = title_face,
        margin = margin(b = title_margin_bottom)
      )
    ) +
    labs(title = group_name, x = x_label, y = y_label)

  if (!show_y_axis) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  if (!show_x_axis) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if (legend_position != "none") {
    p <- p + guides(color = guide_legend(nrow = legend_nrow), shape = guide_legend(nrow = legend_nrow))
  }

  if (inpanel_legend) {
    inpanel_args <- c(list(p = p, data_subset = data_subset), inpanel_legend_args)
    p <- do.call(add_inpanel_city_legend_top, inpanel_args)
  }

  p
}

plot_pareto_by_group <- function(
  data_subset,
  group_name,
  palette = default_energy_plot_palette,
  temperature_shapes = default_energy_temperature_shapes,
  x_limits = c(0, 0.1),
  x_breaks = seq(0, 0.1, by = 0.02),
  y_limits = c(0, 0.45),
  y_breaks = seq(0, 0.4, by = 0.1),
  x_label = NULL,
  y_label = NULL,
  shape_legend_name = "Switch-over outdoor temperature (\u2103)",
  base_size = 12,
  line_alpha = 0.9,
  point_size = 2,
  point_alpha = 0.9,
  legend_position = "top",
  shape_legend_nrow = 1,
  subtitle_size = 11,
  subtitle_face = "bold",
  subtitle_hjust = 0.5,
  panel_grid_minor = FALSE,
  show_y_axis = TRUE,
  show_x_axis = TRUE,
  city_legend = TRUE,
  city_legend_args = list(
    y_start = 0.02,
    y_step = 0.032,
    x_seg_start = 0.063,
    x_seg_end = 0.068,
    x_text = 0.070,
    segment_linewidth = 1.5,
    text_size = 3.2
  )
) {
  p <- ggplot(data_subset, aes(x = EnergyReductionRate, y = MeanViolationRate, color = City, group = City)) +
    geom_line(alpha = line_alpha) +
    geom_point(aes(shape = as.factor(Temperature)), size = point_size, alpha = point_alpha) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = x_limits,
      breaks = x_breaks
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = y_limits,
      breaks = y_breaks
    ) +
    scale_color_manual(values = palette) +
    scale_shape_manual(values = temperature_shapes, name = shape_legend_name) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = legend_position,
      panel.grid.minor = if (panel_grid_minor) element_line() else element_blank(),
      plot.subtitle = element_text(hjust = subtitle_hjust, size = subtitle_size, face = subtitle_face)
    ) +
    labs(x = x_label, y = y_label, subtitle = group_name) +
    guides(color = "none", shape = guide_legend(nrow = shape_legend_nrow))

  if (!show_y_axis) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  if (!show_x_axis) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if (city_legend) {
    label_df <- data_subset %>%
      distinct(City) %>%
      mutate(
        y_pos = city_legend_args$y_start + (row_number() - 1) * city_legend_args$y_step,
        x_seg_start = city_legend_args$x_seg_start,
        x_seg_end = city_legend_args$x_seg_end,
        x_text = city_legend_args$x_text
      )

    p <- p +
      geom_segment(
        data = label_df,
        aes(x = x_seg_start, xend = x_seg_end, y = y_pos, yend = y_pos, color = City),
        size = city_legend_args$segment_linewidth,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = label_df,
        aes(x = x_text, y = y_pos, label = City),
        color = "black",
        hjust = 0,
        size = city_legend_args$text_size,
        inherit.aes = FALSE
      )
  }

  p
}

build_energy_grid_plot <- function(
  master_data,
  plot_groups,
  panel_args = list(),
  ncol = 2,
  x_label = "Switch-over Outdoor Temperature (\u2103)",
  y_label = bquote("Energy consumption" ~ (kWh / m^2)),
  x_label_fontsize = 12,
  y_label_fontsize = 12,
  layout_widths = c(1, 25),
  layout_heights = c(25, 1)
) {
  if (length(plot_groups) != 4) {
    return(NULL)
  }

  plots_for_grid <- list()

  for (i in seq_along(plot_groups)) {
    group_name <- plot_groups[i]
    data_group <- master_data %>% dplyr::filter(PlotGroup == group_name)
    if (nrow(data_group) == 0) {
      next
    }

    plot_args <- c(
      list(
        data_subset = data_group,
        group_name = group_name,
        show_y_axis = !(i %in% c(2, 4)),
        show_x_axis = !(i %in% c(1, 2))
      ),
      panel_args
    )

    plots_for_grid[[group_name]] <- do.call(plot_energy_by_group, plot_args)
  }

  if (length(plots_for_grid) == 0) {
    return(NULL)
  }

  main_grid <- wrap_plots(plots_for_grid, ncol = ncol)
  y_label_plot <- wrap_elements(grid::textGrob(y_label, rot = 90, gp = grid::gpar(fontsize = y_label_fontsize)))
  x_label_plot <- wrap_elements(grid::textGrob(x_label, gp = grid::gpar(fontsize = x_label_fontsize)))

  (y_label_plot + main_grid + plot_layout(widths = layout_widths)) /
    x_label_plot + plot_layout(heights = layout_heights)
}

build_pareto_grid_plot <- function(
  master_data,
  plot_groups,
  panel_args = list(),
  ncol = 2,
  x_label = "Energy Reduction Rate",
  y_label = "Mean Violation Rate",
  x_label_fontsize = 12,
  y_label_fontsize = 12,
  layout_widths = c(1, 25),
  layout_heights = c(25, 1),
  legend_heights = c(1, 25),
  collected_legend_position = "top",
  collected_legend_title_size = 11,
  collected_legend_title_face = "bold",
  collected_legend_margin = margin(t = 5, b = -10)
) {
  if (length(plot_groups) != 4) {
    return(NULL)
  }

  plots_for_grid <- list()

  for (i in seq_along(plot_groups)) {
    group_name <- plot_groups[i]
    data_group <- master_data %>% dplyr::filter(PlotGroup == group_name)
    if (nrow(data_group) == 0) {
      next
    }

    plot_args <- c(
      list(
        data_subset = data_group,
        group_name = group_name,
        show_y_axis = !(i %in% c(2, 4)),
        show_x_axis = !(i %in% c(1, 2))
      ),
      panel_args
    )

    plots_for_grid[[group_name]] <- do.call(plot_pareto_by_group, plot_args)
  }

  if (length(plots_for_grid) == 0) {
    return(NULL)
  }

  main_grid <- wrap_plots(plots_for_grid, ncol = ncol)
  main_grid_with_legend <- (guide_area() / main_grid) +
    plot_layout(guides = "collect", heights = legend_heights) &
    theme(
      legend.position = collected_legend_position,
      legend.title = element_text(size = collected_legend_title_size, face = collected_legend_title_face),
      legend.margin = collected_legend_margin
    )

  y_label_plot <- wrap_elements(grid::textGrob(y_label, rot = 90, gp = grid::gpar(fontsize = y_label_fontsize)))
  x_label_plot <- wrap_elements(grid::textGrob(x_label, gp = grid::gpar(fontsize = x_label_fontsize)))

  (y_label_plot + main_grid_with_legend + plot_layout(widths = layout_widths)) /
    x_label_plot + plot_layout(heights = layout_heights)
}

save_energy_plot <- function(plot_obj, output_path, width, height, dpi, bg = "white") {
  if (is.null(plot_obj)) {
    return(invisible(NULL))
  }

  output_path <- ensure_parent_dir(output_path)
  ggsave(filename = output_path, plot = plot_obj, width = width, height = height, dpi = dpi, bg = bg)
  invisible(output_path)
}
