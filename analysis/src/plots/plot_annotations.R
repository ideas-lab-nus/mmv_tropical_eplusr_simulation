add_inpanel_city_legend_top <- function(p, data_subset, x_range, y_range, n_per_row = 2,
                                        x_pad_left = 0.3, x_pad_right = 0.2, y_top_offset = 0.1,
                                        row_gap = 2.5, seg_len = 0.3, text_gap = 0.1,
                                        text_size = 3.2) {
  cities <- unique(data_subset$City)

  available_width <- (x_range[2] - x_range[1]) - x_pad_left - x_pad_right
  col_width <- available_width / n_per_row

  legend_df <- data.frame(City = cities) %>%
    mutate(
      idx = row_number() - 1,
      row = idx %/% n_per_row,
      col = idx %% n_per_row
    ) %>%
    mutate(
      y_pos = y_range[2] - y_top_offset - (row * row_gap) - 1.0,
      x_start = x_range[1] + x_pad_left + (col * col_width),
      x_end = x_start + seg_len,
      x_text = x_end + text_gap
    )

  p <- p +
    geom_segment(
      data = legend_df,
      aes(x = x_start, xend = x_end, y = y_pos, yend = y_pos, color = City),
      size = 1.2, inherit.aes = FALSE
    ) +
    geom_text(
      data = legend_df,
      aes(x = x_text, y = y_pos, label = City),
      hjust = 0, vjust = 0.5, size = text_size, color = "black", inherit.aes = FALSE
    )

  p
}
