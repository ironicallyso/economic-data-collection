#' Build a YoY + trailing-MA line plot for one series.
#'
#' df_one_series must already have yoy/yoy_ma columns and a single series_id.
plot_yoy_series <- function(df_one_series, label, method, plot_cfg) {
  y_label <- if (method == "percent") "YoY change (%)" else "YoY change (log points)"
  method_label <- if (method == "percent") "percent" else "log difference"

  plot_long <- df_one_series |>
    dplyr::select(date, yoy, yoy_ma) |>
    tidyr::pivot_longer(c(yoy, yoy_ma), names_to = "metric", values_to = "metric_value") |>
    dplyr::mutate(metric = dplyr::recode(metric, yoy = "YoY", yoy_ma = "3-mo MA"))

  p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = date, y = metric_value, color = metric)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("YoY" = "#377eb8", "3-mo MA" = "#e41a1c")) +
    ggplot2::labs(
      title = label,
      caption = paste("YoY method:", method_label),
      x = "Month",
      y = y_label,
      color = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (method == "percent") {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1))
  }

  p
}

#' Save a plot using dimensions/dpi/output_dir from config; returns the filename.
save_plot <- function(plot_obj, source, series_id, method, plot_cfg) {
  dir.create(plot_cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(
    plot_cfg$output_dir,
    sprintf("%s_%s_yoy_%s.png", tolower(source), series_id, method)
  )
  ggplot2::ggsave(
    filename,
    plot = plot_obj,
    width = plot_cfg$width_in,
    height = plot_cfg$height_in,
    dpi = plot_cfg$dpi
  )
  filename
}
