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

  # Interpolated months: marked on the YoY line so filled values are never
  # mistaken for real observations. `imputed` is added by fill_gaps().
  imputed_pts <- if ("imputed" %in% names(df_one_series)) {
    df_one_series |>
      dplyr::filter(imputed, !is.na(yoy)) |>
      dplyr::select(date, metric_value = yoy)
  } else {
    df_one_series[0, c("date", "yoy")] |> dplyr::rename(metric_value = yoy)
  }

  caption <- paste("YoY method:", method_label)
  if (nrow(imputed_pts) > 0) {
    caption <- paste0(caption, " • open markers = interpolated months")
  }

  p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = date, y = metric_value, color = metric)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(
      data = imputed_pts,
      mapping = ggplot2::aes(x = date, y = metric_value),
      inherit.aes = FALSE,
      shape = 21, fill = "white", color = "#377eb8", size = 2, stroke = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(values = c("YoY" = "#377eb8", "3-mo MA" = "#e41a1c")) +
    ggplot2::labs(
      title = label,
      caption = caption,
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

#' TRUE when running interactively inside RStudio (vs. Rscript/CI).
is_rstudio <- function() {
  identical(Sys.getenv("RSTUDIO"), "1")
}

#' Print a plot to the RStudio Plots pane; no-op outside RStudio so headless
#' `Rscript` runs never block on or open a graphics device.
display_plot <- function(p) {
  if (is_rstudio()) print(p)
  invisible(p)
}

#' Filter a single-series df to the last `years` years of its own date range.
#' No-op if `years` is NULL/empty or the df has no rows.
truncate_recent <- function(df, years) {
  if (is.null(years) || nrow(df) == 0) {
    return(df)
  }
  cutoff <- max(df$date) - lubridate::years(years)
  dplyr::filter(df, date >= cutoff)
}

#' Save a plot using dimensions/dpi/output_dir from config; returns the filename.
#' `suffix` (e.g. "_last10y") distinguishes truncated variants from the full-
#' history file so they don't collide on disk.
save_plot <- function(plot_obj, source, series_id, method, plot_cfg, suffix = "") {
  dir.create(plot_cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(
    plot_cfg$output_dir,
    sprintf("%s_%s_yoy_%s%s.png", tolower(source), series_id, method, suffix)
  )
  ggplot2::ggsave(
    filename,
    plot = plot_obj,
    width = plot_cfg$width_in,
    height = plot_cfg$height_in,
    dpi = plot_cfg$dpi
  )
  display_plot(plot_obj)
  filename
}

#' Dual-axis overlay of two series' 3-month MA of YoY (left axis = df_left,
#' right axis = df_right), rescaled so both lines share the plotting range.
#' Raw `yoy` is intentionally excluded — the whole point of this plot is to
#' cut noise via the moving average, not show both metrics' detail again.
plot_combined_series <- function(df_left, df_right, label, plot_cfg) {
  left <- df_left |>
    dplyr::filter(!is.na(yoy_ma)) |>
    dplyr::select(date, left_ma = yoy_ma)
  right <- df_right |>
    dplyr::filter(!is.na(yoy_ma)) |>
    dplyr::select(date, right_ma = yoy_ma)

  joined <- dplyr::inner_join(left, right, by = "date")

  left_range <- range(joined$left_ma)
  right_range <- range(joined$right_ma)
  scale <- diff(left_range) / diff(right_range)
  shift <- left_range[1] - right_range[1] * scale

  joined <- dplyr::mutate(joined, right_scaled = right_ma * scale + shift)

  left_label <- df_left$label[1]
  right_label <- df_right$label[1]

  p <- ggplot2::ggplot(joined, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = left_ma, color = left_label)) +
    ggplot2::geom_line(ggplot2::aes(y = right_scaled, color = right_label)) +
    ggplot2::scale_color_manual(values = setNames(c("#377eb8", "#e41a1c"), c(left_label, right_label))) +
    ggplot2::scale_y_continuous(
      name = paste(left_label, "— YoY (3-mo MA)"),
      labels = scales::label_percent(accuracy = 0.1),
      sec.axis = ggplot2::sec_axis(
        ~ (. - shift) / scale,
        name = paste(right_label, "— YoY (3-mo MA)"),
        labels = scales::label_percent(accuracy = 0.1)
      )
    ) +
    ggplot2::labs(title = label, x = "Month", color = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  p
}

#' Save a combined plot; mirrors save_plot() but with a two-series filename.
save_combined_plot <- function(plot_obj, left_id, right_id, suffix, plot_cfg) {
  dir.create(plot_cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(
    plot_cfg$output_dir,
    sprintf("combined_%s_%s_yoy_ma%s.png", left_id, right_id, suffix)
  )
  ggplot2::ggsave(
    filename,
    plot = plot_obj,
    width = plot_cfg$width_in,
    height = plot_cfg$height_in,
    dpi = plot_cfg$dpi
  )
  display_plot(plot_obj)
  filename
}
