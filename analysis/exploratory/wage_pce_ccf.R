# Exploratory: does real (inflation-adjusted) wage growth lead real PCE
# growth, and by how many months?
#
# Standalone concept-testing script — NOT part of the collection/analysis
# pipeline (analysis/run.R), not referenced by SPEC.md. Reuses compute_yoy()
# from analysis/transforms.R for consistency with the production YoY method,
# but does its own gap handling (see interpolate_gap() below) rather than
# the pipeline's generic fill_gaps().
#
# Run from repo root, after outputs/bls_earnings.csv and outputs/bea_pce.csv
# have been populated by the Python collectors:
#   Rscript analysis/exploratory/wage_pce_ccf.R

source("analysis/transforms.R")

# ---- Config ----------------------------------------------------------------
# Everything tunable (paths, series IDs, lag range, segment boundaries,
# scatter lags, plot sizing) lives here — no magic constants below.
config <- list(
  input = list(
    bls_path = "outputs/bls_earnings.csv", # mirrors config.yaml bls.output_path
    bea_path = "outputs/bea_pce.csv", # mirrors config.yaml bea.output_path
    wage_series_id = "CES0500000013", # real avg hourly earnings, total private
    pce_series_id = "DPCERX" # real PCE level, Table 2.8.6 line 1
  ),
  # One known missing month in the source data (likely a government-
  # shutdown-delayed release). Set `date` once the real CSVs have been
  # inspected and the actual gap identified, e.g.:
  #   list(list(series_id = "DPCERX", date = as.Date("2019-01-01")))
  gaps = list(
    list(series_id = "DPCERX", date = NULL)
  ),
  ccf = list(
    max_lag = 12,
    significance_z = 1.96
  ),
  # Boundaries clamped to the actual observed date range at runtime (see
  # segment_bounds()) — a segment's start/end never need to match the
  # data's exact first/last month.
  segments = list(
    pre      = list(start = NULL,                 end = as.Date("2019-12-01")),
    pandemic = list(start = as.Date("2020-01-01"), end = as.Date("2022-12-01")),
    post     = list(start = as.Date("2023-01-01"), end = NULL)
  ),
  scatter_lags = c(0, 1, 2, 3, 6, 12),
  output = list(
    dir = "outputs/exploratory/wage_pce_ccf",
    width_in = 10,
    height_in = 6,
    dpi = 150
  )
)

dir.create(config$output$dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load raw series ---------------------------------------------------

col_spec <- readr::cols(
  series_id = readr::col_character(),
  date = readr::col_date(format = "%Y-%m-%d"),
  value = readr::col_double(),
  units = readr::col_character(),
  source = readr::col_character(),
  fetched_at = readr::col_character()
)

load_series <- function(path, series_id) {
  readr::read_csv(path, col_types = col_spec) |>
    dplyr::filter(series_id == !!series_id) |>
    dplyr::arrange(date) |>
    dplyr::select(series_id, date, value)
}

wage_raw <- load_series(config$input$bls_path, config$input$wage_series_id)
pce_raw <- load_series(config$input$bea_path, config$input$pce_series_id)

# ---- Gap handling -----------------------------------------------------

#' Complete a single-series df to one row per calendar month (min..max
#' observed date), exposing missing months as explicit NA `value` rows
#' rather than absent rows.
build_monthly_grid <- function(df) {
  grid <- tibble::tibble(date = seq(min(df$date), max(df$date), by = "1 month"))
  dplyr::left_join(grid, df, by = "date") |>
    dplyr::mutate(series_id = df$series_id[1])
}

#' Interpolate one specific known gap as the average of the immediately
#' preceding and following month's values. Deliberately explicit (not a
#' silent/generic NA-fill): prints which date was interpolated and the
#' value used. No-ops (with a message) if `gap_date` is NULL or the month
#' already has a value.
interpolate_gap <- function(df, gap_date, series_label) {
  if (is.null(gap_date)) {
    message(sprintf("interpolate_gap(): no known gap date configured for %s -- skipping", series_label))
    return(df)
  }
  idx <- which(df$date == gap_date)
  if (length(idx) != 1) {
    stop(sprintf("interpolate_gap(): %s not found in %s monthly grid", format(gap_date, "%Y-%m"), series_label))
  }
  if (!is.na(df$value[idx])) {
    message(sprintf("interpolate_gap(): %s already has a value for %s -- skipping", series_label, format(gap_date, "%Y-%m")))
    return(df)
  }
  if (idx <= 1 || idx >= nrow(df)) {
    stop(sprintf("interpolate_gap(): %s at %s has no bracketing months to average", series_label, format(gap_date, "%Y-%m")))
  }
  prev_val <- df$value[idx - 1]
  next_val <- df$value[idx + 1]
  filled <- mean(c(prev_val, next_val))
  df$value[idx] <- filled
  message(sprintf(
    "interpolate_gap(): filled %s %s = %.4f (average of %s = %.4f and %s = %.4f)",
    series_label, format(gap_date, "%Y-%m"), filled,
    format(df$date[idx - 1], "%Y-%m"), prev_val,
    format(df$date[idx + 1], "%Y-%m"), next_val
  ))
  df
}

#' Flag (print, do not fix) any months still missing after known-gap
#' handling. Run on both series before any lag/rolling computation.
assert_no_unexpected_gaps <- function(df, series_label) {
  missing <- df[is.na(df$value), ]
  if (nrow(missing) == 0) {
    message(sprintf("assert_no_unexpected_gaps(): %s -- no unexpected gaps", series_label))
  } else {
    message(sprintf("assert_no_unexpected_gaps(): %s -- %d unexpected missing month(s):", series_label, nrow(missing)))
    message(paste(sprintf("  - %s", format(missing$date, "%Y-%m")), collapse = "\n"))
  }
  invisible(df)
}

wage_grid <- build_monthly_grid(wage_raw)
pce_grid <- build_monthly_grid(pce_raw)

for (gap in config$gaps) {
  if (identical(gap$series_id, config$input$wage_series_id)) {
    wage_grid <- interpolate_gap(wage_grid, gap$date, "wage (CES0500000013)")
  } else if (identical(gap$series_id, config$input$pce_series_id)) {
    pce_grid <- interpolate_gap(pce_grid, gap$date, "PCE (DPCERX)")
  }
}

assert_no_unexpected_gaps(wage_grid, "wage (CES0500000013)")
assert_no_unexpected_gaps(pce_grid, "PCE (DPCERX)")

# ---- YoY ----------------------------------------------------------------
# Reuses transforms.R::compute_yoy() (percent method, positional 12-month
# lag) -- valid here because build_monthly_grid() + interpolate_gap() above
# guarantee each series is a regular, gap-free one-row-per-month grid.

yoy_long <- dplyr::bind_rows(wage_grid, pce_grid) |>
  compute_yoy(method = "percent")

wide <- yoy_long |>
  dplyr::select(date, series_id, yoy) |>
  tidyr::pivot_wider(names_from = series_id, values_from = yoy) |>
  dplyr::arrange(date)

names(wide)[names(wide) == config$input$wage_series_id] <- "real_wage_yoy"
names(wide)[names(wide) == config$input$pce_series_id] <- "real_pce_yoy"
wide <- dplyr::rename(wide, Date = date)

# Complete cases only (drops each series' leading 12-month YoY warm-up
# window). Used for CCF/segment analysis, where equal monthly spacing must
# hold; the lagged-scatter grid below instead lags on the full `wide` grid
# so each lag step stays a true calendar-month lag.
complete_wide <- dplyr::filter(wide, !is.na(real_wage_yoy), !is.na(real_pce_yoy))

# ---- CCF: full sample -------------------------------------------------

#' Tidy cross-correlation table between real wage YoY and real PCE YoY.
#'
#' Positive lag convention: wage leads PCE at lag k means
#' cor(wage_t, PCE_{t+k}). stats::ccf(x, y) reports, at lag k,
#' cor(x_{t+k}, y_t); calling it with x = pce, y = wage therefore gives
#' cor(pce_{t+k}, wage_t) == cor(wage_t, pce_{t+k}) (correlation is
#' symmetric) -- i.e. exactly the wage-leads-PCE-at-lag-k convention, with
#' no sign flip needed.
compute_ccf_table <- function(wage, pce, max_lag) {
  ccf_obj <- stats::ccf(pce, wage, lag.max = max_lag, plot = FALSE)
  tibble::tibble(
    lag = as.integer(ccf_obj$lag[, 1, 1]),
    correlation = as.numeric(ccf_obj$acf[, 1, 1])
  )
}

n_full <- nrow(complete_wide)
ccf_full <- compute_ccf_table(complete_wide$real_wage_yoy, complete_wide$real_pce_yoy, config$ccf$max_lag)

peak_idx_full <- which.max(abs(ccf_full$correlation))
peak_lag_full <- ccf_full$lag[peak_idx_full]
peak_corr_full <- ccf_full$correlation[peak_idx_full]
message(sprintf(
  "Full-sample CCF: peak |correlation| at lag %+d month(s), r = %.4f (n = %d)",
  peak_lag_full, peak_corr_full, n_full
))

sig_band <- config$ccf$significance_z / sqrt(n_full)

#' Bar plot of a CCF table: correlation vs. lag, dashed significance band,
#' the peak-|r| lag highlighted, titled/labeled/legended.
plot_ccf <- function(ccf_df, sig_band, peak_lag, title, subtitle) {
  peak_row <- dplyr::filter(ccf_df, lag == peak_lag)
  band_df <- tibble::tibble(y = c(-sig_band, sig_band))
  peak_label <- sprintf("Peak |r| at lag %+d mo.", peak_lag)

  ggplot2::ggplot(ccf_df, ggplot2::aes(x = lag, y = correlation)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(fill = "Cross-correlation (real wage YoY vs. real PCE YoY)"), width = 0.6) +
    ggplot2::geom_hline(
      data = band_df,
      ggplot2::aes(yintercept = y, linetype = "±95% significance threshold"),
      color = "grey40"
    ) +
    ggplot2::geom_point(data = peak_row, ggplot2::aes(color = peak_label), size = 3) +
    ggplot2::scale_fill_manual(name = NULL, values = c("Cross-correlation (real wage YoY vs. real PCE YoY)" = "#377eb8")) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("±95% significance threshold" = "dashed")) +
    ggplot2::scale_color_manual(name = NULL, values = setNames("#e41a1c", peak_label)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Lag (months); positive = real wage growth leads real PCE growth",
      y = "Cross-correlation (real wage YoY vs. real PCE YoY)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

p_ccf_full <- plot_ccf(
  ccf_full, sig_band, peak_lag_full,
  title = "Real Wage Growth vs. Real PCE Growth -- Cross-Correlation (Full Sample)",
  subtitle = "Real Wage Growth (YoY %) vs. Real PCE Growth (YoY %)"
)
ggplot2::ggsave(
  file.path(config$output$dir, "ccf_full_sample.png"),
  plot = p_ccf_full, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

# ---- CCF: multi-segment robustness check --------------------------------
# Given known volatility around the pandemic, split rather than a simple
# half/half: pre-2020, a pandemic/recovery window (2020-2022), post-2022.

#' Resolve a segment's start/end against the actual observed date range
#' (NULL bound = open-ended, clamped to the data's min/max Date).
segment_bounds <- function(seg, data_min, data_max) {
  list(
    start = if (is.null(seg$start)) data_min else max(seg$start, data_min),
    end = if (is.null(seg$end)) data_max else min(seg$end, data_max)
  )
}

#' CCF for one segment; skips (with a warning) a segment too short to
#' support the requested max_lag.
compute_ccf_segment <- function(df, bounds, max_lag, label) {
  seg_df <- df |>
    dplyr::filter(Date >= bounds$start, Date <= bounds$end) |>
    dplyr::filter(!is.na(real_wage_yoy), !is.na(real_pce_yoy))
  min_rows <- 2 * max_lag + 1
  if (nrow(seg_df) < min_rows) {
    warning(sprintf(
      "Skipping segment '%s' (%s to %s): only %d complete month(s), need >= %d for lag.max = %d",
      label, format(bounds$start, "%Y-%m"), format(bounds$end, "%Y-%m"), nrow(seg_df), min_rows, max_lag
    ), call. = FALSE)
    return(NULL)
  }
  compute_ccf_table(seg_df$real_wage_yoy, seg_df$real_pce_yoy, max_lag) |>
    dplyr::mutate(segment = label, n = nrow(seg_df), .before = 1)
}

data_min <- min(complete_wide$Date)
data_max <- max(complete_wide$Date)

segment_results <- purrr::imap(config$segments, function(seg, label) {
  bounds <- segment_bounds(seg, data_min, data_max)
  compute_ccf_segment(complete_wide, bounds, config$ccf$max_lag, label)
})
ccf_segments <- dplyr::bind_rows(purrr::compact(segment_results)) |>
  dplyr::mutate(segment = factor(segment, levels = names(config$segments)))

segment_summary <- ccf_segments |>
  dplyr::group_by(segment) |>
  dplyr::slice_max(abs(correlation), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(segment, n, peak_lag = lag, peak_correlation = correlation)

cat("\nMulti-segment CCF summary (peak |correlation| per segment):\n")
print(segment_summary)

p_ccf_segments <- ggplot2::ggplot(ccf_segments, ggplot2::aes(x = lag, y = correlation, color = segment, linetype = segment)) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::labs(
    title = "Real Wage Growth vs. Real PCE Growth -- Cross-Correlation by Regime",
    subtitle = "Real Wage Growth (YoY %) vs. Real PCE Growth (YoY %); pre-2020 / pandemic-recovery (2020-2022) / post-2022",
    x = "Lag (months); positive = real wage growth leads real PCE growth",
    y = "Cross-correlation (real wage YoY vs. real PCE YoY)",
    color = "Segment",
    linetype = "Segment"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(config$output$dir, "ccf_by_segment.png"),
  plot = p_ccf_segments, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

# ---- Lagged scatter grid -------------------------------------------------
# Lags applied to the full (contiguous, pre-filter) `wide` grid so each lag
# step is a true calendar-month lag, then NA rows are dropped per lag.

scatter_long <- purrr::map_dfr(config$scatter_lags, function(k) {
  wide |>
    dplyr::arrange(Date) |>
    dplyr::mutate(real_wage_yoy_lag = dplyr::lag(real_wage_yoy, k)) |>
    dplyr::transmute(Date, lag = k, real_wage_yoy_lag, real_pce_yoy)
}) |>
  tidyr::drop_na(real_wage_yoy_lag, real_pce_yoy) |>
  dplyr::mutate(lag_label = factor(
    sprintf("Lag = %d mo.", lag),
    levels = sprintf("Lag = %d mo.", config$scatter_lags)
  ))

facet_cor <- scatter_long |>
  dplyr::group_by(lag_label) |>
  dplyr::summarise(r = cor(real_wage_yoy_lag, real_pce_yoy, use = "complete.obs"), .groups = "drop") |>
  dplyr::mutate(label = sprintf("r = %.2f", r))

cat("\nLagged scatter correlations (real_wage_yoy_{t-k} vs. real_pce_yoy_t):\n")
print(dplyr::select(facet_cor, lag_label, r))

p_scatter <- ggplot2::ggplot(scatter_long, ggplot2::aes(x = real_wage_yoy_lag, y = real_pce_yoy)) +
  ggplot2::geom_point(ggplot2::aes(color = "Monthly observation"), alpha = 0.6, size = 1.5) +
  ggplot2::geom_smooth(ggplot2::aes(linetype = "OLS fit"), method = "lm", se = FALSE, color = "#e41a1c", linewidth = 0.7) +
  ggplot2::geom_text(
    data = facet_cor,
    mapping = ggplot2::aes(x = -Inf, y = Inf, label = label),
    hjust = -0.15, vjust = 1.5, inherit.aes = FALSE, size = 3.5
  ) +
  ggplot2::facet_wrap(~lag_label) +
  ggplot2::scale_color_manual(name = NULL, values = c("Monthly observation" = "#377eb8")) +
  ggplot2::scale_linetype_manual(name = NULL, values = c("OLS fit" = "solid")) +
  ggplot2::labs(
    title = "Lagged Real Wage Growth vs. Real PCE Growth",
    subtitle = "Real Wage Growth (YoY %), lagged k months, vs. Real PCE Growth (YoY %) at month t",
    x = "Real Wage Growth (YoY %), lagged k months",
    y = "Real PCE Growth (YoY %)"
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(config$output$dir, "lagged_scatter_grid.png"),
  plot = p_scatter, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

message(sprintf("Wrote 3 plots to %s/", config$output$dir))
