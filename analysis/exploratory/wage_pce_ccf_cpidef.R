# Exploratory: consistent-deflator follow-up to wage_pce_ccf.R.
#
# wage_pce_ccf.R cross-correlates real wage growth (CES0500000013, deflated
# by BLS using CPI-U) against real PCE growth using BEA's chain-type real
# PCE level (DPCERX, Table 2.8.6) -- two different deflators (CPI-U vs.
# BEA's PCE chain-type price index). This script builds a second real-PCE
# series deflated by the SAME index as wages (CPI-U) and re-runs the CCF /
# segment / lagged-scatter analysis, so the two deflator choices can be
# compared directly on identical wage data.
#
# Standalone concept-testing script -- NOT part of the collection/analysis
# pipeline (analysis/run.R), not referenced by SPEC.md. Reuses compute_yoy()
# from analysis/transforms.R for consistency with the production YoY method,
# but does its own gap handling and CCF helpers (duplicated from
# wage_pce_ccf.R rather than sourced from it) to stay a self-contained
# concept script, matching that script's own stated pattern.
#
# Run from repo root, after outputs/bls_earnings.csv and outputs/bea_pce.csv
# have been populated by the Python collectors (config.yaml now includes
# CUSR0000SA0 under bls.series and BEA Table 2.8.5 under bea.tables):
#   Rscript analysis/exploratory/wage_pce_ccf_cpidef.R

source("analysis/transforms.R")

# ---- Config ----------------------------------------------------------------
# Everything tunable lives here -- no magic constants below. Segment
# boundaries and scatter lags are copied verbatim from wage_pce_ccf.R's
# config list so the two scripts are directly comparable.
config <- list(
  input = list(
    bls_path = "outputs/bls_earnings.csv", # mirrors config.yaml bls.output_path
    bea_path = "outputs/bea_pce.csv", # mirrors config.yaml bea.output_path
    wage_series_id = "CES0500000013", # real avg hourly earnings, total private (CPI-deflated by BLS)
    pce_series_id = "DPCERX", # OLD deflator: real PCE level, Table 2.8.6 (BEA chain-type PCE price index)
    nominal_pce_series_id = "DPCERC", # NEW deflator input: nominal PCE level, Table 2.8.5 (current dollars) -- confirmed
    # against the live BEA API response (T20805, line 1); NOT the same table as pce_series_id above.
    cpi_series_id = "CUSR0000SA0" # NEW deflator: CPI-U, all items, SA -- same index that already deflates wages
  ),
  # Known missing months identified by inspecting the collected CSVs
  # (mirrors wage_pce_ccf.R's interpolate_gap() pattern). CUSR0000SA0 and
  # CES0500000013 both have exactly one missing month, 2025-10-01 (a
  # BLS-shutdown-delayed release affecting both BLS series identically).
  # We interpolate it for the new CPI-U input here, same as wage_pce_ccf.R
  # would for any single-month gap; the wage series' own copy of this same
  # gap is deliberately left untouched (no gap entry below) to keep
  # real_wage_yoy byte-for-byte identical to wage_pce_ccf.R's version, so
  # any difference in CCF results is attributable only to the deflator
  # choice, not to different wage-side gap handling.
  gaps = list(
    list(series_id = "DPCERC", date = NULL), # inspected: no gap found in the observed range
    list(series_id = "CUSR0000SA0", date = as.Date("2025-10-01"))
  ),
  ccf = list(
    max_lag = 12,
    significance_z = 1.96
  ),
  # Identical boundaries to wage_pce_ccf.R, clamped to the actual observed
  # date range at runtime (see segment_bounds()).
  segments = list(
    pre      = list(start = NULL,                 end = as.Date("2019-12-01")),
    pandemic = list(start = as.Date("2020-01-01"), end = as.Date("2022-12-01")),
    post     = list(start = as.Date("2023-01-01"), end = NULL)
  ),
  scatter_lags = c(0, 1, 2, 3, 6, 12),
  # Same output subdirectory as wage_pce_ccf.R -- new plots land alongside
  # the originals for easy side-by-side viewing, distinguished by filename.
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
nominal_pce_raw <- load_series(config$input$bea_path, config$input$nominal_pce_series_id)
cpi_raw <- load_series(config$input$bls_path, config$input$cpi_series_id)

# ---- Gap handling -----------------------------------------------------
# Same functions as wage_pce_ccf.R, duplicated here (not sourced) to keep
# this script self-contained. Applied fresh to all four series, including
# the two new inputs -- gap handling from the prior script's run does not
# carry over automatically.

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
#' handling. Run on all four series before any lag/rolling computation.
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
nominal_pce_grid <- build_monthly_grid(nominal_pce_raw)
cpi_grid <- build_monthly_grid(cpi_raw)

for (gap in config$gaps) {
  if (identical(gap$series_id, config$input$nominal_pce_series_id)) {
    nominal_pce_grid <- interpolate_gap(nominal_pce_grid, gap$date, "nominal PCE (DPCERC)")
  } else if (identical(gap$series_id, config$input$cpi_series_id)) {
    cpi_grid <- interpolate_gap(cpi_grid, gap$date, "CPI-U (CUSR0000SA0)")
  }
}

assert_no_unexpected_gaps(wage_grid, "wage (CES0500000013)")
assert_no_unexpected_gaps(pce_grid, "PCE, index-deflated (DPCERX)")
assert_no_unexpected_gaps(nominal_pce_grid, "nominal PCE (DPCERC)")
assert_no_unexpected_gaps(cpi_grid, "CPI-U (CUSR0000SA0)")

# ---- Construct the CPI-deflated real PCE level --------------------------
# real_pce_cpidef = nominal PCE / (CPI-U / 100), i.e. nominal PCE re-based
# into CPI-U's 1982-84 dollar terms -- the same deflation convention BLS
# already applies to CES0500000013. Restricted (inner join) to the date
# range where both nominal PCE and CPI-U have observations, since CPI-U's
# series only starts 2006-01 versus nominal PCE's 1959-01.
#
# IMPORTANT: this deliberately will NOT reproduce BEA's published real PCE
# (DPCERX / Table 2.8.6). BEA computes real PCE using Fisher chain-type
# indexing with its own PCE-specific deflator (Table 2.8.4), which differs
# methodologically from a simple nominal-level / CPI-U ratio. That
# divergence is the entire point of this script -- it isolates the effect
# of deflator choice on the wage-PCE lead/lag relationship. Expected, not
# a bug.
real_pce_cpidef_grid <- dplyr::inner_join(
  dplyr::select(nominal_pce_grid, date, nominal_value = value),
  dplyr::select(cpi_grid, date, cpi_value = value),
  by = "date"
) |>
  dplyr::mutate(
    series_id = "real_pce_cpidef",
    value = nominal_value / (cpi_value / 100)
  ) |>
  dplyr::select(series_id, date, value)

# ---- YoY ----------------------------------------------------------------
# Reuses transforms.R::compute_yoy() (percent method, positional 12-month
# lag) -- valid here because build_monthly_grid()/interpolate_gap() above
# and the inner join above guarantee each series is a regular,
# (at-most-one-remaining-gap) one-row-per-month grid.

yoy_long <- dplyr::bind_rows(wage_grid, pce_grid, real_pce_cpidef_grid) |>
  compute_yoy(method = "percent")

wide <- yoy_long |>
  dplyr::select(date, series_id, yoy) |>
  tidyr::pivot_wider(names_from = series_id, values_from = yoy) |>
  dplyr::arrange(date)

names(wide)[names(wide) == config$input$wage_series_id] <- "real_wage_yoy"
names(wide)[names(wide) == config$input$pce_series_id] <- "real_pce_yoy"
names(wide)[names(wide) == "real_pce_cpidef"] <- "real_pce_yoy_cpidef"
wide <- dplyr::rename(wide, Date = date)

# Complete cases across ALL THREE yoy columns (not just the pair needed for
# one CCF) -- this pins the old-deflator and new-deflator CCFs to the exact
# same date range and n, so any difference between them is attributable
# only to the deflator choice, not to a different sample window (DPCERX
# starts later than the nominal-PCE/CPI-U combination).
complete_wide <- dplyr::filter(
  wide,
  !is.na(real_wage_yoy), !is.na(real_pce_yoy), !is.na(real_pce_yoy_cpidef)
)

# ---- CCF: full sample, old vs. new deflator ------------------------------

#' Tidy cross-correlation table between real wage YoY and a real PCE YoY
#' series. Positive lag convention: wage leads PCE at lag k means
#' cor(wage_t, PCE_{t+k}). stats::ccf(x, y) reports, at lag k,
#' cor(x_{t+k}, y_t); calling it with x = pce, y = wage therefore gives
#' cor(pce_{t+k}, wage_t) == cor(wage_t, pce_{t+k}) (correlation is
#' symmetric) -- i.e. exactly the wage-leads-PCE-at-lag-k convention, with
#' no sign flip needed. Identical convention/derivation to wage_pce_ccf.R.
compute_ccf_table <- function(wage, pce, max_lag) {
  ccf_obj <- stats::ccf(pce, wage, lag.max = max_lag, plot = FALSE)
  tibble::tibble(
    lag = as.integer(ccf_obj$lag[, 1, 1]),
    correlation = as.numeric(ccf_obj$acf[, 1, 1])
  )
}

n_full <- nrow(complete_wide)
ccf_full_old <- compute_ccf_table(complete_wide$real_wage_yoy, complete_wide$real_pce_yoy, config$ccf$max_lag)
ccf_full_new <- compute_ccf_table(complete_wide$real_wage_yoy, complete_wide$real_pce_yoy_cpidef, config$ccf$max_lag)

peak_idx_old <- which.max(abs(ccf_full_old$correlation))
peak_lag_old <- ccf_full_old$lag[peak_idx_old]
peak_corr_old <- ccf_full_old$correlation[peak_idx_old]

peak_idx_new <- which.max(abs(ccf_full_new$correlation))
peak_lag_new <- ccf_full_new$lag[peak_idx_new]
peak_corr_new <- ccf_full_new$correlation[peak_idx_new]

message(sprintf(
  "Full-sample CCF [OLD deflator, PCE price index]: peak |correlation| at lag %+d month(s), r = %.4f (n = %d)",
  peak_lag_old, peak_corr_old, n_full
))
message(sprintf(
  "Full-sample CCF [NEW deflator, CPI-U]: peak |correlation| at lag %+d month(s), r = %.4f (n = %d)",
  peak_lag_new, peak_corr_new, n_full
))

sig_band <- config$ccf$significance_z / sqrt(n_full)

#' Bar plot of a single CCF table: correlation vs. lag, dashed significance
#' band, the peak-|r| lag highlighted, titled/labeled/legended. Identical
#' to wage_pce_ccf.R's plot_ccf().
plot_ccf <- function(ccf_df, sig_band, peak_lag, title, subtitle) {
  peak_row <- dplyr::filter(ccf_df, lag == peak_lag)
  band_df <- tibble::tibble(y = c(-sig_band, sig_band))
  peak_label <- sprintf("Peak |r| at lag %+d mo.", peak_lag)

  ggplot2::ggplot(ccf_df, ggplot2::aes(x = lag, y = correlation)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(fill = "Cross-correlation (real wage YoY vs. real PCE YoY, CPI-deflated)"), width = 0.6) +
    ggplot2::geom_hline(
      data = band_df,
      ggplot2::aes(yintercept = y, linetype = "±95% significance threshold"),
      color = "grey40"
    ) +
    ggplot2::geom_point(data = peak_row, ggplot2::aes(color = peak_label), size = 3) +
    ggplot2::scale_fill_manual(name = NULL, values = c("Cross-correlation (real wage YoY vs. real PCE YoY, CPI-deflated)" = "#377eb8")) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("±95% significance threshold" = "dashed")) +
    ggplot2::scale_color_manual(name = NULL, values = setNames("#e41a1c", peak_label)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Lag (months); positive = real wage growth leads real PCE growth",
      y = "Cross-correlation (real wage YoY vs. real PCE YoY, CPI-deflated)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

p_ccf_full_new <- plot_ccf(
  ccf_full_new, sig_band, peak_lag_new,
  title = "Real Wage Growth vs. CPI-Deflated Real PCE Growth -- Cross-Correlation (Full Sample)",
  subtitle = "Real Wage Growth (YoY %) vs. CPI-U-Deflated Real PCE Growth (YoY %)"
)
ggplot2::ggsave(
  file.path(config$output$dir, "ccf_full_sample_cpidef.png"),
  plot = p_ccf_full_new, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

#' Overlay plot: old-deflator vs. new-deflator full-sample CCF curves on one
#' set of axes, for direct visual comparison.
ccf_compare <- dplyr::bind_rows(
  dplyr::mutate(ccf_full_old, deflator = "Old: PCE price index (Table 2.8.4 / DPCERX)"),
  dplyr::mutate(ccf_full_new, deflator = "New: CPI-U (consistent with wages)")
) |>
  dplyr::mutate(deflator = factor(deflator, levels = c(
    "Old: PCE price index (Table 2.8.4 / DPCERX)",
    "New: CPI-U (consistent with wages)"
  )))

band_df <- tibble::tibble(y = c(-sig_band, sig_band))

p_ccf_compare <- ggplot2::ggplot(ccf_compare, ggplot2::aes(x = lag, y = correlation, color = deflator, linetype = deflator)) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::geom_hline(
    data = band_df,
    ggplot2::aes(yintercept = y),
    color = "grey40", linetype = "dashed"
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_color_manual(name = "Real PCE deflator", values = c(
    "Old: PCE price index (Table 2.8.4 / DPCERX)" = "#377eb8",
    "New: CPI-U (consistent with wages)" = "#e41a1c"
  )) +
  ggplot2::scale_linetype_manual(name = "Real PCE deflator", values = c(
    "Old: PCE price index (Table 2.8.4 / DPCERX)" = "solid",
    "New: CPI-U (consistent with wages)" = "dashed"
  )) +
  ggplot2::labs(
    title = "Real Wage Growth vs. Real PCE Growth -- CCF by Deflator Choice (Full Sample)",
    subtitle = sprintf(
      "Dashed grey = ±95%% significance threshold (n = %d); same wage series and date range in both curves",
      n_full
    ),
    x = "Lag (months); positive = real wage growth leads real PCE growth",
    y = "Cross-correlation (real wage YoY vs. real PCE YoY)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(config$output$dir, "ccf_deflator_comparison.png"),
  plot = p_ccf_compare, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

# ---- CCF: multi-segment robustness check, old vs. new deflator ----------
# Same three segments as wage_pce_ccf.R: pre-2020, pandemic/recovery
# (2020-2022), post-2022. Computed for both deflators on the same
# complete_wide date range so segment-level comparisons are also
# apples-to-apples.

#' Resolve a segment's start/end against the actual observed date range
#' (NULL bound = open-ended, clamped to the data's min/max Date). Identical
#' to wage_pce_ccf.R's segment_bounds().
segment_bounds <- function(seg, data_min, data_max) {
  list(
    start = if (is.null(seg$start)) data_min else max(seg$start, data_min),
    end = if (is.null(seg$end)) data_max else min(seg$end, data_max)
  )
}

#' CCF for one segment, for a given (wage, pce) column pair; skips (with a
#' warning) a segment too short to support the requested max_lag. Identical
#' to wage_pce_ccf.R's compute_ccf_segment(), generalized to a configurable
#' pce column so it can be reused for both deflators.
compute_ccf_segment <- function(df, bounds, max_lag, label, pce_col) {
  seg_df <- df |>
    dplyr::filter(Date >= bounds$start, Date <= bounds$end) |>
    dplyr::filter(!is.na(real_wage_yoy), !is.na(.data[[pce_col]]))
  min_rows <- 2 * max_lag + 1
  if (nrow(seg_df) < min_rows) {
    warning(sprintf(
      "Skipping segment '%s' (%s to %s): only %d complete month(s), need >= %d for lag.max = %d",
      label, format(bounds$start, "%Y-%m"), format(bounds$end, "%Y-%m"), nrow(seg_df), min_rows, max_lag
    ), call. = FALSE)
    return(NULL)
  }
  compute_ccf_table(seg_df$real_wage_yoy, seg_df[[pce_col]], max_lag) |>
    dplyr::mutate(segment = label, n = nrow(seg_df), .before = 1)
}

data_min <- min(complete_wide$Date)
data_max <- max(complete_wide$Date)

segment_results_old <- purrr::imap(config$segments, function(seg, label) {
  bounds <- segment_bounds(seg, data_min, data_max)
  compute_ccf_segment(complete_wide, bounds, config$ccf$max_lag, label, "real_pce_yoy")
})
ccf_segments_old <- dplyr::bind_rows(purrr::compact(segment_results_old)) |>
  dplyr::mutate(segment = factor(segment, levels = names(config$segments)))

segment_results_new <- purrr::imap(config$segments, function(seg, label) {
  bounds <- segment_bounds(seg, data_min, data_max)
  compute_ccf_segment(complete_wide, bounds, config$ccf$max_lag, label, "real_pce_yoy_cpidef")
})
ccf_segments_new <- dplyr::bind_rows(purrr::compact(segment_results_new)) |>
  dplyr::mutate(segment = factor(segment, levels = names(config$segments)))

p_ccf_segments_new <- ggplot2::ggplot(ccf_segments_new, ggplot2::aes(x = lag, y = correlation, color = segment, linetype = segment)) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::labs(
    title = "Real Wage Growth vs. CPI-Deflated Real PCE Growth -- Cross-Correlation by Regime",
    subtitle = "Real Wage Growth (YoY %) vs. CPI-U-Deflated Real PCE Growth (YoY %); pre-2020 / pandemic-recovery (2020-2022) / post-2022",
    x = "Lag (months); positive = real wage growth leads real PCE growth",
    y = "Cross-correlation (real wage YoY vs. real PCE YoY, CPI-deflated)",
    color = "Segment",
    linetype = "Segment"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(config$output$dir, "ccf_by_segment_cpidef.png"),
  plot = p_ccf_segments_new, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

# ---- Comparison summary table: old vs. new, full sample + each segment --

full_summary <- tibble::tibble(
  deflator = c("old (PCE price index)", "new (CPI-U)"),
  segment = "full",
  n = n_full,
  peak_lag = c(peak_lag_old, peak_lag_new),
  peak_correlation = c(peak_corr_old, peak_corr_new)
)

segment_summary_old <- ccf_segments_old |>
  dplyr::group_by(segment) |>
  dplyr::slice_max(abs(correlation), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(deflator = "old (PCE price index)", segment = as.character(segment), n, peak_lag = lag, peak_correlation = correlation)

segment_summary_new <- ccf_segments_new |>
  dplyr::group_by(segment) |>
  dplyr::slice_max(abs(correlation), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(deflator = "new (CPI-U)", segment = as.character(segment), n, peak_lag = lag, peak_correlation = correlation)

comparison_summary <- dplyr::bind_rows(full_summary, segment_summary_old, segment_summary_new) |>
  dplyr::mutate(segment = factor(segment, levels = c("full", names(config$segments)))) |>
  dplyr::arrange(segment, deflator)

cat("\nDeflator comparison -- peak lag and peak |correlation|, old vs. new, full sample and each segment:\n")
print(comparison_summary)

# ---- Plain-language verdict ----------------------------------------------

lag_diff <- peak_lag_new - peak_lag_old
sign_changed <- sign(peak_corr_old) != sign(peak_corr_new)

verdict <- if (sign_changed) {
  sprintf(
    "Deflator choice materially changes the result: the peak-correlation SIGN flips between deflators (old: lag %+d, r = %.3f; new: lag %+d, r = %.3f). The wage-leads-PCE relationship is not robust to the deflator swap.",
    peak_lag_old, peak_corr_old, peak_lag_new, peak_corr_new
  )
} else if (abs(lag_diff) >= 3) {
  sprintf(
    "Deflator choice shifts the result by %d month(s): peak lag moves from %+d (old, r = %.3f) to %+d (new CPI-U, r = %.3f), same sign but a materially different lag.",
    abs(lag_diff), peak_lag_old, peak_corr_old, peak_lag_new, peak_corr_new
  )
} else {
  sprintf(
    "Deflator choice does NOT materially change the result: peak lag moves only %d month(s) (old: %+d, r = %.3f; new CPI-U: %+d, r = %.3f), same sign in both. The wage-leads-PCE lead/lag finding is robust to using a consistent CPI-U deflator.",
    abs(lag_diff), peak_lag_old, peak_corr_old, peak_lag_new, peak_corr_new
  )
}

message("\n", verdict)

# ---- Lagged scatter grid (CPI-deflated) ----------------------------------
# Lags applied to the full (contiguous, pre-filter) `wide` grid so each lag
# step is a true calendar-month lag, then NA rows are dropped per lag.
# Identical structure to wage_pce_ccf.R, using real_pce_yoy_cpidef instead
# of real_pce_yoy.

scatter_long <- purrr::map_dfr(config$scatter_lags, function(k) {
  wide |>
    dplyr::arrange(Date) |>
    dplyr::mutate(real_wage_yoy_lag = dplyr::lag(real_wage_yoy, k)) |>
    dplyr::transmute(Date, lag = k, real_wage_yoy_lag, real_pce_yoy_cpidef)
}) |>
  tidyr::drop_na(real_wage_yoy_lag, real_pce_yoy_cpidef) |>
  dplyr::mutate(lag_label = factor(
    sprintf("Lag = %d mo.", lag),
    levels = sprintf("Lag = %d mo.", config$scatter_lags)
  ))

facet_cor <- scatter_long |>
  dplyr::group_by(lag_label) |>
  dplyr::summarise(r = cor(real_wage_yoy_lag, real_pce_yoy_cpidef, use = "complete.obs"), .groups = "drop") |>
  dplyr::mutate(label = sprintf("r = %.2f", r))

cat("\nLagged scatter correlations, CPI-deflated (real_wage_yoy_{t-k} vs. real_pce_yoy_cpidef_t):\n")
print(dplyr::select(facet_cor, lag_label, r))

p_scatter <- ggplot2::ggplot(scatter_long, ggplot2::aes(x = real_wage_yoy_lag, y = real_pce_yoy_cpidef)) +
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
    title = "Lagged Real Wage Growth vs. CPI-Deflated Real PCE Growth",
    subtitle = "Real Wage Growth (YoY %), lagged k months, vs. CPI-U-Deflated Real PCE Growth (YoY %) at month t",
    x = "Real Wage Growth (YoY %), lagged k months",
    y = "CPI-U-Deflated Real PCE Growth (YoY %)"
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(config$output$dir, "lagged_scatter_grid_cpidef.png"),
  plot = p_scatter, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

message(sprintf("Wrote 4 plots to %s/", config$output$dir))
