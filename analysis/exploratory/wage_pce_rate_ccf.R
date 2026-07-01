# Exploratory: does the Fed Funds rate change lead real PCE growth the same
# way real wage growth does? Third stage of the wage/PCE lead-lag series --
# adds interest rates as a second candidate leading indicator (the other
# half of Ellis's "true leading indicators" claim) and fixes a bug in the
# by-segment CCF plots from the prior two scripts: they drew the FULL-SAMPLE
# significance band on segment-level correlations, which is invalid because
# threshold width is z/sqrt(n) and each segment has a different n.
#
# Standalone concept-testing script -- NOT part of the collection/analysis
# pipeline (analysis/run.R), not referenced by SPEC.md. Reuses compute_yoy()
# from analysis/transforms.R for consistency with the production YoY method,
# but does its own gap handling and CCF helpers (duplicated from
# wage_pce_ccf_cpidef.R rather than sourced from it), matching both prior
# scripts' stated self-contained-script convention.
#
# Only the CPI-U-deflated real PCE series (real_pce_yoy_cpidef) is used here
# (not the old BEA-PCE-price-index deflator) -- that comparison was already
# settled in wage_pce_ccf_cpidef.R.
#
# Out of scope: no combined/multivariate model (wage + rate jointly
# predicting PCE). This script only produces two independent bivariate CCF
# analyses (wage-vs-PCE, rate-vs-PCE) plus one visual overlay comparison --
# a joint model is a separate follow-up once both univariate relationships
# are individually characterized.
#
# Run from repo root, after outputs/bls_earnings.csv, outputs/bea_pce.csv,
# and outputs/fred_dff.csv have been populated by the Python collectors:
#   Rscript analysis/exploratory/wage_pce_rate_ccf.R

source("analysis/transforms.R")

# ---- Config ----------------------------------------------------------------
# Everything tunable lives here -- no magic constants below. Segment
# boundaries and scatter lags are copied verbatim from wage_pce_ccf_cpidef.R
# so all three exploratory scripts stay directly comparable.
config <- list(
  input = list(
    bls_path = "outputs/bls_earnings.csv", # mirrors config.yaml bls.output_path
    bea_path = "outputs/bea_pce.csv", # mirrors config.yaml bea.output_path
    fred_path = "outputs/fred_dff.csv", # mirrors config.yaml fred.output_path
    wage_series_id = "CES0500000013", # real avg hourly earnings, total private (CPI-deflated by BLS)
    nominal_pce_series_id = "DPCERC", # nominal PCE level, Table 2.8.5 (current dollars)
    cpi_series_id = "CUSR0000SA0", # CPI-U, all items, SA -- same index that already deflates wages
    rate_series_id = "DFF" # Federal Funds Effective Rate, daily -- mirrors config.yaml fred.series[0].id
  ),
  # Known missing months identified by inspecting the collected CSVs (mirrors
  # wage_pce_ccf_cpidef.R's interpolate_gap() pattern). CES0500000013 and
  # CUSR0000SA0 both have exactly one missing month, 2025-10-01 (a
  # BLS-shutdown-delayed release affecting both BLS series identically). We
  # interpolate it for CPI-U here, same as wage_pce_ccf_cpidef.R; the wage
  # series' own copy of this same gap is deliberately left untouched (no gap
  # entry below), matching both prior scripts' handling -- that one month
  # simply drops out via the complete-case filter below. DPCERC and the
  # DFF-monthly rollup have no gap in the observed range (confirmed by
  # inspection; date = NULL below still runs assert_no_unexpected_gaps() as
  # a live check rather than assuming it stays gap-free).
  gaps = list(
    list(series_id = "DPCERC", date = NULL),
    list(series_id = "CUSR0000SA0", date = as.Date("2025-10-01")),
    list(series_id = "DFF", date = NULL)
  ),
  ccf = list(
    max_lag = 12,
    significance_z = 1.96
  ),
  # Identical boundaries to the prior two scripts, clamped to the actual
  # observed date range at runtime (see segment_bounds()).
  segments = list(
    pre      = list(start = NULL,                 end = as.Date("2019-12-01")),
    pandemic = list(start = as.Date("2020-01-01"), end = as.Date("2022-12-01")),
    post     = list(start = as.Date("2023-01-01"), end = NULL)
  ),
  scatter_lags = c(0, 1, 2, 3, 6, 12),
  # Same output subdirectory as the prior two scripts -- new plots land
  # alongside the originals for easy side-by-side viewing.
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
nominal_pce_raw <- load_series(config$input$bea_path, config$input$nominal_pce_series_id)
cpi_raw <- load_series(config$input$bls_path, config$input$cpi_series_id)

# FRED's DFF is published daily, unlike the monthly BLS/BEA series (SPEC.md).
# Roll it up to one row per month (mean of daily values) via
# aggregate_daily_to_monthly() from transforms.R -- this matches FRED's own
# FEDFUNDS convention (SPEC.md line 41) and analysis/run.R's existing
# production handling of this exact series, so no new collector or FEDFUNDS
# config entry is needed.
rate_raw_daily <- readr::read_csv(config$input$fred_path, col_types = col_spec) |>
  dplyr::filter(series_id == !!config$input$rate_series_id) |>
  dplyr::arrange(date) |>
  dplyr::select(series_id, date, value, units, source)

rate_monthly_raw <- aggregate_daily_to_monthly(rate_raw_daily) |>
  dplyr::select(series_id, date, value)

# ---- Gap handling -----------------------------------------------------
# Same functions as the prior two scripts, duplicated here (not sourced) to
# keep this script self-contained. Applied to all four series, including
# the new monthly rate series.

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
nominal_pce_grid <- build_monthly_grid(nominal_pce_raw)
cpi_grid <- build_monthly_grid(cpi_raw)
rate_grid <- build_monthly_grid(rate_monthly_raw)

for (gap in config$gaps) {
  if (identical(gap$series_id, config$input$nominal_pce_series_id)) {
    nominal_pce_grid <- interpolate_gap(nominal_pce_grid, gap$date, "nominal PCE (DPCERC)")
  } else if (identical(gap$series_id, config$input$cpi_series_id)) {
    cpi_grid <- interpolate_gap(cpi_grid, gap$date, "CPI-U (CUSR0000SA0)")
  } else if (identical(gap$series_id, config$input$rate_series_id)) {
    rate_grid <- interpolate_gap(rate_grid, gap$date, "Fed Funds Rate, monthly (DFF)")
  }
}

assert_no_unexpected_gaps(wage_grid, "wage (CES0500000013)")
assert_no_unexpected_gaps(nominal_pce_grid, "nominal PCE (DPCERC)")
assert_no_unexpected_gaps(cpi_grid, "CPI-U (CUSR0000SA0)")
assert_no_unexpected_gaps(rate_grid, "Fed Funds Rate, monthly (DFF)")

# ---- Construct the CPI-deflated real PCE level --------------------------
# real_pce_cpidef = nominal PCE / (CPI-U / 100), i.e. nominal PCE re-based
# into CPI-U's 1982-84 dollar terms -- the same deflation convention BLS
# already applies to CES0500000013. Identical construction to
# wage_pce_ccf_cpidef.R. Restricted (inner join) to the date range where
# both nominal PCE and CPI-U have observations.
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

# ---- YoY / bps ------------------------------------------------------------
# compute_yoy() (transforms.R) applies ONE `method` to every series_id in
# its input, so series needing different methods must go through SEPARATE
# calls and be combined afterward -- the same pattern analysis/run.R already
# uses in production. Wage and CPI-deflated real PCE both use "percent".
# The Fed Funds rate uses "bps": EFFR near zero (2008-2015, 2020-2022) makes
# a percent-of-a-near-zero-base explode or flip sign on trivial absolute
# moves, so its YoY is instead the level change in basis points,
# (rate_t - rate_t-12) * 100 -- exactly transforms.R's "bps" method.
yoy_percent <- dplyr::bind_rows(wage_grid, real_pce_cpidef_grid) |>
  compute_yoy(method = "percent")

yoy_bps <- rate_grid |>
  compute_yoy(method = "bps")

# Safe to bind_rows() then pivot_wider() in one shot: compute_yoy()'s output
# schema is identical regardless of method, and series_id values are
# disjoint across the two calls, so pivot_wider(names_from = series_id)
# cannot collide keys.
yoy_long <- dplyr::bind_rows(yoy_percent, yoy_bps)

wide <- yoy_long |>
  dplyr::select(date, series_id, yoy) |>
  tidyr::pivot_wider(names_from = series_id, values_from = yoy) |>
  dplyr::arrange(date)

names(wide)[names(wide) == config$input$wage_series_id] <- "real_wage_yoy"
names(wide)[names(wide) == "real_pce_cpidef"] <- "real_pce_yoy_cpidef"
names(wide)[names(wide) == config$input$rate_series_id] <- "rate_yoy_chg_bps"
wide <- dplyr::rename(wide, Date = date)

# Complete cases across ALL THREE yoy columns -- one shared n_full and one
# shared per-segment n across the wage-CCF, rate-CCF, and by-segment
# analyses below. Sound because DFF goes back to 1954, far earlier than
# wage's ~2006 start, so restricting to wage's availability window never
# drops rate data -- wage/CPI-U are always the binding constraint.
complete_wide <- dplyr::filter(
  wide,
  !is.na(real_wage_yoy), !is.na(real_pce_yoy_cpidef), !is.na(rate_yoy_chg_bps)
)
n_full <- nrow(complete_wide)

# ---- CCF: full sample, wage vs. rate -------------------------------------

#' Tidy cross-correlation table between a "leading" series and a
#' "following" series. Positive lag convention: leading_series leads
#' following_series at lag k means cor(leading_t, following_{t+k}).
#' stats::ccf(x, y) reports, at lag k, cor(x_{t+k}, y_t); calling it with
#' x = following_series, y = leading_series therefore gives
#' cor(following_{t+k}, leading_t) == cor(leading_t, following_{t+k})
#' (correlation is symmetric) -- i.e. exactly the leading-at-lag-k
#' convention, with no sign flip needed. Same derivation as both prior
#' scripts' compute_ccf_table(wage, pce, ...), genericized so it also works
#' with the rate as the leading series.
#'
#' Rate case: a negative correlation at positive lags (rate hikes damping
#' future PCE growth) is the economically expected pattern, but that is NOT
#' hardcoded anywhere below -- the code reports whatever sign the data shows.
compute_ccf_table <- function(leading_series, following_series, max_lag) {
  ccf_obj <- stats::ccf(following_series, leading_series, lag.max = max_lag, plot = FALSE)
  tibble::tibble(
    lag = as.integer(ccf_obj$lag[, 1, 1]),
    correlation = as.numeric(ccf_obj$acf[, 1, 1])
  )
}

#' +/- z / sqrt(n): the correct significance threshold for a CCF computed
#' on n paired observations. Full-sample and per-segment CCFs have
#' different n, so each needs its OWN threshold -- reusing one full-sample
#' band on a by-segment plot (as both prior scripts' segment plots did) is
#' the bug this helper fixes.
segment_significance_band <- function(n, z) {
  z / sqrt(n)
}

ccf_full_wage <- compute_ccf_table(complete_wide$real_wage_yoy, complete_wide$real_pce_yoy_cpidef, config$ccf$max_lag)
ccf_full_rate <- compute_ccf_table(complete_wide$rate_yoy_chg_bps, complete_wide$real_pce_yoy_cpidef, config$ccf$max_lag)

peak_idx_wage <- which.max(abs(ccf_full_wage$correlation))
peak_lag_wage <- ccf_full_wage$lag[peak_idx_wage]
peak_corr_wage <- ccf_full_wage$correlation[peak_idx_wage]

peak_idx_rate <- which.max(abs(ccf_full_rate$correlation))
peak_lag_rate <- ccf_full_rate$lag[peak_idx_rate]
peak_corr_rate <- ccf_full_rate$correlation[peak_idx_rate]

message(sprintf(
  "Full-sample CCF [real wage YoY %% leads real PCE YoY %%, CPI-deflated]: peak |correlation| at lag %+d month(s), r = %.4f (n = %d)",
  peak_lag_wage, peak_corr_wage, n_full
))
message(sprintf(
  "Full-sample CCF [Fed Funds Rate YoY chg (bps) leads real PCE YoY %%, CPI-deflated]: peak |correlation| at lag %+d month(s), r = %.4f (n = %d)",
  peak_lag_rate, peak_corr_rate, n_full
))

sig_band <- segment_significance_band(n_full, config$ccf$significance_z)

#' Bar plot of a single CCF table: correlation vs. lag, dashed significance
#' band, the peak-|r| lag highlighted, titled/labeled/legended. Generic
#' over which series is leading via `series_desc`/`x_lab`/`y_lab` (both
#' prior scripts hardcoded "real wage" in this plot's text; parameterizing
#' it here lets it serve both the wage and rate full-sample plots without
#' mislabeling axis/legend units).
plot_ccf <- function(ccf_df, sig_band, peak_lag, title, subtitle, series_desc, x_lab, y_lab) {
  peak_row <- dplyr::filter(ccf_df, lag == peak_lag)
  band_df <- tibble::tibble(y = c(-sig_band, sig_band))
  peak_label <- sprintf("Peak |r| at lag %+d mo.", peak_lag)

  ggplot2::ggplot(ccf_df, ggplot2::aes(x = lag, y = correlation)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(fill = series_desc), width = 0.6) +
    ggplot2::geom_hline(
      data = band_df,
      ggplot2::aes(yintercept = y, linetype = "±95% significance threshold"),
      color = "grey40"
    ) +
    ggplot2::geom_point(data = peak_row, ggplot2::aes(color = peak_label), size = 3) +
    ggplot2::scale_fill_manual(name = NULL, values = setNames("#377eb8", series_desc)) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("±95% significance threshold" = "dashed")) +
    ggplot2::scale_color_manual(name = NULL, values = setNames("#e41a1c", peak_label)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

p_ccf_rate_full <- plot_ccf(
  ccf_full_rate, sig_band, peak_lag_rate,
  title = "Fed Funds Rate Change vs. CPI-Deflated Real PCE Growth -- Cross-Correlation (Full Sample)",
  subtitle = sprintf("Fed Funds Rate YoY Change (bps) vs. CPI-U-Deflated Real PCE Growth (YoY %%); n = %d", n_full),
  series_desc = "Cross-correlation (Fed Funds Rate YoY chg, bps, vs. real PCE YoY %, CPI-deflated)",
  x_lab = "Lag (months); positive = Fed Funds Rate change leads real PCE growth",
  y_lab = "Cross-correlation (Fed Funds Rate YoY chg, bps, vs. real PCE YoY %, CPI-deflated)"
)
ggplot2::ggsave(
  file.path(config$output$dir, "ccf_rate_full_sample.png"),
  plot = p_ccf_rate_full, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

# ---- CCF: multi-segment robustness check, with per-segment thresholds ---
# Same three segments as the prior two scripts: pre-2020, pandemic/recovery
# (2020-2022), post-2022. Fixes the significance-band bug in both prior
# scripts' by-segment plots (which reused the full-sample band): each
# segment's threshold is now z/sqrt(n) using that segment's OWN n.

#' Resolve a segment's start/end against the actual observed date range
#' (NULL bound = open-ended, clamped to the data's min/max Date). Identical
#' to the prior scripts' segment_bounds().
segment_bounds <- function(seg, data_min, data_max) {
  list(
    start = if (is.null(seg$start)) data_min else max(seg$start, data_min),
    end = if (is.null(seg$end)) data_max else min(seg$end, data_max)
  )
}

#' CCF for one segment, for a given (leading, following) column pair;
#' skips (with a warning) a segment too short to support the requested
#' max_lag. Generalized over leading_col/following_col (prior scripts
#' hardcoded real_wage_yoy) so it works for both wage and rate, and now
#' also computes each segment's own significance threshold from its own n
#' -- the fix for the shared-band bug in both prior scripts' segment plots.
compute_ccf_segment <- function(df, bounds, max_lag, label, leading_col, following_col, z) {
  seg_df <- df |>
    dplyr::filter(Date >= bounds$start, Date <= bounds$end) |>
    dplyr::filter(!is.na(.data[[leading_col]]), !is.na(.data[[following_col]]))
  min_rows <- 2 * max_lag + 1
  if (nrow(seg_df) < min_rows) {
    warning(sprintf(
      "Skipping segment '%s' (%s to %s): only %d complete month(s), need >= %d for lag.max = %d",
      label, format(bounds$start, "%Y-%m"), format(bounds$end, "%Y-%m"), nrow(seg_df), min_rows, max_lag
    ), call. = FALSE)
    return(NULL)
  }
  n_seg <- nrow(seg_df)
  compute_ccf_table(seg_df[[leading_col]], seg_df[[following_col]], max_lag) |>
    dplyr::mutate(
      segment = label,
      n = n_seg,
      threshold = segment_significance_band(n_seg, z),
      .before = 1
    )
}

data_min <- min(complete_wide$Date)
data_max <- max(complete_wide$Date)

segment_results_wage <- purrr::imap(config$segments, function(seg, label) {
  bounds <- segment_bounds(seg, data_min, data_max)
  compute_ccf_segment(
    complete_wide, bounds, config$ccf$max_lag, label,
    leading_col = "real_wage_yoy", following_col = "real_pce_yoy_cpidef", z = config$ccf$significance_z
  )
})
ccf_segments_wage <- dplyr::bind_rows(purrr::compact(segment_results_wage)) |>
  dplyr::mutate(segment = factor(segment, levels = names(config$segments)))

segment_results_rate <- purrr::imap(config$segments, function(seg, label) {
  bounds <- segment_bounds(seg, data_min, data_max)
  compute_ccf_segment(
    complete_wide, bounds, config$ccf$max_lag, label,
    leading_col = "rate_yoy_chg_bps", following_col = "real_pce_yoy_cpidef", z = config$ccf$significance_z
  )
})
ccf_segments_rate <- dplyr::bind_rows(purrr::compact(segment_results_rate)) |>
  dplyr::mutate(segment = factor(segment, levels = names(config$segments)))

#' Print each segment's n, threshold, and peak lag/correlation -- the
#' console-output requirement accompanying the threshold-bug fix.
print_segment_summary <- function(ccf_segments_df, header) {
  summary_df <- ccf_segments_df |>
    dplyr::group_by(segment) |>
    dplyr::slice_max(abs(correlation), n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(segment, n, threshold, peak_lag = lag, peak_correlation = correlation)
  cat("\n", header, "\n", sep = "")
  print(summary_df)
  invisible(summary_df)
}

print_segment_summary(ccf_segments_wage, "Wage-leads-PCE multi-segment CCF summary (n, threshold, peak lag/correlation per segment):")
print_segment_summary(ccf_segments_rate, "Rate-leads-PCE multi-segment CCF summary (n, threshold, peak lag/correlation per segment):")

#' By-segment CCF plot with a PER-SEGMENT (not shared) significance
#' threshold. `ccf_segments_df` must carry columns: lag, correlation,
#' segment, threshold (one threshold value per segment, from
#' compute_ccf_segment()'s output). Color encodes segment for both the CCF
#' line and its own threshold hlines (dashed, same color as that segment's
#' line); linetype instead distinguishes "CCF value" vs. "±95% threshold" --
#' a deliberate change from both prior scripts' by-segment plots, which
#' used linetype = segment together with one shared full-sample band.
plot_ccf_by_segment <- function(ccf_segments_df, title, subtitle, x_lab, y_lab) {
  threshold_df <- ccf_segments_df |>
    dplyr::distinct(segment, threshold) |>
    dplyr::reframe(y = c(-threshold, threshold), .by = segment)

  ggplot2::ggplot(ccf_segments_df, ggplot2::aes(x = lag, y = correlation, color = segment)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(linetype = "CCF value"), linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::geom_hline(
      data = threshold_df,
      ggplot2::aes(yintercept = y, color = segment, linetype = "±95% threshold (per-segment n)")
    ) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("CCF value" = "solid", "±95% threshold (per-segment n)" = "dashed")) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab, color = "Segment") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

p_ccf_wage_segments <- plot_ccf_by_segment(
  ccf_segments_wage,
  title = "Real Wage Growth vs. CPI-Deflated Real PCE Growth -- Cross-Correlation by Regime",
  subtitle = "Real Wage Growth (YoY %) vs. CPI-U-Deflated Real PCE Growth (YoY %); pre-2020 / pandemic-recovery (2020-2022) / post-2022; dashed lines are each segment's OWN ±95% threshold (z/sqrt(n)), not a shared full-sample band",
  x_lab = "Lag (months); positive = real wage growth leads real PCE growth",
  y_lab = "Cross-correlation (real wage YoY vs. real PCE YoY, CPI-deflated)"
)
ggplot2::ggsave(
  file.path(config$output$dir, "ccf_wage_by_segment_with_thresholds.png"),
  plot = p_ccf_wage_segments, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

p_ccf_rate_segments <- plot_ccf_by_segment(
  ccf_segments_rate,
  title = "Fed Funds Rate Change vs. CPI-Deflated Real PCE Growth -- Cross-Correlation by Regime",
  subtitle = "Fed Funds Rate YoY Change (bps) vs. CPI-U-Deflated Real PCE Growth (YoY %); pre-2020 / pandemic-recovery (2020-2022) / post-2022; dashed lines are each segment's OWN ±95% threshold (z/sqrt(n)), not a shared full-sample band",
  x_lab = "Lag (months); positive = Fed Funds Rate change leads real PCE growth",
  y_lab = "Cross-correlation (Fed Funds Rate YoY chg, bps, vs. real PCE YoY %, CPI-deflated)"
)
ggplot2::ggsave(
  file.path(config$output$dir, "ccf_rate_by_segment.png"),
  plot = p_ccf_rate_segments, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

# ---- Wage vs. rate comparison plot ---------------------------------------
# Both full-sample CCF curves on one set of axes, for a direct visual
# comparison of which indicator shows a cleaner/earlier lead. Both curves
# share n_full (complete_wide is a single unified complete-case table
# across wage, rate, and PCE), so one shared significance band is valid and
# shown once. The two leading series are in DIFFERENT units (wage: %,
# rate: bps) -- called out explicitly in the subtitle so only lag
# location/shape is read as comparable, not the correlation magnitude's
# economic size.
ccf_compare <- dplyr::bind_rows(
  dplyr::mutate(ccf_full_wage, leading = "Real wage YoY (%)"),
  dplyr::mutate(ccf_full_rate, leading = "Fed Funds Rate YoY chg (bps)")
) |>
  dplyr::mutate(leading = factor(leading, levels = c("Real wage YoY (%)", "Fed Funds Rate YoY chg (bps)")))

band_df <- tibble::tibble(y = c(-sig_band, sig_band))

p_ccf_compare <- ggplot2::ggplot(ccf_compare, ggplot2::aes(x = lag, y = correlation, color = leading, linetype = leading)) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::geom_hline(
    data = band_df,
    ggplot2::aes(yintercept = y),
    color = "grey40", linetype = "dashed"
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_color_manual(name = "Leading series", values = c(
    "Real wage YoY (%)" = "#377eb8",
    "Fed Funds Rate YoY chg (bps)" = "#e41a1c"
  )) +
  ggplot2::scale_linetype_manual(name = "Leading series", values = c(
    "Real wage YoY (%)" = "solid",
    "Fed Funds Rate YoY chg (bps)" = "dashed"
  )) +
  ggplot2::labs(
    title = "Wage vs. Rate -- Which Leads CPI-Deflated Real PCE Growth? (Full Sample)",
    subtitle = sprintf(
      "Dashed grey = shared ±95%% significance threshold (n = %d, common sample).\nSeries are in DIFFERENT units (wage: %%, rate: bps) -- compare lag location\nand shape only, not correlation magnitude's economic size.",
      n_full
    ),
    x = "Lag (months); positive = leading series leads real PCE growth",
    y = "Cross-correlation (vs. real PCE YoY, CPI-deflated)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(config$output$dir, "ccf_wage_vs_rate_comparison.png"),
  plot = p_ccf_compare, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

# ---- Lagged scatter grid (rate) ------------------------------------------
# Lags applied to the full (contiguous, pre-filter) `wide` grid so each lag
# step is a true calendar-month lag, then NA rows are dropped per lag.
# Same structure as the prior scripts' scatter grids, using
# rate_yoy_chg_bps_{t-k} on x instead of real_wage_yoy_{t-k}. Because x is
# already a scaled bps number (not a fraction), it must NOT use a
# percent-formatter -- scale_x_continuous(labels = scales::label_percent())
# would be wrong here; a plain numeric formatter is used instead, with
# "(bps)" spelled out in the axis title.

scatter_long_rate <- purrr::map_dfr(config$scatter_lags, function(k) {
  wide |>
    dplyr::arrange(Date) |>
    dplyr::mutate(rate_yoy_chg_bps_lag = dplyr::lag(rate_yoy_chg_bps, k)) |>
    dplyr::transmute(Date, lag = k, rate_yoy_chg_bps_lag, real_pce_yoy_cpidef)
}) |>
  tidyr::drop_na(rate_yoy_chg_bps_lag, real_pce_yoy_cpidef) |>
  dplyr::mutate(lag_label = factor(
    sprintf("Lag = %d mo.", lag),
    levels = sprintf("Lag = %d mo.", config$scatter_lags)
  ))

facet_cor_rate <- scatter_long_rate |>
  dplyr::group_by(lag_label) |>
  dplyr::summarise(r = cor(rate_yoy_chg_bps_lag, real_pce_yoy_cpidef, use = "complete.obs"), .groups = "drop") |>
  dplyr::mutate(label = sprintf("r = %.2f", r))

cat("\nLagged scatter correlations (rate_yoy_chg_bps_{t-k} vs. real_pce_yoy_cpidef_t):\n")
print(dplyr::select(facet_cor_rate, lag_label, r))

p_scatter_rate <- ggplot2::ggplot(scatter_long_rate, ggplot2::aes(x = rate_yoy_chg_bps_lag, y = real_pce_yoy_cpidef)) +
  ggplot2::geom_point(ggplot2::aes(color = "Monthly observation"), alpha = 0.6, size = 1.5) +
  ggplot2::geom_smooth(ggplot2::aes(linetype = "OLS fit"), method = "lm", se = FALSE, color = "#e41a1c", linewidth = 0.7) +
  ggplot2::geom_text(
    data = facet_cor_rate,
    mapping = ggplot2::aes(x = -Inf, y = Inf, label = label),
    hjust = -0.15, vjust = 1.5, inherit.aes = FALSE, size = 3.5
  ) +
  ggplot2::facet_wrap(~lag_label) +
  ggplot2::scale_color_manual(name = NULL, values = c("Monthly observation" = "#377eb8")) +
  ggplot2::scale_linetype_manual(name = NULL, values = c("OLS fit" = "solid")) +
  ggplot2::labs(
    title = "Lagged Fed Funds Rate Change vs. CPI-Deflated Real PCE Growth",
    subtitle = "Fed Funds Rate YoY Change (bps), lagged k months, vs. CPI-U-Deflated Real PCE Growth (YoY %) at month t",
    x = "Fed Funds Rate YoY Change (bps), lagged k months",
    y = "CPI-U-Deflated Real PCE Growth (YoY %)"
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(config$output$dir, "lagged_scatter_grid_rate.png"),
  plot = p_scatter_rate, width = config$output$width_in, height = config$output$height_in, dpi = config$output$dpi
)

message(sprintf("Wrote 5 plots to %s/", config$output$dir))
