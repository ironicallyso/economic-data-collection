#' Fill interior monthly gaps so the calendar grid is regular before YoY math.
#'
#' compute_yoy() lags by ROW position (lag(value, 12)), not by calendar date,
#' so a missing month would silently misalign the year-over-year comparison.
#' Rather than refuse, we complete each series to a one-row-per-month grid
#' (first..last observed month) and linearly interpolate the missing values.
#' Completing the grid makes lag(value, 12) a valid 12-calendar-month lag again.
#'
#' Gaps are normal (e.g. a government shutdown delays a BLS/BEA release). Short
#' interior gaps are interpolated; an interior gap longer than `max_fill_months`
#' is fabricating too much, so we stop() and let the caller skip the series.
#' By construction the grid runs first..last observed month, so there are no
#' leading/trailing gaps within a series and no extrapolation is needed.
#'
#' Adds a logical `imputed` column (TRUE for filled rows) for downstream
#' transparency. Static per-series columns are carried onto the new rows.
fill_gaps <- function(df, max_fill_months) {
  static_cols <- c("series_id", "units", "source", "label", "method")
  static_cols <- intersect(static_cols, names(df))

  df |>
    dplyr::arrange(series_id, date) |>
    dplyr::group_by(series_id) |>
    dplyr::group_modify(function(g, key) {
      grid <- tibble::tibble(
        date = seq(min(g$date), max(g$date), by = "1 month")
      )
      out <- dplyr::left_join(grid, g, by = "date") |>
        dplyr::mutate(imputed = is.na(value))

      # Reject any consecutive run of missing months longer than the cap.
      runs <- rle(out$imputed)
      too_long <- runs$values & runs$lengths > max_fill_months
      if (any(too_long)) {
        end_idx <- cumsum(runs$lengths)
        first_bad <- which(too_long)[1]
        start_row <- end_idx[first_bad] - runs$lengths[first_bad] + 1L
        stop(sprintf(
          "series '%s': interior gap of %d month(s) starting %s exceeds max_fill_months (%d)",
          key$series_id[1], runs$lengths[first_bad],
          format(out$date[start_row], "%Y-%m"), max_fill_months
        ), call. = FALSE)
      }

      # Linear interpolation of interior NAs (no extrapolation at the edges).
      # The grid is one consecutive row per month, so interpolate on row index
      # (even monthly spacing) rather than calendar days, which have unequal
      # month lengths — a single-month gap then equals the neighbor average.
      if (any(out$imputed)) {
        out$value <- zoo::na.approx(out$value, na.rm = FALSE)
      }

      # Carry constant per-series columns onto the newly created rows.
      fill_these <- setdiff(static_cols, "series_id")
      if (length(fill_these) > 0) {
        out <- tidyr::fill(out, dplyr::all_of(fill_these), .direction = "downup")
      }
      out
    }) |>
    dplyr::ungroup()
}

#' Year-over-year change at month t vs t-12, per series.
#'
#' Caller must have already run assert_no_gaps() on df: lag(value, 12) is a
#' positional lag, valid as a 12-calendar-month comparison only when there
#' are no missing months.
#'
#' `bps` is for level series like an interest rate, where a percent-of-percent
#' ratio is unstable/misleading near zero (e.g. the Fed funds rate in
#' 2008-2015 or 2020-2021): it reports the level change in basis points
#' instead of a relative change.
compute_yoy <- function(df, method = c("percent", "log", "bps")) {
  method <- match.arg(method)

  df |>
    dplyr::arrange(series_id, date) |>
    dplyr::group_by(series_id) |>
    dplyr::mutate(
      value_lag12 = dplyr::lag(value, 12),
      yoy = dplyr::case_when(
        method == "percent" ~ value / value_lag12 - 1,
        method == "log" ~ log(value) - log(value_lag12),
        method == "bps" ~ (value - value_lag12) * 100
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-value_lag12)
}

#' Roll a daily series up to one row per calendar month (mean of the daily
#' values), so it can flow through the same monthly fill_gaps/compute_yoy/
#' compute_ma pipeline as the BLS/BEA series. Static per-series columns
#' (units, source) are carried via dplyr::first().
aggregate_daily_to_monthly <- function(df) {
  df |>
    dplyr::mutate(date = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(series_id, date) |>
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      units = dplyr::first(units),
      source = dplyr::first(source),
      .groups = "drop"
    )
}

#' Trailing moving average of the YoY series (not of the level), per SPEC.
compute_ma <- function(df, ma_window) {
  df |>
    dplyr::arrange(series_id, date) |>
    dplyr::group_by(series_id) |>
    dplyr::mutate(
      # na.rm = FALSE is deliberate: a window touching the leading NA yoy
      # values (first 12 months) should stay NA, not silently average fewer points.
      yoy_ma = slider::slide_dbl(
        yoy, mean,
        .before = ma_window - 1, .complete = TRUE, na.rm = FALSE
      )
    ) |>
    dplyr::ungroup()
}
