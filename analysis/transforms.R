#' Year*12 + month, used to detect non-consecutive months within a series.
month_index <- function(date) {
  lubridate::year(date) * 12L + lubridate::month(date)
}

#' Stop if any series has a non-consecutive month-to-month gap.
#'
#' compute_yoy() lags by ROW position (lag(value, 12)), not by calendar date,
#' so a missing month would silently misalign the year-over-year comparison.
#' This check guarantees every series has exactly one row per consecutive
#' month before any lag-based math runs.
assert_no_gaps <- function(df) {
  gaps <- df |>
    dplyr::arrange(series_id, date) |>
    dplyr::group_by(series_id) |>
    dplyr::mutate(idx = month_index(date), gap = idx - dplyr::lag(idx)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(gap) & gap != 1)

  if (nrow(gaps) > 0) {
    messages <- sprintf(
      "series '%s': gap of %d month(s) before %s",
      gaps$series_id, gaps$gap - 1L, format(gaps$date, "%Y-%m")
    )
    stop(paste(c("Gap(s) detected in monthly data:", messages), collapse = "\n"))
  }

  invisible(df)
}

#' Year-over-year change at month t vs t-12, per series.
#'
#' Caller must have already run assert_no_gaps() on df: lag(value, 12) is a
#' positional lag, valid as a 12-calendar-month comparison only when there
#' are no missing months.
compute_yoy <- function(df, method = c("percent", "log")) {
  method <- match.arg(method)

  df |>
    dplyr::arrange(series_id, date) |>
    dplyr::group_by(series_id) |>
    dplyr::mutate(
      value_lag12 = dplyr::lag(value, 12),
      yoy = dplyr::case_when(
        method == "percent" ~ value / value_lag12 - 1,
        method == "log" ~ log(value) - log(value_lag12)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-value_lag12)
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
