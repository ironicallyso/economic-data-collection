source("analysis/transforms.R")
source("analysis/plot.R")

config <- yaml::read_yaml("config.yaml")

col_spec <- readr::cols(
  series_id = readr::col_character(),
  date = readr::col_date(format = "%Y-%m-%d"),
  value = readr::col_double(),
  units = readr::col_character(),
  source = readr::col_character(),
  fetched_at = readr::col_character()
)

bls_df <- readr::read_csv(config$bls$output_path, col_types = col_spec)
bea_df <- readr::read_csv(config$bea$output_path, col_types = col_spec)

bls_meta <- purrr::map_dfr(config$bls$series, ~ tibble::tibble(series_id = .x$id, label = .x$label))
bea_meta <- purrr::map_dfr(config$bea$tables, ~ tibble::tibble(series_id = .x$series_id, label = .x$label))
meta <- dplyr::bind_rows(bls_meta, bea_meta)

# inner_join both attaches the plot label and restricts analysis to series
# configured in config.yaml, even if the CSV has historical rows beyond that.
combined <- dplyr::bind_rows(bls_df, bea_df) |>
  dplyr::inner_join(meta, by = "series_id")

method <- config$analysis$method
ma_window <- config$analysis$ma_window
max_fill_months <- config$analysis$max_fill_months
plot_cfg <- config$plot

analyzed <- list()

for (sid in unique(combined$series_id)) {
  df_s <- dplyr::filter(combined, series_id == sid)
  result <- tryCatch(
    df_s |>
      fill_gaps(max_fill_months = max_fill_months) |>
      compute_yoy(method = method) |>
      compute_ma(ma_window = ma_window),
    error = function(e) {
      warning(sprintf("Skipping series '%s': %s", sid, conditionMessage(e)), call. = FALSE)
      NULL
    }
  )
  if (!is.null(result)) {
    analyzed[[sid]] <- result
  }
}

analysis_df <- dplyr::bind_rows(analyzed)

recent_years <- plot_cfg$recent_years
recent_suffix <- sprintf("_last%dy", recent_years)

for (sid in unique(analysis_df$series_id)) {
  df_s <- dplyr::filter(analysis_df, series_id == sid)
  variants <- list(
    list(df = df_s, suffix = ""),
    list(df = truncate_recent(df_s, recent_years), suffix = recent_suffix)
  )
  for (variant in variants) {
    p <- plot_yoy_series(variant$df, label = df_s$label[1], method = method, plot_cfg = plot_cfg)
    out_path <- save_plot(
      p, source = df_s$source[1], series_id = sid, method = method,
      plot_cfg = plot_cfg, suffix = variant$suffix
    )
    message("Wrote ", out_path)
  }
}

for (pair in plot_cfg$combined) {
  df_left <- dplyr::filter(analysis_df, series_id == pair$left_series_id)
  df_right <- dplyr::filter(analysis_df, series_id == pair$right_series_id)
  if (nrow(df_left) == 0 || nrow(df_right) == 0) {
    warning(sprintf("Skipping combined plot '%s': missing series data", pair$label), call. = FALSE)
    next
  }
  variants <- list(
    list(l = df_left, r = df_right, suffix = ""),
    list(l = truncate_recent(df_left, recent_years), r = truncate_recent(df_right, recent_years), suffix = recent_suffix)
  )
  for (variant in variants) {
    p <- plot_combined_series(variant$l, variant$r, label = pair$label, plot_cfg = plot_cfg)
    out_path <- save_combined_plot(
      p, left_id = pair$left_series_id, right_id = pair$right_series_id,
      suffix = variant$suffix, plot_cfg = plot_cfg
    )
    message("Wrote ", out_path)
  }
}

if (nrow(analysis_df) > 0) {
  spot_check_id <- analysis_df$series_id[1]
  cat("\nSpot-check tail for series:", spot_check_id, "\n")
  analysis_df |>
    dplyr::filter(series_id == spot_check_id) |>
    dplyr::select(date, value, yoy, yoy_ma) |>
    utils::tail(8) |>
    print()
}
