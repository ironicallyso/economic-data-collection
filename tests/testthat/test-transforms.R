make_series <- function(n, series_id = "S1", start = "2020-01-01", value_fn = function(i) 100 + i - 1) {
  dates <- seq(as.Date(start), by = "1 month", length.out = n)
  tibble::tibble(series_id = series_id, date = dates, value = value_fn(seq_len(n)))
}

test_that("compute_yoy percent method matches hand calc", {
  df <- make_series(13) # values 100..112, +1/month
  result <- compute_yoy(df, method = "percent")
  expect_true(all(is.na(result$yoy[1:12])))
  expect_equal(result$yoy[13], 112 / 100 - 1, tolerance = 1e-8)
})

test_that("compute_yoy log method matches hand calc", {
  df <- make_series(13)
  result <- compute_yoy(df, method = "log")
  expect_true(all(is.na(result$yoy[1:12])))
  expect_equal(result$yoy[13], log(112) - log(100), tolerance = 1e-8)
})

test_that("compute_ma produces leading NAs and correct trailing mean", {
  df <- make_series(6)
  df$yoy <- c(NA, NA, 1, 2, 3, 4)
  result <- compute_ma(df, ma_window = 3)
  expect_true(all(is.na(result$yoy_ma[1:4])))
  expect_equal(result$yoy_ma[5], mean(c(1, 2, 3)), tolerance = 1e-8)
  expect_equal(result$yoy_ma[6], mean(c(2, 3, 4)), tolerance = 1e-8)
})

test_that("fill_gaps leaves a clean series unchanged with imputed all FALSE", {
  df <- make_series(12)
  result <- fill_gaps(df, max_fill_months = 3)
  expect_equal(nrow(result), nrow(df))
  expect_equal(result$value, df$value)
  expect_false(any(result$imputed))
})

test_that("fill_gaps interpolates a single-month gap to the neighbor average", {
  df <- make_series(6) # values 100..105, +1/month
  df_gapped <- df[-4, ] # drop the 4th month (value 103)
  result <- fill_gaps(df_gapped, max_fill_months = 3)

  expect_equal(nrow(result), 6) # grid restored to one row per month
  filled <- result[result$imputed, ]
  expect_equal(nrow(filled), 1)
  expect_equal(filled$date, df$date[4])
  expect_equal(filled$value, mean(c(102, 104)), tolerance = 1e-8)
  expect_equal(sum(result$imputed), 1)
})

test_that("fill_gaps interpolates a multi-month gap within the cap", {
  df <- make_series(8) # values 100..107
  df_gapped <- df[-c(4, 5), ] # drop two consecutive months (103, 104)
  result <- fill_gaps(df_gapped, max_fill_months = 3)

  expect_equal(nrow(result), 8)
  filled <- result[result$imputed, ]
  expect_equal(nrow(filled), 2)
  # Linear interpolation between 102 and 105 -> 103, 104.
  expect_equal(filled$value, c(103, 104), tolerance = 1e-8)
})

test_that("fill_gaps errors naming the series when a gap exceeds the cap", {
  df <- make_series(10)
  df_gapped <- df[-c(4, 5, 6, 7), ] # drop four consecutive months
  expect_error(fill_gaps(df_gapped, max_fill_months = 3), regexp = "S1")
  expect_error(fill_gaps(df_gapped, max_fill_months = 3), regexp = "max_fill_months")
})
