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

test_that("assert_no_gaps passes silently on a clean series", {
  df <- make_series(12)
  expect_silent(assert_no_gaps(df))
})

test_that("assert_no_gaps stops with an informative message on a gap", {
  df <- make_series(12)
  df_gapped <- df[-6, ] # drop one mid-series month
  expect_error(assert_no_gaps(df_gapped), regexp = "S1")
  expect_error(assert_no_gaps(df_gapped), regexp = "gap")
})
