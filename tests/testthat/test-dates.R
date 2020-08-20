context("Dates")


test_that("Date utils", {
  expect_equal(as.Date(NA), NA_date_)

  expect_false(is_leap(1500))
  expect_true(is_leap(1600))
  expect_false(is_leap(1700))
  expect_false(is_leap(1800))
  expect_false(is_leap(1900))
  expect_true(is_leap(2000))
  expect_true(is_leap(2400))
  expect_true(is_leap(4000))
  expect_true(is_leap(0))
  expect_false(is_leap(100))
  expect_true(is_leap(400))

  expect_named(days_in_month)
  expect_named(get_days_in_month(0))
  expect_equal(get_days_in_month(2000)[[2]], 29L)
  expect_equal(get_days_in_month(1900)[[2]], 28L)
  expect_equal(get_days_in_month(2100), days_in_month)
})

test_that("Examples in documentation are correct", {
  expect_equal(earliest_date(2019, 1, 0), as.Date("2019-01-01"))
  expect_equal(earliest_date(2000), as.Date("2000-01-01"))
  expect_equal(earliest_date(2019, NA, NA), as.Date("2019-01-01"))
  expect_equal(earliest_date(2019, NA, 2), as.Date("2019-01-02"))
  expect_equal(earliest_date(2019, NA, 31), as.Date("2019-01-31"))

  expect_equal(latest_date(2019), as.Date("2019-12-31"))
  expect_equal(latest_date(2019, NA, NA), as.Date("2019-12-31"))
  expect_equal(latest_date(2019, 0), as.Date("2019-12-31"))
  expect_equal(latest_date(2019, NA, 2), as.Date("2019-12-02"))
  expect_equal(latest_date(2019, 2), as.Date("2019-02-28"))
  expect_equal(latest_date(2016, 2), as.Date("2016-02-29"))

  x <- "UN UNK 2019"
  expect_equal(unknown_date(x), NA_date_)
  expect_equal(unknown_date(x, format = "dmy"), as.Date("2019-01-01"))
  expect_equal(unknown_date(x, format = "dmy", possible = "l"), as.Date("2019-12-31"))
  expect_setequal(
    unknown_date(c("01 JAN 1996", "Feb 2010", "2019"), "dmy"),
    as.Date(c("1996-01-01", "2010-02-01", "2019-01-01"))
  )
  expect_setequal(
    unknown_date(c("01 JAN 1996", "Feb 2016"), "dmy", "latest"),
    as.Date(c("1996-01-01", "2016-02-29"))
  )
  expect_equal(unknown_date("2015", possible = "e"), as.Date("2015-01-01"))
  expect_equal(unknown_date("2015", possible = "l"), as.Date("2015-12-31"))
})

test_that("Bad date: Earliest", {
  dates <- c("3 UNK 2019", "UN JUN 2004", "Feb 2000")
  res <- unknown_date(dates, format = "dmy")
  expected <- as.Date(c("2019-01-03", "2004-06-01", "2000-02-01"))
  expect_equal(res, expected)
})

test_that("Bad date: Latest", {
  dates <- c("3 UNK 2019", "UN JUN 2004", "Feb 2000", "Feb 2100", "UK UNK UNKN")
  res <- unknown_date(dates, format = "dmy", possible = "latest")
  expected <- as.Date(c("2019-12-03", "2004-06-30", "2000-02-29", "2100-02-28", NA_character_))
  expect_equal(res, expected)
})

test_that("'Empty' dates don't cause errors", {
  expect_error(unknown_date(""), NA)
  expect_error(unknown_date("    "), NA)
  expect_error(unknown_date("."), NA)
  expect_error(unknown_date("?.."), NA)
})
