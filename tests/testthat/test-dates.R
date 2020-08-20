context("Dates")


test_that("Date utils", {
  expect_equal(as.Date(NA), NA_date_)
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
  dates <- c("3 UNK 2019", "UN JUN 2004", "Feb 2000", "Feb 2100")
  res <- unknown_date(dates, format = "dmy", possible = "latest")
  expected <- as.Date(c("2019-12-03", "2004-06-30", "2000-02-29", "2100-02-28"))
  expect_equal(res, expected)
})

test_that("'Empty' dates don't cause errors", {
  expect_error(unknown_date(""), NA)
  expect_error(unknown_date("    "), NA)
  expect_error(unknown_date("."), NA)
  expect_error(unknown_date("?.."), NA)
})
