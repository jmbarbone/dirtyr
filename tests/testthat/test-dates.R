context("Dates")

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
