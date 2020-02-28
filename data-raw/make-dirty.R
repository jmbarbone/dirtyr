# Create some fake data to play around with

library(usethis)
library(tidyverse)

make_dirtyr <- function(n = 20) {
  set.seed(42)
  dirty <- as_tibble(matrix(data = stats::runif(n), ncol = 20))
  dirty[dirty < .05] <- NaN
  dirty[dirty > .99] <- -Inf
  dirty[dirty > .95] <- Inf
  dirty
}

dirty_small <- make_dirtyr
dirty <- make_dirtyr(100)
# dirty_large <- make_dirtyr(1e7)

na_data <- tibble(
  logical = c(TRUE, FALSE, NA, FALSE, TRUE),
  character = c(NA_character_, letters[1:4]),
  integer = c(NA_integer_, 1L, 2L, 3L, 4L),
  double = c(1, NaN, 2, 3, 4),
  date = c(as.Date(c("2019-01-21", "1992-12-17", "2020-02-26", "1999-12-31")), NA_date_),
  infinity = c(Inf, 1e10, 1e1, 1e-1, -Inf)
)

use_data(dirty_small, overwrite = TRUE)
use_data(dirty, overwrite = TRUE)
# use_data(dirty_large, overwrite = TRUE)
use_data(na_data, overwrite = TRUE)
