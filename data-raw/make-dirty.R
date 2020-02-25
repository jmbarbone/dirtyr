# Create some fake data to play around with

library(usethis)

make_dirtyr <- function(n = 20) {
  set.seed(42)
  dirty <- as.data.frame(matrix(data = stats::runif(n), ncol = 20))
  dirty[dirty < .05] <- NaN
  dirty[dirty > .99] <- -Inf
  dirty[dirty > .95] <- Inf
  dirty
}

dirty_small <- make_dirtyr
dirty <- make_dirtyr(100)
# dirty_large <- make_dirtyr(1e7)

use_data(dirty_small, overwrite = TRUE)
use_data(dirty, overwrite = TRUE)
# use_data(dirty_large, overwrite = TRUE)
