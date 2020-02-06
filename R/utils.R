#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

df <- data.frame(
  A = c(1, 2, NA, 4, 5, NA),
  B = c(6, NA, 8, 9, 9, NaN),
  C = c("A", "B", "C", "D", "E", NA_character_)
)


if_then <- function(x, FUN, y) {
  if(FUN(x)) y else x
}

make_dirtyr <- function(n = 1e7) {
  set.seed(42)
  dirty <- as.data.frame(matrix(data = runif(n), ncol = 20))
  dirty[dirty < .05] <- NaN
  dirty[dirty > .99] <- -Inf
  dirty[dirty > .95] <- Inf
  dirty
}

# saveRDS(make_dirtyr(), "data/dirty.rds")
# saveRDS(make_dirtyr(100), "data/dirty_small.rds")
