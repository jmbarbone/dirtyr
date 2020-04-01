
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dirtyr

<!-- badges: start -->

<!-- badges: end -->

The goal of dirtyr is to provide additional tools for working with
“dirty” data. It is true that on second thought the name “dirtyr”
doesn’t sound as appropriate as possibly “cleanr”. In fact, I should
probably change this because you may not want to put “dirtyr” into a
search engine.

This package is in a very developmental state. Use with caution.

## Installation

You can install the most recent development version of dirtyr from
[GitHub](https://github.com/jmbarbone/dirtyr) with:

``` r
remotes::install_github("jmbarbone/dirtyr")
```

## Dates

Not every system will provide easily accessible dates.

``` r
x <- c("UN Feb 2000", "1958", "17 UNK 1992")

unknown_date(x, format = "dmy")
#> [1] "2000-02-01" "1958-01-01" "1992-01-17"
unknown_date(x, format = "dmy", possible = "latest")
#> [1] "2000-02-29" "1958-12-31" "1992-12-17"
```

## Reindex

``` r
x <- 2:5
names(x) <- letters[x]
new_index <- c(1, 3, 5, 7)
names(new_index) <- letters[new_index]
reindex(x, new_index = new_index)
#>  a  b  c  d  e  g 
#> NA  2  3  4  5 NA
```

## QC
