
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dirtyr

<!-- badges: start -->

<!-- badges: end -->

The goal of dirtyr is to provide additional tools for working with
“dirty” data. It is true that on second thought the name “dirtyr”
doesn’t sound as appropriate as possibly “cleanr”, but that has already
been taken. In fact, I should probably change this because you may not
want to put “dirtyr” into a search engine.

This package is in a very developmental state. Use with caution.

## Installation

You can install the most recent development version of dirtyr from
[GitHub](https://github.com/jmbarbone/dirtyr) with:

``` r
devtools::install_github("jmbarbone/dirtyr")
```

## Dates

Not every system will provide easily accessible dates. Systems may allow
for ambiguous dates to be recorded alongside exact dates.
`unknown_date()` creates a date object out of the ambiguity and provides
either the earliest or latest possible dates.

``` r
x <- c("UN Feb 2000", "1958", "17 UNK 1992")

unknown_date(x, format = "dmy")
#> [1] "2000-02-01" "1958-01-01" "1992-01-17"

unknown_date(x, format = "dmy", possible = "latest")
#> [1] "2000-02-29" "1958-12-31" "1992-12-17"
```

We can also split dates apart when we need to:

``` r
x <- c("2010-01-12", "2020-09-30", "1999-12-31")
split_date(as.Date(x))
#>   year month day
#> 1 2010     1  12
#> 2 2020     9  30
#> 3 1999    12  31
```

Or even parse a column inside a data frame;

``` r
xx <- data.frame(
  x1 = 1:3,
  x2 = runif(3),
  date1 = as.Date(c("1950-10-05", "2020-04-29", "1992-12-17")),
  x3 = letters[1:3],
  date2 = as.Date(c("2010-01-12", "2020-09-30", "1999-12-31")))

parse_date(xx, c("date1", "date2"))
#>   x1        x2      date1 date1_year date1_month date1_day x3      date2
#> 1  1 0.5585511 1950-10-05       1950          10         5  a 2010-01-12
#> 2  2 0.3621280 2020-04-29       2020           4        29  b 2020-09-30
#> 3  3 0.4652415 1992-12-17       1992          12        17  c 1999-12-31
#>   date2_year date2_month date2_day
#> 1       2010           1        12
#> 2       2020           9        30
#> 3       1999          12        31
```

## Reindex

``` r
x <- 2:5
names(x) <- letters[x]
new_index <- c(1, 3, 5, 7)
names(new_index) <- letters[new_index]

reindex(x, new_index = new_index)
#>  a  c  e  g 
#> NA  3  5 NA

reindex(x, new_index = new_index, keep_empty = TRUE)
#>  a  c  e  g 
#> NA  3  5 NA
```

## To boolean

`to_boolean()` presents a simplified method of recoding vectors as
logical values. This function is flexible enough to take strict
requirements for the true/false values or a simple operation.

``` r
x <- c("Y", "Y", "N", "N", "N/A", "Y - not sure", "YN", "YY")
to_boolean(x, "Y", names= TRUE)
#>            Y            Y            N            N          N/A Y - not sure 
#>         TRUE         TRUE        FALSE        FALSE        FALSE        FALSE 
#>           YN           YY 
#>        FALSE        FALSE

y <- c(1, 2, 3, 4)
to_boolean(y, 1, 2)
#> [1]  TRUE FALSE    NA    NA

to_boolean(y, "<= 1", 2)
#> [1]  TRUE FALSE    NA    NA

tibble::tibble(x = x,
               y = rep(y, 2)) %>% 
  dplyr::mutate(xb = to_boolean(x, c("Y", "Y - not sure"), na = "N/A"),
                yb = to_boolean(y, "<= 1", ">= 3"))
#> # A tibble: 8 x 4
#>   x                y xb    yb   
#>   <chr>        <dbl> <lgl> <lgl>
#> 1 Y                1 TRUE  TRUE 
#> 2 Y                2 TRUE  NA   
#> 3 N                3 FALSE FALSE
#> 4 N                4 FALSE FALSE
#> 5 N/A              1 NA    TRUE 
#> 6 Y - not sure     2 TRUE  NA   
#> 7 YN               3 FALSE FALSE
#> 8 YY               4 FALSE FALSE
```

## QC

``` r
## coming soon...
```

## Smaller things

### Text to numeric

Easier conversions of non-numeric data to numeric (not just double).
Here numeric is a value that can either be an integer or a double.

``` r
x <- c(as.character(1:5), NA, letters[1:2], "1e4")

## Check if values may be numeric
maybe_numeric(x, names = TRUE)
#>     1     2     3     4     5  <NA>     a     b   1e4 
#>  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE

maybe_integer(x, names = TRUE)
#>    1    2    3    4    5 <NA>    a    b  1e4 
#> TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

maybe_integer(c(x, 1.2), names = TRUE)
#>     1     2     3     4     5  <NA>     a     b   1e4   1.2 
#>  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE

## Throws a warning
as.numeric(x)
#> Warning: NAs introduced by coercion
#> [1]     1     2     3     4     5    NA    NA    NA 10000

## Doesn't throw a warning
(res <- to_numeric(x))
#> [1]     1     2     3     4     5    NA    NA    NA 10000

## correctly identifies as integers
class(res)
#> [1] "integer"

## control for turning off
x %>% 
  to_numeric(int = FALSE) %>% 
  class()
#> [1] "numeric"
```

This has special behaviors for factors where the character label is used
rather than the numeric level.

``` r
f <- factor(c(seq(0, 1, .2), "No", "Na_character_"))

## Check if possibly numeric
maybe_numeric(f, names = TRUE)
#>             0           0.2           0.4           0.6           0.8 
#>          TRUE          TRUE          TRUE          TRUE          TRUE 
#>             1            No Na_character_ 
#>          TRUE         FALSE         FALSE

maybe_integer(f, names = TRUE)
#>             0           0.2           0.4           0.6           0.8 
#>          TRUE         FALSE         FALSE         FALSE         FALSE 
#>             1 Na_character_            No 
#>          TRUE          TRUE          TRUE

## Uses factor level
as.numeric(f)
#> [1] 1 2 3 4 5 6 8 7

## uses text
to_numeric(f)                    
#> [1] 0.0 0.2 0.4 0.6 0.8 1.0  NA  NA
```

### Reversing data.frames

This flips a vector (just a wrapper for `rev()`) or a data.frame or
matrix. This does not perform any sorting on the data frame, just a
flip.

``` r
iris %>% 
  head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

iris %>% 
  head() %>% 
  reverse()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 6          5.4         3.9          1.7         0.4  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 1          5.1         3.5          1.4         0.2  setosa
```
