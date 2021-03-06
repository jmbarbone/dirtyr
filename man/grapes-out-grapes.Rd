% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/no-match.R
\name{\%out\%}
\alias{\%out\%}
\alias{\%wo\%}
\alias{\%wi\%}
\alias{no_match}
\title{Additional value matching}
\usage{
x \%out\% table

x \%wo\% table

x \%wi\% table

no_match(x, table)
}
\arguments{
\item{x}{vector or \code{NULL}: the values to be matched.
    \link[base]{Long vectors} are supported.}

\item{table}{vector or \code{NULL}: the values to be matched against.
    \link[base]{Long vectors} are not supported.}
}
\description{
Non matching alternatives and supplementary functions.
  Contrast with [base::match()], [base::intersect()], and [base::`%in%`()]
}
\examples{
1:10 \%in\% c(1,3,5,9)
1:10 \%out\% c(1,3,5,9)

# intersect() and setdiff() are similiar but don't keep duplicates
x <- c(1:6, 7:2)
y <- c(3, 7, 12)

x \%wo\% y      # --> keeps duplicated
setdiff(x, y) # --> unique values

x \%wi\% y        # --> keeps duplicated
intersect(x, y) # --> unique values
}
