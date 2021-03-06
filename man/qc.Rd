% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc.R
\name{qc}
\alias{qc}
\alias{qc.default}
\alias{qc.logical}
\alias{qc.character}
\title{Quality check}
\usage{
qc(target, reference, ...)

\method{qc}{default}(target, reference, ...)

\method{qc}{logical}(target, reference, ...)

\method{qc}{character}(target, reference, string_dist = FALSE, ignore_case = FALSE, ...)
}
\arguments{
\item{target}{Target}

\item{reference}{Reference}

\item{...}{Additional arguments passed to methods}

\item{string_dist}{If TRUE, string distances calculated with [stringr::stringdist]}

\item{ignore_case}{Should cases be ignored?}
}
\value{
A data.frame with each target and reference value and their difference.
Values in the `target` and `reference` column will always be returned as character.

\item{differences}{A logical vector (`TRUE` or `FALSE` if items were different)}
}
\description{
Perform a QC on an object.
}
\details{
If both objects are named, they will be `reindex()`'d to be of equal length
  and sorted with matching names.
`qc.factor` will default to character if not ordered
}
