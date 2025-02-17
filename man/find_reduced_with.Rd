% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{find_reduced_with}
\alias{find_reduced_with}
\title{Find Reduced Warehouse Width}
\usage{
find_reduced_with(arcs)
}
\arguments{
\item{arcs}{A data frame or matrix representing the arcs in the warehouse.}
}
\value{
The reduced warehouse width as an integer.
}
\description{
This internal function finds the reduced width of the warehouse by analyzing the arcs and detecting the last non-empty aisle.
}
\keyword{internal}
