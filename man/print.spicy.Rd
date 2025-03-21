% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_spicy.R
\name{print.spicy}
\alias{print.spicy}
\title{Three-Line Table}
\usage{
\method{print}{spicy}(x, ...)
}
\arguments{
\item{x}{A data frame, matrix, array (2d) or table}

\item{...}{Additional arguments}
}
\value{
Invisibly returns \code{x}, after printing its formatted content.
}
\description{
\code{print.spicy()} prints any data frame as a three-line table with horizontal separators. Used in other functions of \code{spicy}
}
\examples{
\dontrun{
print.spicy(mtcars)
}
}
