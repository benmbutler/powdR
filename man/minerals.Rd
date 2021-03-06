% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{minerals}
\alias{minerals}
\title{An example powdRlib reference library}
\format{
A list of 3
\describe{
\item{xrd}{A dataframe of all xrd data (counts only). Column names denote the
reference sample}
\item{tth}{A vector of 2theta intervals of all measurements in the library}
\item{phases}{A dataframe the phase ID's, names and reference intensity
ratios (RIR)}
}
}
\usage{
minerals
}
\description{
This \code{powdRlib} object, built using the \code{powdRlib} constructor function,
contains a range of measured XRPD data (Cu K-alpha radiation) along with their reference
intensity ratios. The library can be used with the \code{soils} example data for full
pattern summation.
}
\keyword{datasets}
