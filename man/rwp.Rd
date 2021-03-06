% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objective_functions.R
\name{rwp}
\alias{rwp}
\title{Calculate the Rwp value for a fitted pattern}
\usage{
rwp(measured, fitted, weighting)
}
\arguments{
\item{measured}{a vector of count intensities for a measured pattern}

\item{fitted}{a vector of count intensities for a fitted pattern}

\item{weighting}{an optional weighting vector of the same length as those specified
in \code{measured} and \code{fitted}, which specifies areas of the pattern
to either emphasise (values > 1) or omit (values = 0) from the calculation.
Use with caution. Default is simply a weighting vector where all values are 1, which
hence has no effect on the computed value.}
}
\value{
a single numeric value
}
\description{
\code{rwp} computes the difference between a measured and fitted pattern. See
equation for Rwp in section 2.1 of Butler and Hillier (2021).
}
\examples{
# Load soils xrd data
data(soils)

#Load minerals library
data(minerals)

\dontrun{
#Produce a fit
fps_sand <-  fps(lib = minerals,
                 smpl = soils$sandstone,
                 refs = minerals$phases$phase_id,
                 std = "QUA.1",
                 align = 0.2)

rwp(measured = fps_sand$measured,
    fitted = fps_sand$fitted)

}

}
\references{
Butler, B.M., Hillier, S., 2021. powdR: An R package for quantitative mineralogy using full pattern summation
of X-ray powder diffraction data. Computers and Geosciences. 147, 104662. doi:10.1016/j.cageo.2020.104662
}
