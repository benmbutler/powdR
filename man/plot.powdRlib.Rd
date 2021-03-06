% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot.powdRlib}
\alias{plot.powdRlib}
\title{Plotting elements of a powdRlib object}
\usage{
\method{plot}{powdRlib}(x, wavelength, refs, interactive, ...)
}
\arguments{
\item{x}{a powdRlib object}

\item{wavelength}{One of "Cu", "Co" or a custom numeric value defining the wavelength
(in Angstroms). Used to compute d-spacings.When "Cu" or "Co" are supplied, wavelengths
of 1.54056 or 1.78897 are used, respectively.}

\item{refs}{a character string of reference pattern id's to be plotted}

\item{interactive}{Logical. If TRUE then the output will be an interactive
ggplotly object. If FALSE then the output will be a ggplot object.}

\item{...}{other arguments}
}
\description{
\code{plot.powdRlib} is designed to provide easy, adaptable plots
of an XRPD reference library built using the \code{powdRlib} constructor
function.
}
\details{
Plots can be made interactive using the logical \code{interactive} argument.
}
\examples{
# Load the minerals library
data(minerals)
\dontrun{
plot(minerals, wavelength = "Cu", refs = "ALB")
plot(minerals, wavelength = "Cu", refs = "ALB", interactive = TRUE)
}
}
