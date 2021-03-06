% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_multi_xy.R
\name{as_multi_xy}
\alias{as_multi_xy}
\title{Create a multiXY object}
\usage{
as_multi_xy(x)
}
\arguments{
\item{x}{a list XRPD dataframes (column 1 = 2theta, column 2 = counts)}
}
\value{
a multiXY object, which is a list of xy dataframes.
}
\description{
\code{as_multi_xy} takes a list of XRPD data and ensures that they meet
various requirements to create a multiXY object. These requirements
include that the data is in list format, with each item in the list
containing 2 columns of numeric data in a dataframe. \code{as_multi_xy}
also checks that all names are unique. Once a multiXY object has been
created, it can easily be plotted using the associated \code{plot.multiXY}
method.
}
\examples{
# Load soils xrd data
data(rockjock_mixtures)

multi_xy <- as_multi_xy(rockjock_mixtures)

class(multi_xy)

}
