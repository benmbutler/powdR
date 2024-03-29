% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/close_quant.R
\name{close_quant}
\alias{close_quant}
\title{Close the phase concentration data within a \code{powdRfps} or
\code{powdRafps} object}
\usage{
close_quant(x, ...)
}
\arguments{
\item{x}{A \code{powdRfps} or \code{powdRafps} object..}

\item{...}{other arguments}
}
\value{
a \code{powdRfps} or \code{powdRafps} object with components:
\item{tth}{a vector of the 2theta scale of the fitted data}
\item{fitted}{a vector of the fitted XRPD pattern}
\item{measured}{a vector of the original XRPD measurement (aligned and harmonised)}
\item{residuals}{a vector of the residuals (fitted vs measured)}
\item{phases}{a dataframe of the phases used to produce the fitted pattern and their concentrations}
\item{phases_grouped}{the phases dataframe grouped by phase_name and concentrations summed}
\item{obj}{named vector of the objective parameters summarising the quality of the fit}
\item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
All patterns have been weighted according to the coefficients used in the fit}
\item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
\item{inputs}{a list of input arguments used in the function call}
}
\description{
\code{close_quant} closes the quantitative data within a \code{powdRfps} or
\code{powdRafps} object (derived from \code{fps()} and \code{afps()}, respectively) by
ensuring that the composition sums to 100 percent. See also \code{?close_quant.powdRfps} and
\code{?close_quant.powdRafps}.
}
\examples{
\dontrun{
data(rockjock)
data(rockjock_mixtures)

rockjock_1 <- fps(lib = rockjock,
                  smpl = rockjock_mixtures$Mix1,
                  refs = c("ORDERED_MICROCLINE",
                           "LABRADORITE",
                           "KAOLINITE_DRY_BRANCH",
                           "MONTMORILLONITE_WYO",
                           "ILLITE_1M_RM30",
                           "CORUNDUM"),
                 std = "CORUNDUM",
                 align = 0.3,
                 std_conc = 20)

sum(rockjock_1$phases$phase_percent)

rockjock_1c <- close_quant(rockjock_1)

sum(rockjock_1c$phases$phase_percent)

rockjock_a1 <- afps(lib = rockjock,
                    smpl = rockjock_mixtures$Mix1,
                    std = "CORUNDUM",
                    align = 0.3,
                    lod = 1,
                    std_conc = 20)

sum(rockjock_a1$phases$phase_percent)

rockjock_a1c <- close_quant(rockjock_a1)

sum(rockjock_a1c$phases$phase_percent)

}
}
