#' Objective measures to be minimised during full pattern fitting
#'
#' \code{fullpat} is a function that gets optimised during full
#' pattern fitting when using \code{fpf} and \code{auto.fpf}. Three
#' objective functions are available for minimisation: Delta, R, and
#' Rwp. We recommend Rwp.
#'
#' @param par a named vector of parameters to be optimised
#' @param pure.patterns a dataframe of pure patterns (counts only).
#' The ith column in the dataframe relates to the ith parameter of the
#' \code{par} vector
#' @param sample.pattern a vector of the measured pattern (counts only)
#' to be fitted.
#' @param obj the objective function to be minimised. One of "Delta",
#' "R" and "Rwp". See page 247 of Bish & Post (1989), Modern Powder
#' Diffraction.
#' @param weighting a weighting pattern (2theta and counts) specifying
#' which regions to emphasise during full pattern fitting. Use with
#' caution
fullpat <- function (par, pure.patterns, sample.pattern, obj, weighting)
{

#if only 1 pattern is being fitted:
  if (length(par) == 1) {

    #calculate fitted pattern
    s.mix <- par * pure.patterns

    #objective functions
    if(obj == "Delta") {
      d <- sum(abs(sample.pattern - s.mix) * weighting[,2])
    }

    if(obj == "R") {
      d <- sqrt(sum((sample.pattern - s.mix)^2 * weighting[,2])/sum(sample.pattern^2))
    }

    if(obj == "Rwp") {
      d <-  sqrt(sum((1/sample.pattern) * ((sample.pattern - s.mix)^2 * weighting[,2])) / sum((1/sample.pattern) * (sample.pattern^2)))
    }

    return(d)
  }

#if more than 1 pattern is being fitted
  if (length(par) > 1) {

    #This calculates the fitted pattern
    s.mix <- apply(sweep(pure.patterns, 2, par, "*"),
                   1, sum)

    #objective functions
    if(obj == "Delta") {
      d <- sum(abs(sample.pattern - s.mix) * weighting[,2])
    }

    if(obj == "R") {
      d <- sqrt(sum((sample.pattern - s.mix)^2 * weighting[,2])/sum(sample.pattern^2))
    }

    if(obj == "Rwp") {
      d <-  sqrt(sum((1/sample.pattern) * ((sample.pattern - s.mix)^2 * weighting[,2])) / sum((1/sample.pattern) * (sample.pattern^2)))
    }

    return(d)
  }
}
