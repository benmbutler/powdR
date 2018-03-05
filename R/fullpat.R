# FULLPAT FUNCTION --------------------------------------

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
