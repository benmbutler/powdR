# FULLPAT FUNCTION --------------------------------------

fullpat <- function (par, pure.patterns, sample.pattern, obj)
{

  if (length(par) == 1) {
    pure.weights <- par
    s.mix <- par * pure.patterns
    d <- sum(abs(sample.pattern - s.mix))
    return(d)
  }

  if (length(par) > 1) {
    #These will be the pure weights already estimated using qr.solve
    pure.weights <- par

    #This calculates the fitted pattern
    s.mix <- apply(sweep(pure.patterns, 2, pure.weights, "*"),
                   1, sum)
    #This is the objective function that is minimised

    if(obj == "Delta") {
      d <- sum(abs(sample.pattern - s.mix))
    }

    if(obj == "R") {
      d <- sqrt(sum((sample.pattern - s.mix)^2)/sum(sample.pattern^2))
    }

    if(obj == "Rwp") {
      d <-  sqrt(sum((1/sample.pattern) * ((sample.pattern - s.mix)^2)) / sum((1/sample.pattern) * (sample.pattern^2)))
    }

    return(d)
  }
}
