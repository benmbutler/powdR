#' Automated selection of crystalline phases for quantitative analysis
#'
#' \code{xrd.autoid} is used as part of \code{auto.fpf}. It uses \code{qr.solve} to
#' rapidly select appropriate phases from a reference library based on the correlation
#' to the residuals of the fit.
#'
#' @param xrd.lib an dataframe containing XRPD reference patterns (counts only, by column)
#' @param xrd.sample a vector of the sample counts
#' @param delta_lim a tuning parameter used to adjust the coarseness of the selection.
#' High values (e.g. 2) will select fewer phases than low values (e.g. 0.01).
#'
#' @return a list with components:
#' \item{x}{named vector of coefficients derived from qr.solve for each mineral selected}
#' \item{xrd.lib}{data frame of selected reference patterns}
#' \item{fit.error}{the trend in total fit error associated with the sequential addition of
#' phases to the library}
#'
#' @examples
#' data(soils)
#' data(minerals)
#'
#' soil <- soils$sandstone
#'
#' auto_select <- xrd.autoid(xrd.lib = minerals$xrd, xrd.sample = soil$counts,
#'                           delta_lim = 1)
#' plot(auto_select$fit.error)
xrd.autoid <- function(xrd.lib, xrd.sample, delta_lim) {

  lib_length <- ncol(xrd.lib)

  df <- data.frame(matrix(nrow = nrow(xrd.lib)))
  delta <- 100
  resid_x <- xrd.sample
  initial_error <- sqrt(sum(abs(xrd.sample)^2))
  fit_error <- c()
  fit_error[1] <- initial_error
  names(fit_error) <- c("inital")
  n <- 1

  #This is a while loop that will continue until the conditions is met
  while (delta > delta_lim) {

    cor_v <- as.numeric(lapply(names(xrd.lib),
                               function(x) stats::cor(xrd.lib[x], resid_x)))

    #identify the phase with maximum correlation
    cor_max <- which.max(cor_v)
    #extract its name
    v_name <- names(xrd.lib)[cor_max]

    #add this phase to the library to be used in fitting
    df[n] <- xrd.lib[v_name]

    #add its name
    names(df)[n] <- v_name

    #remove the identified phase from the original library
    xrd.lib[v_name] <- NULL

    ## fit pattern mixtures with qr.solve
    mat <- as.matrix(df)
    x <- qr.solve(mat, xrd.sample)

    #calculate fitted pattern and residuals
    fitted_pattern <- apply(sweep(mat, 2, x, "*"), 1, sum)
    resid_x <- xrd.sample - fitted_pattern

    #Overall error squared
    err <- sqrt(sum(abs(xrd.sample - fitted_pattern)^2))

    #add 1 to n so that the correct indexes are accessed
    n <- n + 1

    #add the error to the vector in order to deduce how much improvement there is in the fit
    fit_error[n] <- err
    names(fit_error)[n] <- v_name

    #get the current length of the error vector (dependent on the number of iterations of this while loop)
    l <- length(fit_error)

    #If this conditions is met, it means that all phases from the initial library have been used,
    #which means the loop has to stop.
    if (ncol(df) == lib_length) {
      delta <- 0
    }  else{
      #Calculate the percentage difference between the nth and nth-1 iteration
      delta <- 100 - ((fit_error[l]/fit_error[l - 1])* 100)
    }
  }

  # The while loop is always one step behind,

  #calculate the percentage improvement from adding sequential phases to the fit
  min_v <- c()
  for (i in 1:(length(fit_error)-1)) {
    min_v[i] <- 100 - ((fit_error[i+1]/fit_error[i])* 100)
  }

  #Remove the phase that is below the limit set in the function call
  remove_index <- which(min_v < delta_lim)

  #remove the column from the library that contains the identified data
  if (length(which(min_v < delta_lim)) > 0) {
    mat <- df[,-remove_index]
  }

  #re-solve
  x <- qr.solve(mat, xrd.sample)

  out <- list("x" = x, "xrd.lib" = data.frame(mat), "fit.error" = fit_error)
  return(out)

}
