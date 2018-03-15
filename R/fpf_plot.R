#' Interactive full pattern fitting plot
#'
#' \code{fpf.plot} returns a ggplotly interactive graph.
#'
#' This function is for use in combination with the output from \code{fpf}.
#'
#' @param x An \code{fpf} output to be plotted
#' @param d (optional) logical parameter specifying whether to compute d spacing.
#' Default = FALSE
#' @param lambda (required only with \code{d = TRUE}) the wavelength samples were measured at (in Angstroms). Changes x-axis to d-spacing.
#' @examples
#' # Load the Xpert library
#' data(Xpert)
#'
#' # Load the Xpert soil data to use in example
#' data(Xpert_soil)
#'
#' # define the phases to include in the fit
#' xrd_phases <- c("Qzt.662070.Strath.12Mins.P", "Qzt.662074.Qua.10.P", "X996730.QUA.11.P")
#'
#' # without organic
#' #not run
#' #fpf_out <-  fpf(smpl = Xpert_soil$mineral,
#' #               lib = Xpert,
#' #                 int.std = "Qzt.662070.Strath.12Mins.P",
#' #                 phases = xrd_phases,
#' #                 tth.min = 4.5,
#' #                 tth.max = 69.5,
#' #                 align.shift = 0.1,
#' #                 solver = "BFGS",
#' #                 obj.function = "Rwp",
#' #                 fpf.shift = 0.05)
#'
#' # Plot data on 2theta scale
#' # fpf.plot(fpf_out)
#'
#' # Plot data according to d-spacing
#' # fpf.plot(fpf_out, d = TRUE, lambda = 1.54060)
fpf.plot <- function(x, d = FALSE, lambda = 1) {

  if((d == TRUE & lambda == 1) == TRUE) {
    stop("Please provide the wavelength (lambda) of the XRPD measurements
         in order to calculate d-spacing.")
  }

  #Create a dataframe of the weighted pure patterns and fitted pattern
  pure_patterns <- data.frame(tth = x$tth,
                              FITTED = x$fitted,
                              x$weighted_pure_patterns)

  #The original measurement
  measured <- data.frame(tth = x$tth,
                         Measured = x$measured)

  #Residuals
  resids <- data.frame(tth = x$tth,
                       Residuals = x$residuals)

  #melt the pure patterns data frame
  pure_patterns_long <- reshape::melt(pure_patterns, id = c("tth"))

#If wavelength is supplied, then compute d
  if(d == TRUE) {
    pure_patterns[["d"]] <- lambda/(2*sin((pure_patterns$tth/2)*pi/180))
    measured[["d"]] <- lambda/(2*sin((measured$tth/2)*pi/180))
    resids[["d"]] <- lambda/(2*sin((resids$tth/2)*pi/180))
    pure_patterns_long[["d"]] <- lambda/(2*sin((pure_patterns_long$tth/2)*pi/180))

    #and plot
  g1 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = measured,
              ggplot2::aes(x = d, y = Measured, color = "Measured"), size = 0.35, linetype = "dotted") +
    ggplot2::geom_line(data = pure_patterns_long,
              ggplot2::aes(x = d, y = value, color = variable), size = 0.15) +
    ggplot2::scale_x_reverse() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  g2 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = resids,
              ggplot2::aes(x = d, y = Residuals, color = "Residuals"), size = 0.15) +
    ggplot2::scale_colour_manual(name = "",
                        values = c("Residuals" = "blue")) +
    ggplot2::scale_x_reverse()
  }
#If d is false then just plot a normal 2theta graph
else {
  g1 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = measured,
                       ggplot2::aes(x = tth, y = Measured, color = "Measured"), size = 0.35, linetype = "dotted") +
    ggplot2::geom_line(data = pure_patterns_long,
                       ggplot2::aes(x = tth, y = value, color = variable), size = 0.15) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  g2 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = resids,
                       ggplot2::aes(x = tth, y = Residuals, color = "Residuals"), size = 0.15) +
    ggplot2::scale_colour_manual(name = "",
                                 values = c("Residuals" = "blue"))
}

  #Convert to ggplotly
  p1 <- plotly::ggplotly(g1)
  p2 <- plotly::ggplotly(g2)
  p3 <- plotly::subplot(p1, p2,
                nrows = 2,
                heights = c(0.5, 0.5),
                widths = c(1),
                shareX = TRUE,
                titleY = TRUE)

  return(p3)
}
