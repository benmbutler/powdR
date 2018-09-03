#' Plotting elements of a powdRfps object
#'
#' \code{plot.powdRfps} is designed to provide easy, adaptable plots
#' of full pattern summation outputs produced from \code{\link{fps}}.
#'
#' The only mandatory argument is \code{x}, which must be a powdRlib object.
#' When seeking to inspect the results from full pattern summation, interactive
#' plots are particularly useful and can be specified with the \code{interactive}
#' argument.
#'
#' @param x a powdRlib object
#' @param d logical. Denotes whether x-axis should be d-spacing. Default = FALSE.
#' @param wavelength numeric. Wavelength of the measurements to be plotted (in
#' angstroms), only required when \code{d = TRUE}.
#' @param interactive logical. If TRUE then the output will be an interactive
#' ggplotly object. If FALSE then the output will be a ggplot object.
#' @param ... other arguments
#'
#' @method plot powdRfps
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
#' \dontrun{
#' fps_sand <-  fps(smpl = soils$sandstone,
#'                 lib = minerals,
#'                 tth = c(4.5, 69.5),
#'                 refs = minerals$phases$phase_id,
#'                 std = "QUA.1")
#' plot(fps_sand)
#' plot(fps_sand, interactive = TRUE)
#' plot(fps_sand, d = TRUE, wavelength = 1.54, interactive = TRUE)
#' }
#' @export
plot.powdRfps <- function(x, d, wavelength, interactive, ...) {

  if(missing(interactive)) {
    interactive <- FALSE
  }

  if(!missing(interactive) & !is.logical(interactive)) {
    stop("The interactive arugment must be logical.")
  }

  if(missing(d)) {
    d <- FALSE
  }

  if(missing(wavelength)) {
    wavelength <- 1
  }

  if((d == TRUE & wavelength == 1) == TRUE) {
    stop("Please provide the wavelength of the XRPD measurements
         in order to calculate d-spacing.")
  }

  #Create a dataframe of the weighted pure patterns and fitted pattern
  pure_patterns <- data.frame(tth = x$tth,
                              Fitted = x$fitted,
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
    pure_patterns[["d"]] <- wavelength/(2*sin((pure_patterns$tth/2)*pi/180))
    measured[["d"]] <- wavelength/(2*sin((measured$tth/2)*pi/180))
    resids[["d"]] <- wavelength/(2*sin((resids$tth/2)*pi/180))
    pure_patterns_long[["d"]] <- wavelength/(2*sin((pure_patterns_long$tth/2)*pi/180))

    #and plot
    g1 <- ggplot2::ggplot() +
      ggplot2::geom_line(data = measured,
                         ggplot2::aes_(x = ~d, y = ~Measured, color = "Measured"), size = 0.35, linetype = "dotted") +
      ggplot2::geom_line(data = pure_patterns_long,
                         ggplot2::aes_(x = ~d, y = ~value, color = ~variable), size = 0.15) +
      ggplot2::scale_x_reverse() +
      ggplot2::ylab("Counts") +
      ggplot2::xlab("2theta") +
      ggplot2::theme(legend.title = ggplot2::element_blank())

    g2 <- ggplot2::ggplot() +
      ggplot2::geom_line(data = resids,
                         ggplot2::aes_(x = ~d, y = ~Residuals, color = "Residuals"), size = 0.15) +
      ggplot2::scale_colour_manual(name = "",
                                   values = c("Residuals" = "blue")) +
      ggplot2::ylab("Counts") +
      ggplot2::xlab("2theta") +
      ggplot2::scale_x_reverse()
  }
  #If d is false then just plot a normal 2theta graph
  else {
    g1 <- ggplot2::ggplot() +
      ggplot2::geom_line(data = measured,
                         ggplot2::aes_(x = ~tth, y = ~Measured, color = "Measured"), size = 0.35, linetype = "dotted") +
      ggplot2::geom_line(data = pure_patterns_long,
                         ggplot2::aes_(x = ~tth, y = ~value, color = ~variable), size = 0.15) +
      ggplot2::ylab("Counts") +
      ggplot2::xlab("2theta")
    ggplot2::theme(legend.title = ggplot2::element_blank())

    g2 <- ggplot2::ggplot() +
      ggplot2::geom_line(data = resids,
                         ggplot2::aes_(x = ~tth, y = ~Residuals, color = "Residuals"), size = 0.15) +
      ggplot2::ylab("Counts") +
      ggplot2::xlab("2theta") +
      ggplot2::scale_colour_manual(name = "",
                                   values = c("Residuals" = "blue"))
  }

  if(interactive == TRUE) {
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
  else {
    g3 <- ggpubr::ggarrange(g1, g2, nrow = 2)
    return(g3)
  }

  }



#' Plotting elements of a powdRlib object
#'
#' \code{plot.powdRlib} is designed to provide easy, adaptable plots
#' of an XRPD reference library built using the \code{powdRlib} constructor
#' function.
#'
#' The only mandatory argument is x, which must be a powdRlib object. Plots can
#' be made interactive using the logical \code{interactive} argument.
#'
#' @param x a powdRlib object
#' @param patterns a character string of reference pattern id's to be plotted
#' @param interactive Logical. If TRUE then the output will be an interactive
#' ggplotly object. If FALSE then the output will be a ggplot object.
#' @param ... other arguments
#'
#' @method plot powdRlib
#'
#' @examples
#' # Load the minerals library
#' data(minerals)
#'
#'plot(minerals, patterns = "ALB")
#' @export
plot.powdRlib <- function(x, patterns, interactive, ...) {

  if(missing(patterns)) {
    patterns <- c("")
  }

  if(!missing(patterns) & !is.character(patterns)) {
    stop("The patterns argument must be a character string
         of the reference pattern ID's to be plotted")
  }

  if(missing(interactive)) {
    interactive <- FALSE
  }

  if(!missing(interactive) & !is.logical(interactive)) {
    stop("The interactive arugment must be logical.")
  }

  melted <- reshape::melt(data.frame("tth" = x[[2]],
                                     x[[1]]), id = c("tth"))

  names(melted) <- c("tth", "phase", "counts")

  if(length(which(melted$phase %in% patterns)) > 0) {
    melted <- melted[which(melted$phase %in% patterns), ]
  }

  p <- ggplot2::ggplot(data = melted) +
    ggplot2::geom_line(ggplot2::aes_(x = ~tth, y = ~counts,
                           color = ~phase),
                       size = 0.15) +
    ggplot2::xlab("2theta") +
    ggplot2::ylab("Counts")

  if(interactive == TRUE) {
    p <- plotly::ggplotly(p)
  }

  return(p)

  }
