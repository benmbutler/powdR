.group_patterns <- function(x) {

  wpp_df <- x$weighted_pure_patterns

  if (!identical(names(wpp_df), x$phases$phase_id)) {

    stop("The ID's and names don't match")

  }

  wpp_df <- data.frame(t(wpp_df))

  wpp_df <- data.frame("phase_name" = x$phases$phase_name,
                       wpp_df)

  wpp_df <- stats::aggregate(. ~ phase_name, data = wpp_df, FUN = sum)
  wpp_df_names <- wpp_df$phase_name
  wpp_df <- data.frame(t(wpp_df[-1]))
  names(wpp_df) <- wpp_df_names
  rownames(wpp_df) <- NULL

  x$weighted_pure_patterns <- wpp_df

  return(x)

}


.gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Plotting elements of a powdRfps object
#'
#' \code{plot.powdRfps} is designed to provide easy, adaptable plots
#' of full pattern summation outputs produced from \code{\link{fps}}.
#'
#' When seeking to inspect the results from full pattern summation, interactive
#' plots are particularly useful and can be specified with the \code{interactive}
#' argument.
#'
#' @param x a powdRfps object
#' @param wavelength One of "Cu", "Co" or a custom numeric value defining the wavelength
#' (in Angstroms). Used to compute d-spacings.When "Cu" or "Co" are supplied, wavelengths
#' of 1.54056 or 1.78897 are used, respectively.
#' @param mode One of "fit", "residuals" or "both" defining whether to plot the fitted
#' patterns, the residuals of the fit, or both, respectively. Default = "fit".
#' @param xlim A numeric vector of length two providing limits of the x-axis. Defaults
#' to full x-axis unless specified.
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
#' fps_sand <-  fps(lib = minerals,
#'                 smpl = soils$sandstone,
#'                 refs = minerals$phases$phase_id,
#'                 std = "QUA.1",
#'                 align = 0.2)
#'
#' plot(fps_sand, wavelength = "Cu")
#' plot(fps_sand, wavelength = "Cu", interactive = TRUE)
#' }
#' @export
plot.powdRfps <- function(x, wavelength, mode, xlim, interactive, ...) {

  if (missing(xlim)) {

    xlim <- c(min(x$tth), max(x$tth))

  }

  if (length(xlim) > 2) {

    stop("xlim must be a numeric vector of length 2",
         call. = FALSE)

  }

  if (!is.numeric(xlim)) {

    stop("xlim must be a numeric vector of length 2",
         call. = FALSE)

  }

  if (xlim[1] < min(x$tth) | xlim[2] > max(x$tth)) {

    stop("The limits defined in xlim are outside of the 2theta range of the data",
         call. = FALSE)

  }


  if (missing(mode)) {

    mode <- "fit"

  }

  if (!mode %in% c("fit", "residuals", "both")) {

    stop("The mode arugment must be one of `fit`, `residuals`, or `both`",
         call. = FALSE)

  }

  if(missing(interactive)) {
    interactive <- FALSE
  }

  if(!missing(interactive) & !is.logical(interactive)) {
    stop("The interactive arugment must be logical.",
         call. = FALSE)
  }

  #If wavelength is missing then stop the function call
  if (missing(wavelength)) {

    stop("Provide a wavelength so that d-spacings can be calculated",
         call. = FALSE)

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Cu") {

    wavelength <- 1.54056

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Co") {

    wavelength <- 1.78897

  }

  #At this point if wavelength isn't numeric then stop
  if (!is.numeric(wavelength)) {

    stop("The wavelength argument must be one of either 'Cu', 'Co', or
         a custom numeric value",
         call. = FALSE)

  }

  #compute d
  d_v <- round(wavelength/(2*sin((x$tth/2)*pi/180)), 3)

  #Create a dataframe of the weighted pure patterns and fitted pattern
  pure_patterns <- data.frame(tth = x$tth,
                              d = d_v,
                              Measured = x$measured,
                              Fitted = x$fitted,
                              x$weighted_pure_patterns,
                              check.names = FALSE)

  refs_colors <- .gg_color_hue(ncol(x$weighted_pure_patterns))

  #Residuals
  resids <- data.frame(tth = x$tth,
                       d = d_v,
                       Counts = x$residuals)

  #melt the pure patterns data frame
  pure_patterns_long <- reshape::melt(pure_patterns, id = c("tth", "d"))

  #Name the counts column
  names(pure_patterns_long)[4] <- "Counts"


  g1 <- suppressWarnings(ggplot2::ggplot() +
                           ggplot2::geom_line(data = pure_patterns_long,
                                              ggplot2::aes_(x = ~tth, y = ~Counts,
                                                            color = ~variable,
                                                            linetype = ~variable,
                                                            d = ~d),
                                              size = 0.25) +
                           ggplot2::scale_color_manual(values = c("black", "red",
                                                                  refs_colors)) +
                           ggplot2::scale_linetype_manual(values = c("solid", "solid",
                                                                     rep("dotted", ncol(x$weighted_pure_patterns)))) +
                           ggplot2::ylab("Counts") +
                           ggplot2::xlab("2theta") +
                           ggplot2::scale_x_continuous(limits = xlim) +
                           ggplot2::theme(legend.title = ggplot2::element_blank()))

  g2 <- suppressWarnings(ggplot2::ggplot() +
                           ggplot2::geom_line(data = resids,
                                              ggplot2::aes_(x = ~tth, y = ~Counts, color = "Residuals", d = ~d), size = 0.15) +
                           ggplot2::ylab("Counts") +
                           ggplot2::xlab("2theta") +
                           ggplot2::scale_x_continuous(limits = xlim) +
                           ggplot2::scale_colour_manual(name = "",
                                                        values = c("Residuals" = "blue")))


  if (interactive == TRUE) {

    #Convert to ggplotly
    p1 <- plotly::ggplotly(g1, tooltip = c("x", "y", "d", "colour"))
    p2 <- plotly::ggplotly(g2, tooltip = c("x", "y", "d", "colour"))


    if (mode == "fit") {

      return(p1)

    }

    if (mode == "residuals") {

      return(p2)

    }

    if (mode  == "both") {

      p3 <- plotly::subplot(p1, p2,
                            nrows = 2,
                            heights = c((2/3), (1/3)),
                            widths = c(1),
                            shareX = TRUE,
                            titleY = TRUE)

      return(p3)

    }


  } else {

    if (mode == "fit") {

      return(g1)

    }

    if (mode == "residuals") {

      return(g2)

    }

    if (mode == "both") {

    g3 <- ggpubr::ggarrange(g1, g2, nrow = 2,
                            heights = c(2,1))
    return(g3)

    }

  }

}

#' Plotting elements of a powdRafps object
#'
#' \code{plot.powdRafps} is designed to provide easy, adaptable plots
#' of full pattern summation outputs produced from \code{\link{afps}}.
#'
#' When seeking to inspect the results from full pattern summation, interactive
#' plots are particularly useful and can be specified with the \code{interactive}
#' argument.
#'
#' @param x a powdRfps object
#' @param wavelength One of "Cu", "Co" or a custom numeric value defining the wavelength
#' (in Angstroms). Used to compute d-spacings.When "Cu" or "Co" are supplied, wavelengths
#' of 1.54056 or 1.78897 are used, respectively.
#' @param mode One of "fit", "residuals" or "both" defining whether to plot the fitted
#' patterns, the residuals of the fit, or both, respectively. Default = "fit".
#' @param xlim A numeric vector of length two providing limits of the x-axis. Defaults
#' to full x-axis unless specified.
#' @param interactive logical. If TRUE then the output will be an interactive
#' ggplotly object. If FALSE then the output will be a ggplot object.
#' @param ... other arguments
#'
#' @method plot powdRafps
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
#' \dontrun{
#' afps_sand <-  afps(lib = minerals,
#'                    smpl = soils$sandstone,
#'                    std = "QUA.1",
#'                    amorphous = "ORG",
#'                    align = 0.2,
#'                    lod = 0.1)
#'
#' plot(afps_sand, wavelength = "Cu")
#' plot(afps_sand, wavelength = "Cu", interactive = TRUE)
#'
#' }
#' @export
plot.powdRafps <- function(x, wavelength, mode, xlim, interactive, ...) {

  if (missing(xlim)) {

    xlim <- c(min(x$tth), max(x$tth))

  }

  if (length(xlim) > 2) {

    stop("xlim must be a numeric vector of length 2",
         call. = FALSE)

  }

  if (!is.numeric(xlim)) {

    stop("xlim must be a numeric vector of length 2",
         call. = FALSE)

  }

  if (xlim[1] < min(x$tth) | xlim[2] > max(x$tth)) {

    stop("The limits defined in xlim are outside of the 2theta range of the data",
         call. = FALSE)

  }

  if (missing(mode)) {

    mode <- "fit"

  }

  if (!mode %in% c("fit", "residuals", "both")) {

    stop("The mode arugment must be one of `fit`, `residuals`, or `both`",
         call. = FALSE)

  }

  if(missing(interactive)) {
    interactive <- FALSE
  }

  if(!missing(interactive) & !is.logical(interactive)) {
    stop("The interactive arugment must be logical.",
         call. = FALSE)
  }

  #If wavelength is missing then stop the function call
  if (missing(wavelength)) {

    stop("Provide a wavelength so that d-spacings can be calculated",
         call. = FALSE)

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Cu") {

    wavelength <- 1.54056

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Co") {

    wavelength <- 1.78897

  }

  #At this point if wavelength isn't numeric then stop
  if (!is.numeric(wavelength)) {

    stop("The wavelength argument must be one of either 'Cu', 'Co', or
         a custom numeric value",
         call. = FALSE)

  }

  #compute d
  d_v <- round(wavelength/(2*sin((x$tth/2)*pi/180)), 3)

  #Create a dataframe of the weighted pure patterns and fitted pattern
  pure_patterns <- data.frame(tth = x$tth,
                              d = d_v,
                              Measured = x$measured,
                              Fitted = x$fitted,
                              x$weighted_pure_patterns,
                              check.names = FALSE)

  refs_colors <- .gg_color_hue(ncol(x$weighted_pure_patterns))

  #Residuals
  resids <- data.frame(tth = x$tth,
                       d = d_v,
                       Counts = x$residuals)

  #melt the pure patterns data frame
  pure_patterns_long <- reshape::melt(pure_patterns, id = c("tth", "d"))

  #Name the counts column
  names(pure_patterns_long)[4] <- "Counts"


  g1 <- suppressWarnings(ggplot2::ggplot() +
                           ggplot2::geom_line(data = pure_patterns_long,
                                              ggplot2::aes_(x = ~tth, y = ~Counts,
                                                            color = ~variable,
                                                            linetype = ~variable,
                                                            d = ~d),
                                              size = 0.25) +
                           ggplot2::scale_color_manual(values = c("black", "red",
                                                                  refs_colors)) +
                           ggplot2::scale_linetype_manual(values = c("solid", "solid",
                                                                     rep("dotted", ncol(x$weighted_pure_patterns)))) +
                           ggplot2::ylab("Counts") +
                           ggplot2::xlab("2theta") +
                           ggplot2::scale_x_continuous(limits = xlim) +
                           ggplot2::theme(legend.title = ggplot2::element_blank()))

  g2 <- suppressWarnings(ggplot2::ggplot() +
                           ggplot2::geom_line(data = resids,
                                              ggplot2::aes_(x = ~tth, y = ~Counts, color = "Residuals", d = ~d), size = 0.15) +
                           ggplot2::ylab("Counts") +
                           ggplot2::xlab("2theta") +
                           ggplot2::scale_x_continuous(limits = xlim) +
                           ggplot2::scale_colour_manual(name = "",
                                                        values = c("Residuals" = "blue")))


  if (interactive == TRUE) {

    #Convert to ggplotly
    p1 <- plotly::ggplotly(g1, tooltip = c("x", "y", "d", "colour"))
    p2 <- plotly::ggplotly(g2, tooltip = c("x", "y", "d", "colour"))


    if (mode == "fit") {

      return(p1)

    }

    if (mode == "residuals") {

      return(p2)

    }

    if (mode  == "both") {

      p3 <- plotly::subplot(p1, p2,
                            nrows = 2,
                            heights = c((2/3), (1/3)),
                            widths = c(1),
                            shareX = TRUE,
                            titleY = TRUE)

      return(p3)

    }

  } else {

    if (mode == "fit") {

      return(g1)

    }

    if (mode == "residuals") {

      return(g2)

    }

    if (mode == "both") {

      g3 <- ggpubr::ggarrange(g1, g2, nrow = 2,
                              heights = c(2,1))
      return(g3)

    }

  }

}

#' Plotting elements of a powdRlm object
#'
#' \code{plot.powdRlm} is designed to provide easy, adaptable plots
#' of full pattern summation outputs produced from \code{\link{fps_lm}}.
#'
#' When seeking to inspect the results from full pattern summation, interactive
#' plots are particularly useful and can be specified with the \code{interactive}
#' argument.
#'
#' @param x a powdRfps object
#' @param wavelength One of "Cu", "Co" or a custom numeric value defining the wavelength
#' (in Angstroms). Used to compute d-spacings.When "Cu" or "Co" are supplied, wavelengths
#' of 1.54056 or 1.78897 are used, respectively.
#' @param mode One of "fit", "residuals" or "both" defining whether to plot the fitted
#' patterns, the residuals of the fit, or both, respectively. Default = "fit".
#' @param xlim A numeric vector of length two providing limits of the x-axis. Defaults
#' to full x-axis unless specified.
#' @param group A logical parameter used to specify whether the plotted data are grouped
#' according to the phase name. Default = FALSE.
#' @param interactive logical. If TRUE then the output will be an interactive
#' ggplotly object. If FALSE then the output will be a ggplot object.
#' @param ... other arguments
#'
#' @method plot powdRlm
#' @examples
#' #Load the rockjock library
#' data(rockjock)
#'
#' # Load the rockjock loadings data
#' data(rockjock_loadings)
#'
#' \dontrun{
#' fps_lm_out <- fps_lm(rockjock,
#'                      smpl = rockjock_loadings$Dim.1,
#'                      refs = rockjock$phases$phase_id,
#'                      std = "QUARTZ",
#'                      align = 0.3,
#'                      p = 0.01)
#'
#' plot(fps_lm_out,
#'      wavelength = "Cu",
#'      interactive = TRUE,
#'      group = TRUE)
#'
#' }
#' @export
plot.powdRlm <- function(x, wavelength, mode, xlim, interactive, group, ...) {

  if (missing(xlim)) {

    xlim <- c(min(x$tth), max(x$tth))

  }

  if (missing(group)) {

    group <- FALSE

  }

  if (!is.logical(group)) {

    stop("group must be logical",
         call. = FALSE)

  }

  if (length(xlim) > 2) {

    stop("xlim must be a numeric vector of length 2",
         call. = FALSE)

  }

  if (!is.numeric(xlim)) {

    stop("xlim must be a numeric vector of length 2",
         call. = FALSE)

  }

  if (xlim[1] < min(x$tth) | xlim[2] > max(x$tth)) {

    stop("The limits defined in xlim are outside of the 2theta range of the data",
         call. = FALSE)

  }

  if (missing(mode)) {

    mode <- "fit"

  }

  if (!mode %in% c("fit", "residuals", "both")) {

    stop("The mode arugment must be one of `fit`, `residuals`, or `both`",
         call. = FALSE)

  }

  if(missing(interactive)) {
    interactive <- FALSE
  }

  if(!missing(interactive) & !is.logical(interactive)) {
    stop("The interactive arugment must be logical.",
         call. = FALSE)
  }

  #If wavelength is missing then stop the function call
  if (missing(wavelength)) {

    stop("Provide a wavelength so that d-spacings can be calculated",
         call. = FALSE)

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Cu") {

    wavelength <- 1.54056

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Co") {

    wavelength <- 1.78897

  }

  #At this point if wavelength isn't numeric then stop
  if (!is.numeric(wavelength)) {

    stop("The wavelength argument must be one of either 'Cu', 'Co', or
         a custom numeric value",
         call. = FALSE)

  }


  #-----------------------------------
  #Optional grouping
  #-----------------------------------

  if (group == TRUE) {

    x <- .group_patterns(x = x)

  }

  #compute d
  d_v <- round(wavelength/(2*sin((x$tth/2)*pi/180)), 3)

  #Create a dataframe of the weighted pure patterns and fitted pattern
  pure_patterns <- data.frame(tth = x$tth,
                              d = d_v,
                              Measured = x$measured,
                              Fitted = x$fitted,
                              x$weighted_pure_patterns,
                              check.names = FALSE)

  refs_colors <- .gg_color_hue(ncol(x$weighted_pure_patterns))

  #Residuals
  resids <- data.frame(tth = x$tth,
                       d = d_v,
                       Counts = x$residuals)

  #melt the pure patterns data frame
  pure_patterns_long <- reshape::melt(pure_patterns, id = c("tth", "d"))

  #Name the counts column
  names(pure_patterns_long)[4] <- "Counts"


  g1 <- suppressWarnings(ggplot2::ggplot() +
                           ggplot2::geom_line(data = pure_patterns_long,
                                              ggplot2::aes_(x = ~tth, y = ~Counts,
                                                            color = ~variable,
                                                            linetype = ~variable,
                                                            d = ~d),
                                              size = 0.25) +
                           ggplot2::scale_color_manual(values = c("black", "red",
                                                                  refs_colors)) +
                           ggplot2::scale_linetype_manual(values = c("solid", "solid",
                                                                     rep("dotted", ncol(x$weighted_pure_patterns)))) +
                           ggplot2::ylab("Counts") +
                           ggplot2::xlab("2theta") +
                           ggplot2::scale_x_continuous(limits = xlim) +
                           ggplot2::theme(legend.title = ggplot2::element_blank()))

  g2 <- suppressWarnings(ggplot2::ggplot() +
                           ggplot2::geom_line(data = resids,
                                              ggplot2::aes_(x = ~tth, y = ~Counts, color = "Residuals", d = ~d), size = 0.15) +
                           ggplot2::ylab("Counts") +
                           ggplot2::xlab("2theta") +
                           ggplot2::scale_x_continuous(limits = xlim) +
                           ggplot2::scale_colour_manual(name = "",
                                                        values = c("Residuals" = "blue")))


  if (interactive == TRUE) {

    #Convert to ggplotly
    p1 <- plotly::ggplotly(g1, tooltip = c("x", "y", "d", "colour"))
    p2 <- plotly::ggplotly(g2, tooltip = c("x", "y", "d", "colour"))


    if (mode == "fit") {

      return(p1)

    }

    if (mode == "residuals") {

      return(p2)

    }

    if (mode  == "both") {

      p3 <- plotly::subplot(p1, p2,
                            nrows = 2,
                            heights = c((2/3), (1/3)),
                            widths = c(1),
                            shareX = TRUE,
                            titleY = TRUE)

      return(p3)

    }

  } else {

    if (mode == "fit") {

      return(g1)

    }

    if (mode == "residuals") {

      return(g2)

    }

    if (mode == "both") {

      g3 <- ggpubr::ggarrange(g1, g2, nrow = 2,
                              heights = c(2,1))
      return(g3)

    }

  }

}


#' Plotting elements of a powdRlib object
#'
#' \code{plot.powdRlib} is designed to provide easy, adaptable plots
#' of an XRPD reference library built using the \code{powdRlib} constructor
#' function.
#'
#' Plots can be made interactive using the logical \code{interactive} argument.
#'
#' @param x a powdRlib object
#' @param wavelength One of "Cu", "Co" or a custom numeric value defining the wavelength
#' (in Angstroms). Used to compute d-spacings.When "Cu" or "Co" are supplied, wavelengths
#' of 1.54056 or 1.78897 are used, respectively.
#' @param refs a character string of reference pattern id's to be plotted
#' @param interactive Logical. If TRUE then the output will be an interactive
#' ggplotly object. If FALSE then the output will be a ggplot object.
#' @param ... other arguments
#'
#' @method plot powdRlib
#'
#' @examples
#' # Load the minerals library
#' data(minerals)
#' \dontrun{
#' plot(minerals, wavelength = "Cu", refs = "ALB")
#' plot(minerals, wavelength = "Cu", refs = "ALB", interactive = TRUE)
#' }
#' @export
plot.powdRlib <- function(x, wavelength, refs, interactive, ...) {

  #If a pattern is specified but isn't there, then stop
  if (!missing(refs)) {

    if (!length(which(refs %in% x$phases$phase_id)) == length(refs)) {
      stop("Not all refs defined relate to phase ID's in the library",
           call. = FALSE)
    }

  }

  if(missing(refs)) {
    refs <- c("")
  }

  #If wavelength is missing then stop the function call
  if (missing(wavelength)) {

    stop("Provide a wavelength so that d-spacings can be calculated",
         call. = FALSE)

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Cu") {

    wavelength <- 1.54056

  }

  #If wavelength = "Cu" then define it
  if (wavelength == "Co") {

    wavelength <- 1.78897

  }

  #At this point if wavelength isn't numeric then stop
  if (!is.numeric(wavelength)) {

    stop("The wavelength argument must be one of either 'Cu', 'Co', or
         a custom numeric value",
         call. = FALSE)

  }

  if(!missing(refs) & !is.character(refs)) {
    stop("The refs argument must be a character string
         of the reference pattern ID's to be plotted",
         call. = FALSE)
  }

  if(missing(interactive)) {
    interactive <- FALSE
  }

  if(!missing(interactive) & !is.logical(interactive)) {
    stop("The interactive arugment must be logical.",
         call. = FALSE)
  }

  d_v <- round(wavelength/(2*sin((x$tth/2)*pi/180)), 3)

  melted <- reshape::melt(data.frame("tth" = x[[2]],
                                     "d" = d_v,
                                     x[[1]],
                                     check.names = FALSE), id = c("tth", "d"))

  names(melted) <- c("tth", "d", "phase", "Counts")

  if(length(which(melted$phase %in% refs)) > 0) {
    melted <- melted[which(melted$phase %in% refs), ]
  }


  p <- suppressWarnings(ggplot2::ggplot(data = melted) +
    ggplot2::geom_line(ggplot2::aes_(x = ~tth, y = ~Counts,
                           color = ~phase, d = ~d),
                       size = 0.15) +
    ggplot2::xlab("2theta") +
    ggplot2::ylab("Counts") +
    ggplot2::theme(legend.title = ggplot2::element_blank()))


  if(interactive == TRUE) {
    p <- plotly::ggplotly(p)
  }

  return(p)

}


#' Plotting a powdRbkg object
#'
#' \code{plot.powdRbkg} is designed to provide quick plots to inspect the
#' fitted backgrounds obtained from \code{bkg}.
#'
#' The only mandatory argument is x, which must be a powdRbkg object. Plots can
#' be made interactive using the logical \code{interactive} argument.
#'
#' @param x a powdRlib object
#' @param interactive Logical. If TRUE then the output will be an interactive
#' ggplotly object. If FALSE then the output will be a ggplot object.
#' @param ... other arguments
#'
#' @method plot powdRbkg
#'
#' @examples
#' # Load the minerals library
#' data(minerals)
#'
#' \dontrun{
#' plot(minerals, interactive = TRUE)
#' }
#' @export
plot.powdRbkg <- function(x, interactive, ...) {

  if(missing(interactive)) {
    interactive <- FALSE
  }

  if(!missing(interactive) & !is.logical(interactive)) {
    stop("The interactive arugment must be logical.",
         call. = FALSE)
  }

  observed <- data.frame("tth" = x[[1]],
                         "counts" = x[[2]],
                         "id" = rep("Observed", length(x[[1]])))

  fitted_bkg <- data.frame("tth" = x[[1]],
                           "counts" = x[[3]],
                           "id" = rep("Background", length(x[[1]])))

  df <- rbind(observed, fitted_bkg)

  p <- ggplot2::ggplot(data = data.frame(df)) +
    ggplot2::geom_line(ggplot2::aes_(x = ~tth, y = ~counts,
                                     color = ~id),
                       size = 0.15) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::xlab("2theta") +
    ggplot2::ylab("Counts") +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  if(interactive == TRUE) {
    p <- plotly::ggplotly(p)
  }

  return(p)

  }
