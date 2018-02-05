#' Interactive full pattern fitting plot
#'
#' \code{fpf.plot} returns a ggplotly interactive graph.
#'
#' This function is for use in combination with the output from \code{fpf}.
#'
#' @param x An \code{fpf} output to be plotted
fpf.plot <- function(x) {

  # x needs to be a list output from the fpf function

  pure_patterns <- data.frame(TTH = x[["TTH"]],
                              FITTED = x[["FITTED"]],
                              x[["WEIGHTED_PURE_PATTERNS"]])

  measured <- data.frame(TTH = x[["TTH"]],
                         MEASURED = x[["MEASURED"]])

  resids <- data.frame(TTH = x[["TTH"]],
                       RESIDUALS = x[["RESIDUALS"]])


  pure_patterns_long <- reshape::melt(pure_patterns, id = c("TTH"))


  g1 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = measured,
              ggplot2::aes(x = TTH, y = MEASURED, color = "Measured"), size = 0.35, linetype = "dotted") +
    ggplot2::geom_line(data = pure_patterns_long,
              ggplot2::aes(x = TTH, y = value, color = variable), size = 0.15)

  p1 <- plotly::ggplotly(g1)


  g2 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = resids,
              ggplot2::aes(x = TTH, y = RESIDUALS, color = "Residuals"), size = 0.15) +
    ggplot2::scale_colour_manual(name = "",
                        values = c("Residuals" = "blue"))

  p2 <- plotly::ggplotly(g2)

  p3 <- plotly::subplot(p1, p2,
                nrows = 2,
                heights = c(0.5, 0.5),
                widths = c(1),
                shareX = TRUE,
                titleY = TRUE)

  return(p3)
}
