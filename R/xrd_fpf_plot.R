fpf.plot <- function(x, plot.width, plot.height) {

  #library(rJava)

  #This code can be used to access the screen height and width
  # .jinit()
  # toolkit <- J("java.awt.Toolkit")
  # default_toolkit <- .jrcall(toolkit, "getDefaultToolkit")
  # dim <- .jrcall(default_toolkit, "getScreenSize")
  # fig.height <- .jcall(dim, "D", "getHeight")
  # fig.width <- .jcall(dim, "D", "getWidth")

  # x needs to be a list output from the fpf function

  pure_patterns <- data.frame(TTH = x[["TTH"]],
                              FITTED = x[["FITTED"]],
                              x[["WEIGHTED_PURE_PATTERNS"]])

  measured <- data.frame(TTH = x[["TTH"]],
                         MEASURED = x[["MEASURED"]])

  resids <- data.frame(TTH = x[["TTH"]],
                       RESIDUALS = x[["RESIDUALS"]])


  pure_patterns_long <- melt(pure_patterns, id = c("TTH"))


  g1 <- ggplot() +
    geom_line(data = measured,
              aes(x = TTH, y = MEASURED, color = "Measured"), size = 0.35, linetype = "dotted") +
    geom_line(data = pure_patterns_long,
              aes(x = TTH, y = value, color = variable), size = 0.15)

  p1 <- ggplotly(g1, width = plot.width, height = plot.height)


  g2 <- ggplot() +
    geom_line(data = resids,
              aes(x = TTH, y = RESIDUALS, color = "Residuals"), size = 0.15) +
    scale_colour_manual(name = "",
                        values = c("Residuals" = "blue"))

  p2 <- ggplotly(g2, width = plot.width, height = plot.height)

  p3 <- subplot(p1, p2,
                nrows = 2,
                heights = c(0.5, 0.5),
                widths = c(1),
                shareX = TRUE,
                titleY = TRUE)

  return(p3)
}
