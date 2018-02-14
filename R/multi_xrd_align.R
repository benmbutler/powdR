
#This function aligns AND harmonises a list of XRD patterns
multi.xrd.align <- function(xrd, xrd.standard, xmin, xmax, xshift) {

  quartz <- xrd.standard

  xrd_aligned <- list()
  min_tth <- c()
  max_tth <- c()

  for (i in 1:length(xrd)) {
    x <- xrd.align(xrd.sample = xrd[[i]], xrd.standard = quartz,
                      xmin = xmin,
                      xmax = xmax,
                      xshift = xshift)

    xrd_aligned[[i]] <- x

    min_tth[i] <- min(x[,1])
    max_tth[i] <- max(x[,1])
  }

  #Define the tth interval that a new scale will be built upon
  tth_interval <- (max(xrd[[1]][, 1]) - min(xrd[[1]][, 1])) / nrow(xrd[[1]])

  #Create a new 2th scale based on the shifts of the data
  tth <- seq(from = max(min_tth), to = min(max_tth), by = tth_interval)

  #Harmonise the aligned data to the new 2TH scale
  xrd_harm <- list()
  for (i in 1:length(xrd_aligned)) {
    xrd_harm[[i]] <- data.frame(approx(x = xrd_aligned[[i]][, 1],
                                        y = xrd_aligned[[i]][, 2],
                                        xout = tth))
  }

  #preserve names
  names(xrd_harm) <- names(xrd)

  xrd <- xrd_harm

  return(xrd)

}
