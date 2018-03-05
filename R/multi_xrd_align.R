#Align and harmonise a list of xy XRD patterns
multi.xrd.align <- function(xrd, xrd.standard, xmin, xmax, xshift) {

  #Aligned data will end up in this list
  xrd_aligned <- list()

  #The minimum and maximum 2theta value from each alignment will
  #end up in these vectors
  min_tth <- c()
  max_tth <- c()

  for (i in 1:length(xrd)) {
    x <- xrd.align(xrd.sample = xrd[[i]], xrd.standard = xrd.standard,
                      xmin = xmin,
                      xmax = xmax,
                      xshift = xshift)

    xrd_aligned[[i]] <- x

    min_tth[i] <- min(x[[2]][,1])
    max_tth[i] <- max(x[[2]][,1])
  }

  #Define the 2theta resolution that a new scale will be built upon. Based on
  #the first pattern in the list
  tth_interval <- (max(xrd[[1]][, 1]) - min(xrd[[1]][, 1])) / nrow(xrd[[1]])

  #Create a new 2th scale based on the shifts of the data so that no NA
  #values result from the subsequent linear spline
  tth <- seq(from = max(min_tth), to = min(max_tth), by = tth_interval)

  #Harmonise the aligned data to the new 2TH scale
  xrd_harm <- list()
  for (i in 1:length(xrd_aligned)) {
    xrd_harm[[i]] <- data.frame(approx(x = xrd_aligned[[i]][[2]][, 1],
                                       y = xrd_aligned[[i]][[2]][, 2],
                                       method = "linear",
                                       xout = tth))
  }

  #preserve names
  names(xrd_harm) <- names(xrd)

  return(xrd_harm)

}
