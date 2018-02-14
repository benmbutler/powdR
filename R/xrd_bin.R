xrd.bin <- function(smpl, bin.size) {

  tth <- smpl[[1]]
  counts <- smpl[[2]]

  #Split the x-axis into chunks
  x <- split(tth, ceiling(seq_along(tth)/bin.size))

  #calculate the mean of each chunk
  x_mean <- c()
  for(i in 1:length(x)) {
    x_mean[i] <- mean(x[[i]])
  }

  #Split the y-axis into chunks
  y <- split(counts, ceiling(seq_along(counts)/bin.size))

  #calculate the mean of each chunk
  y_mean <- c()
  for(i in 1:length(y)) {
    y_mean[i] <- mean(y[[i]])
  }

  out <- data.frame("TTH" = x_mean,
                    "COUNTS" = y_mean)

  return(out)
}
