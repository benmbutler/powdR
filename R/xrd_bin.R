xrd.bin <- function(smpl, bin.size) {

  #Split the x-axis into chunks defined by bin.size
  x <- split(smpl[[1]], ceiling(seq_along(smpl[[1]])/bin.size))

  #calculate the mean of each chunk
  x_mean <- c()
  for(i in 1:length(x)) {
    x_mean[i] <- mean(x[[i]])
  }

  #Split the y-axis into chunks defined by bin.size
  y <- split(smpl[[2]], ceiling(seq_along(smpl[[2]])/bin.size))

  #calculate the mean of each chunk
  y_mean <- c()
  for(i in 1:length(y)) {
    y_mean[i] <- mean(y[[i]])
  }

  #create a dataframe of binned data
  out <- data.frame("tth" = x_mean,
                    "counts" = y_mean)

  return(out)
}
