.shift <- function(smpl, lib, max_shift, x, res, obj) {

  #create a blank list
  pure_patterns <- list()

  #Create a new sample pattern with res times the resolution of the original
  sample_pattern <- data.frame(stats::approx(x = smpl[,1], y = smpl[,2],
                                             method = "linear", n = length(smpl[,1]) * res))

  TTH_long <- sample_pattern[[1]]
  sample_long <- sample_pattern[[2]]

  #Do the same for all data in the reference library
  for (i in 1:ncol(lib$xrd)) {
    pure_patterns[[i]] <- stats::approx(x = lib$tth, y = lib$xrd[, i],
                                        method = "linear", n = nrow(lib$xrd) * res)[[2]]
  }

  #convert from list to data frame, to matrix
  if (length(pure_patterns) == 1) {
    pure_patterns <- data.frame("phase" = pure_patterns[[1]])
    names(pure_patterns) <- names(lib$xrd)
    pure_patterns <- as.matrix(pure_patterns)
  } else {
    pure_patterns <- data.frame(pure_patterns)
    names(pure_patterns) <- names(lib$xrd)
    pure_patterns <- as.matrix(pure_patterns)
  }

  #Define a value that will be used to shift the data.
  TTH_res <- (TTH_long[length(TTH_long)] - TTH_long[1])/(length(TTH_long)-1)
  #Round up the number of increments the data can shift by (based on max_shift)
  shift_value <- round(max_shift/TTH_res)

  #Shorten the sample pattern and 2theta to account for
  #the maximum/minimum shifts that might be applied
  sample_long <- sample_long[((shift_value + 1):(length(sample_long)-shift_value))]
  TTH_long <- TTH_long[((shift_value + 1):(length(TTH_long) - shift_value))]

  #define an integer sequence of positive and negative shifts
  initial_shift <- c((0 - shift_value):shift_value)

  shifting_length <- nrow(pure_patterns)-shift_value

  #Create a matrix of the shortened length that will be used during alignment
  shift_mat <- pure_patterns[((shift_value + 1):(shifting_length)), ]

  if (ncol(pure_patterns) == 1) {
    shift_mat <- data.frame("phase" = shift_mat)
    names(shift_mat) <- names(lib$xrd)
    shift_mat <- as.matrix(shift_mat)
  }

  #define blank lists to be populated during alignment
  v <- list()
  vm <- list()
  vf <- list()
  d <- list()

  #This vector will be used to identify the optimum shift
  #dmin <- c()

  #This vs matrix will be populated with the aligned patterns
  vs <- shift_mat

  cat("\n-Shifting patterns")

  for (i in 1:ncol(shift_mat)) {
    for (j in 1:length(initial_shift)) {

      v[[j]] <- pure_patterns[c(((shift_value + 1) + (initial_shift[j])):(shifting_length + (initial_shift[j]))), i]

      #adjusted matrix for each shift

      vm[[j]] <- shift_mat
      # #add the shifted data
      vm[[j]][,i] <- v[[j]]

      # compute the fitted pattern
      vf[[j]] <- apply(sweep(vm[[j]], 2, x, "*"), 1, sum)

      #Compute Rwp or NNLS is defined in obj then compute the Rwp
      if (obj %in% c("Rwp", "NNLS")) {

      d[[j]] <- sqrt(sum((1/sample_long) * ((sample_long - vf[[j]])^2)) /
                       sum((1/sample_long) * (sample_long^2)))

      }

      #Compute the Delta is defined in obj
      if (obj == "Delta") {

      d[[j]] <- sum(abs(sample_long - vf[[j]]))

      }

      #Compute R if defined in obj
      if (obj == "R") {

        d[[j]] <- sqrt(sum((sample_long - vf[[j]])^2)/sum(sample_long^2))

      }

      #identify which shifted pattern results in minimum Delta
      #dmin[[i]] <- which.min(d)

      #Populate a library with the optimumly shifted references
      vs[, i] <- vm[[which.min(d)]][, i]
    }
  }

  #re-approximate the data to the old TTH resolution (i.e. reduce by res times)
  vs_short <- list()

  #re-approximate the reference library
  for (i in 1:ncol(vs)) {
    vs_short[[i]] <- stats::approx(x = 1:nrow(vs), y = vs[ , i], method = "linear", n = (nrow(vs) / res))[[2]]
  }
  #Convert from list to data frame to matrix

  if (length(vs_short) == 1) {
    vs_short <- data.frame("phase" = vs_short[[1]])
    names(vs_short) <- names(data.frame(vs))
    vs_short <- as.matrix(vs_short)
  } else {
    vs_short <- data.frame(vs_short)
    names(vs_short) <- names(data.frame(vs))
    vs_short <- as.matrix(vs_short)
  }

  #reapproximate the sample
  sample_pattern <- stats::approx(x = TTH_long, y = sample_long, method = "linear", n = (nrow(vs) / res))[[2]]

  #reapproximate the 2theta
  TTH_short <- stats::approx(x = TTH_long, y = vs[, 1], method = "linear", n = (nrow(vs) / res))[[1]]

  vs <- vs_short
  TTH <- TTH_short

  out <- list("smpl" = data.frame("tth" = TTH, "counts" = sample_pattern),
              "lib" = vs)

}
