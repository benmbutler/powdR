#fpf Alignment ---------------------------------------------------------------

fpf.align <- function(sample.tth, sample.counts, xrd.lib, fpf_shift, pure.weights, weighting) {

  #create a blank list
  pure.patterns <- list()

  #Create a new 2theta scale with 4 times the resolution of the original sample
  sample.pattern <- data.frame(approx(x = sample.tth, y = sample.counts,
                                      method = "linear", n = length(sample.tth) * 4))

  weighting <- data.frame(approx(x = weighting[,1], y = weighting[,2],
                                 method = "linear", n = length(sample.tth) * 4))

  TTH_long <- sample.pattern[,1]
  sample_long <- sample.pattern[,2]

  #Do the same for all data in the selected reference library
  for (i in 1:ncol(xrd.lib[["XRD"]])) {
    pure.patterns[[i]] <- approx(x = xrd.lib[["TTH"]], y = xrd.lib[["XRD"]][, i],
                                 method = "linear", n = nrow(xrd.lib[["XRD"]]) * 4)[[2]]
  }

  #convert from list to data frame
  pure.patterns <- data.frame(pure.patterns)
  names(pure.patterns) <- names(data.frame(xrd.lib[["XRD"]]))
  pure.patterns <- as.matrix(pure.patterns)


  #Define a value that will be used to shift the data.
  TTH_res <- (TTH_long[length(TTH_long)] - TTH_long[1])/(length(TTH_long)-1)

  shift_value <- round(fpf_shift/TTH_res, 0)

  #Shorten the sample pattern, 2theta and weighting to account for
  #the maximum/minimum shifts that might be applied
  sample_long <- sample_long[((shift_value + 1):(length(sample_long)-shift_value))]
  TTH_long <- TTH_long[((shift_value + 1):(length(TTH_long) - shift_value))]

  weighting <- weighting[((shift_value + 1):(nrow(weighting)-shift_value)), ]


  #define an integer vector of positive and negative shifts
  initial.shift <- c((0 - shift_value):shift_value)

  shifting.length <- nrow(pure.patterns)-shift_value

  #Create a matrix of the shortened length that will be used during alignment
  shift.mat <- pure.patterns[((shift_value + 1):(shifting.length)), ]

  #define blank lists to be populated during alignment
  v <- list()
  vm <- list()
  vf <- list()
  d <- list()

  #This vector will be used to identify the optimum shift
  dmin <- c()

  #This vs matrix will be populated with the aligned patterns
  vs <- shift.mat

  for (i in 1:ncol(shift.mat)) {
    for (j in 1:length(initial.shift)) {

      v[[j]] <- pure.patterns[c(((shift_value + 1) + (initial.shift[j])):(shifting.length + (initial.shift[j]))), i]

      #adjusted matrix for each shift

      vm[[j]] <- shift.mat
      # #add the shifted data
      vm[[j]][,i] <- v[[j]]
      #
      # #compute the fitted pattern
      #
      vf[[j]] <- apply(sweep(vm[[j]], 2, pure.weights, "*"), 1, sum)
      #
      #compute the error
      #d[[j]] <- sqrt(sum(abs(sample_long - vf[[j]])^2))

      #Compute the Rwp
      d[[j]] <- sqrt(sum((1/sample_long) * ((sample_long - vf[[j]])^2* weighting[,2])) / sum((1/sample_long) * (sample_long^2)))

      #identify which shifted pattern results in minimum Rwp
      dmin[[i]] <- which.min(d)

      #Populate a library with the optimumly shifted references
      vs[, i] <- vm[[which.min(d)]][, i]
    }
  }

  #re-approximate the data to the old TTH scale (i.e. reduce by 4 times)

  vs_short <- list()

  #re-approximate the reference library
  for (i in 1:ncol(vs)) {
    vs_short[[i]] <- approx(x = 1:nrow(vs), y = vs[ , i], method = "linear", n = (nrow(vs) / 4))[[2]]
  }
  #Convert from list to data frame
  vs_short <- data.frame(vs_short)
  names(vs_short) <- names(data.frame(vs))
  #convert to matrix
  vs_short <- as.matrix(vs_short)

  #reapproximate the sample
  sample.pattern <- approx(x = TTH_long, y = sample_long, method = "linear", n = (nrow(vs) / 4))[[2]]

  #reapproximate the 2theta
  TTH_short <- approx(x = TTH_long, y = vs[, 1], method = "linear", n = (nrow(vs) / 4))[[1]]

  vs <- vs_short
  TTH <- TTH_short

  out <- list("sample" = data.frame("TTH" = TTH, "COUNTS" = sample.pattern),
              "xrdlib_aligned" = vs)

}
