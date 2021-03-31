#Create a function that adds the shift to the 2theta axis
.shift_list <- function(x, y) {

  x <- data.frame("tth" = x[[1]] + y,
                  "counts" = x[[2]])

}

.fullpat_shift <- function(smpl, lib, par_shift) {

#Create a list of xy patterns from the xrd data
l <- lapply(lib$xrd, function(x) data.frame("tth" = lib$tth,
                                            "counts" = x))

#Now create a list of the shifted patterns
l2 <- mapply(.shift_list, x = l, y = par_shift, SIMPLIFY = FALSE)

#Now spline all of the shifted patterns
l3 <- lapply(l2,
             function(n) stats::spline(x = n[[1]],
                                       y = n[[2]],
                                       method = "natural",
                                       xout = lib$tth))

#Now create a data frame of the shifted xrd data
lib$xrd <- data.frame(lapply(l3, function(x) x[[2]]),
                      check.names = FALSE)

names(smpl) <- c("tth", "counts")

return(list("smpl" = smpl,
            "lib" = lib))

}


# .fullpat_shift_optim <- function (par, lib, smpl, obj) {
#
#     par_w <- par[1:(length(par)/2)]
#     par_s <- par[((length(par)/2)+1):length(par)]
#
#     #Shift the data
#     fs <- .fullpat_shift(smpl = smpl,
#                          lib = lib,
#                          par_shift = par_s)
#
#     #This calculates the fitted pattern
#     s_mix <- apply(sweep(fs$lib$xrd, 2, par_w, "*"),
#                    1, sum)
#
#     #objective functions
#     if(obj == "Delta") {
#       d <- sum(abs(fs$smpl$counts - s_mix))
#     }
#
#     if(obj == "R") {
#       d <- sqrt(sum((fs$smpl$counts - s_mix)^2)/sum(fs$smpl$count^2))
#     }
#
#     if(obj == "Rwp") {
#       d <-  sqrt(sum((1/fs$smpl$counts) * ((fs$smpl$counts - s_mix)^2)) / sum((1/fs$smpl$counts) * (fs$smpl$counts^2)))
#     }
#
#     return(d)
#
# }


#This should be a faster shifting version that does not optimise all coefficients at once
#and instead JUST optimises the shifts
.fullpat_shift_seq <- function (par, weightings, lib, smpl, obj, tth_fps) {

  #check for excluded tth values
  ex_tth <- which(lib$tth < tth_fps[1] | lib$tth > tth_fps[2])

  if (length(ex_tth) > 0) {

    #Remove the rows that are outside of tth_fps range
    lib$xrd <- lib$xrd[-ex_tth,]
    lib$tth <- lib$tth[-ex_tth]
    smpl <- smpl[-ex_tth,]

  }

  #Shift the data
  fs <- .fullpat_shift(smpl = smpl,
                       lib = lib,
                       par_shift = par)

  #This calculates the fitted pattern
  s_mix <- apply(sweep(fs$lib$xrd, 2, weightings, "*"),
                 1, sum)

  #objective functions
  if(obj == "Delta") {
    d <- sum(abs(fs$smpl$counts - s_mix))
  }

  if(obj == "R") {
    d <- sqrt(sum((fs$smpl$counts - s_mix)^2)/sum(fs$smpl$count^2))
  }

  if(obj == "Rwp") {
    d <-  sqrt(sum((1/fs$smpl$counts) * ((fs$smpl$counts - s_mix)^2)) / sum((1/fs$smpl$counts) * (fs$smpl$counts^2)))
  }

  return(d)

}


