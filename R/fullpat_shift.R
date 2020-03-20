#Create a function that adds the shift to the 2theta axis
.shift_list <- function(x, y) {

  x <- data.frame("tth" = x[[1]] + y,
                  "counts" = x[[2]])

}

.fullpat_shift <- function(smpl, lib, par_shift, limit) {

if (!missing(limit)) {

  par_shift[which(par_shift > limit | par_shift < -limit)] <- 0

}

#Create a list of xy patterns from the xrd data
l <- lapply(lib$xrd, function(x) data.frame("tth" = lib$tth,
                                                 "counts" = x))

#Now create a list of the shifted patterns
l2 <- mapply(.shift_list, x = l, y = par_shift, SIMPLIFY = FALSE)


#Calculate the tth range to interpolate to
tth_min <- max(unlist(lapply(l2, function(x) min(x[[1]]))))
tth_max <- min(unlist(lapply(l2, function(x) max(x[[1]]))))
tth_res <- mean(diff(lib$tth))

new_tth <- seq(from = tth_min, to = tth_max, by = tth_res)

#Now approx all of the shifted patterns
l3 <- lapply(l2,
             function(n) stats::approx(x = n[[1]],
                                       y = n[[2]],
                                       xout = new_tth))

#Now create a data frame of the shifted xrd data
lib$xrd <- data.frame(lapply(l3, function(x) x[[2]]))
lib$tth <- new_tth

#
smpl <- data.frame(stats::approx(x = smpl[[1]],
                        y = smpl[[2]],
                        xout = new_tth))

names(smpl) <- c("tth", "counts")

return(list("smpl" = smpl,
            "lib" = lib))

}


.fullpat_shift_optim <- function (par, tth, lib, smpl, obj) {

    par_w <- par[1:(length(par)/2)]
    par_s <- par[((length(par)/2)+1):length(par)]

    #Shift the data
    fs <- .fullpat_shift(smpl = smpl,
                         lib = lib,
                         par_shift = par_s)

    #This calculates the fitted pattern
    s_mix <- apply(sweep(fs$lib$xrd, 2, par_w, "*"),
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


