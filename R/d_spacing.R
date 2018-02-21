d.spacing <- function(x, wavelength) {

#Calculating d-spacing

d <- wavelength/(2*sin((x/2)*pi/180))

return(d)
}

