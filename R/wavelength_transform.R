wavelength.transform <- function(x, measured.wavelength, new.wavelength) {
  #Calculating d-spacing

  d <- measured.wavelength/(2*sin((x/2)*pi/180))

  #Calculating new TTH based on a new wavelength

  new.tth <- NISTunits::NISTradianTOdeg((asin(new.wavelength/(2*d))*2))
}
