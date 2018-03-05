### This function loads a start up message when my powdR package is loaded

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("powdR: Tools for digital mineralogy")
}
