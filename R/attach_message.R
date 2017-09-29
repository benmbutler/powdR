### This function loads a start up message when my powdR package is loaded

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the powdR package")
}
