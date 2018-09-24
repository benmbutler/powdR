powdR: full pattern summation of XRPD data
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/benmbutler/powdR.svg?branch=master)](https://travis-ci.org/benmbutler/powdR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/powdR)](https://CRAN.R-project.org/package=powdR) [![Total\_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/powdR)](https://cran.r-project.org/package=powdR)

Overview
--------

`powdR` is an implementation of the full pattern summation approach to quantitative mineralogy from X-ray powder diffraction data (Chipera and Bish 2002; Chipera and Bish 2013; Eberl 2003). Version 0.1.0 contains functionality for a standardless approach, which assumes all phases within a sample can be identified, and thus they sum to 100 %. Future versions of `powdR` seek to add functionality for samples prepared with an internal standard.

`powdR` has several advantages over the excel based implementations of full pattern summation such as FULLPAT (Chipera and Bish 2002) and ROCKJOCK (Eberl 2003). First, computation is faster and, when quantifying multiple samples, can be used in combination with other packages (e.g [`foreach`](https://cran.r-project.org/web/packages/foreach/index.html)) for parralel processing. Secondly, powdR can be run via a `shiny` web application, which offers a user friendly interface for fast and iterative mineral quantification. Lastly, R represents a powerful tool for data manipulation, allowing users to creatively adapt, pre-treat and visualise their XRPD data.

Installation
------------

The stable version of `powdR` is on CRAN:

``` r
install.packages("powdR")
```

Alternatively, the development version can be downloaded from Github

``` r
#Install devtools if you don't already have it on your machine
install.packages("devtools")

#install without vignette
devtools::install_github('benmbutler/powdR')

#alternatively, an installation that builds the vignette
devtools::install_github('benmbutler/powdR', build_vignettes = TRUE)
```

Usage
-----

``` r
library(powdR)
#> powdR: full pattern summation of X-ray powder diffraction data

#Load some soils to quantify
data(soils)

#Load a reference library of pure patterns
data(minerals)

#Quantify a sample
q <-  fps(lib = minerals,
          smpl = soils$sandstone,
          refs = minerals$phases$phase_id,
          std = "QUA.1")

#Inspect the phase concentrations (summarised by name)
q$phases_summary
#>       phase_name phase_percent
#> 1         Illite      1.237556
#> 2     K-feldspar      1.250870
#> 3      Kaolinite      1.390756
#> 4 Organic-Matter     39.805850
#> 5    Plagioclase      1.159294
#> 6         Quartz     55.155674

#Inspect the quantification
plot(q)
```

![](man/figures/README-example-1.png)

Alternatively, `plot(q, interactive = TRUE)` provides an interative plot for better inspection of the fit. More detailed usage instructions are provided in the package vignette.

Shiny app
---------

To run `powdR` via the shiny app, use `run_powdR()`. This loads the application in your default web browser. The application has four tabs:

1.  Reference library builder: Allows you to create a reference library from two .csv files: one for the XRPD measurements, and the other for the ID, name and reference intensity ratio of each pattern.
2.  Reference library Viewer: For quick inspection of the phases within a reference library.
3.  Full pattern summation: A user friendly interface for iterative full pattern summation of single samples.
4.  Results viewer: Allows for quick inspection of results derived from full pattern summation.

References
----------

Chipera, Steve J., and David L. Bish. 2002. “FULLPAT: A full-pattern quantitative analysis program for X-ray powder diffraction using measured and calculated patterns.” *Journal of Applied Crystallography* 35 (6): 744–49. doi:[10.1107/S0021889802017405](https://doi.org/10.1107/S0021889802017405).

———. 2013. “Fitting Full X-Ray Diffraction Patterns for Quantitative Analysis: A Method for Readily Quantifying Crystalline and Disordered Phases.” *Advances in Materials Physics and Chemistry* 03 (01): 47–53. doi:[10.4236/ampc.2013.31A007](https://doi.org/10.4236/ampc.2013.31A007).

Eberl, D. D. 2003. “User’s guide to ROCKJOCK - A program for determining quantitative mineralogy from powder X-ray diffraction data.” Boulder, CA: USGS.
