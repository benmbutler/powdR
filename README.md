powdR: Full Pattern Summation of X-Ray Powder Diffraction Data
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.com/benmbutler/powdR.svg?branch=master)](https://travis-ci.com/benmbutler/powdR)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/powdR)](https://CRAN.R-project.org/package=powdR)
[![Total\_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/powdR)](https://cran.r-project.org/package=powdR)

## Overview

`powdR` is an implementation of the full pattern summation approach to
quantitative mineralogy from X-ray powder diffraction data (Chipera and
Bish 2002, 2013; Eberl 2003).

`powdR` has several advantages over the excel based implementations of
full pattern summation such as FULLPAT (Chipera and Bish 2002) and
RockJock (Eberl 2003). First, computation is faster and, when
quantifying multiple samples, can be used in combination with other
packages (e.g [`foreach`](https://cran.r-project.org/package=foreach)
and [`doParallel`](https://cran.r-project.org/package=doParallel)) for
parallel processing. Secondly, powdR can be run via a Shiny web
application, which offers a user friendly interface for fast and
iterative mineral quantification. Lastly, R represents a powerful tool
for data manipulation, allowing users to creatively adapt, pre-treat and
visualise their data.

## Installation

The stable version of `powdR` is on CRAN:

``` r
install.packages("powdR")
```

Alternatively, the development version can be downloaded from GitHub

``` r
#Install devtools if you don't already have it on your machine
install.packages("devtools")

devtools::install_github('benmbutler/powdR')
```

## Usage

``` r
library(powdR)
#> powdR: Full Pattern Summation of X-Ray Powder Diffraction Data

#Load some soils to quantify
data(soils)

#Load a powdRlib reference library of pure patterns
data(minerals)

#Quantify a sample
q <-  fps(lib = minerals,
          smpl = soils$sandstone,
          refs = minerals$phases$phase_id,
          std = "QUA.1")
#> 
#> -Aligning sample to the internal standard
#> -Interpolating library to same 2theta scale as aligned sample
#> -Optimising...
#> -Removing negative coefficients and reoptimising...
#> -Computing phase concentrations
#> -Internal standard concentration unknown. Assuming phases sum to 100 %
#> ***Full pattern summation complete***

#Inspect the phase concentrations (summarised by name)
q$phases_grouped
#>       phase_name phase_percent
#> 1         Quartz       53.6918
#> 2     K-feldspar        1.3184
#> 3    Plagioclase        1.1637
#> 4         Illite        1.3515
#> 5      Kaolinite        1.3697
#> 6 Organic-Matter       41.1049

#Inspect the quantification
plot(q, wavelength = "Cu")
```

![](man/figures/README-example-1.png)<!-- -->

Alternatively, `plot(q, wavelength = "Cu", interactive = TRUE)` provides
an interactive plot for better inspection of the fit. More detailed
usage instructions are provided in the package vignette.

## The powdR Shiny app

To run `powdR` via the Shiny app, use `run_powdR()`. This loads the
application in your default web browser. The application has six tabs:

1.  **Reference Library Builder:** Allows you to create and export a
    `powdRlib` reference library from two .csv files: one for the XRPD
    measurements, and the other for the ID, name and reference intensity
    ratio of each pattern.
2.  **Reference Library Viewer:** Facilitates quick inspection of the
    phases within a `powdRlib` reference library.
3.  **Reference Library Editor:** Allows the user to easily subset a
    `powdRlib` reference library .
4.  **Full Pattern Summation:** A user friendly interface for manual and
    automated full pattern summation of single samples.
5.  **Results Viewer/Editor:** Allows for results from previously saved
    `powdRfps` and `powdRafps` objects to be viewed and edited via
    addition or removal of reference patterns.
6.  **Help** Provides a series of video tutorials (via YouTube)
    detailing the use of the `powdR` Shiny application.

**Please note that the `powdR` Shiny app may not work in the Microsoft
Edge internet browser.**

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Chipera2002" class="csl-entry">

Chipera, Steve J., and David L. Bish. 2002. “<span
class="nocase">FULLPAT: A full-pattern quantitative analysis program for
X-ray powder diffraction using measured and calculated patterns</span>.”
*Journal of Applied Crystallography* 35 (6): 744–49.
<https://doi.org/10.1107/S0021889802017405>.

</div>

<div id="ref-Chipera2013" class="csl-entry">

———. 2013. “<span class="nocase">Fitting Full X-Ray Diffraction Patterns
for Quantitative Analysis: A Method for Readily Quantifying Crystalline
and Disordered Phases</span>.” *Advances in Materials Physics and
Chemistry* 03 (01): 47–53. <https://doi.org/10.4236/ampc.2013.31A007>.

</div>

<div id="ref-Eberl2003" class="csl-entry">

Eberl, D. D. 2003. “<span class="nocase">User’s guide to ROCKJOCK - A
program for determining quantitative mineralogy from powder X-ray
diffraction data</span>.” Boulder, CA: USGS.

</div>

</div>
