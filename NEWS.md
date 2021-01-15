# powdR 1.2.5

## New features
# `fps()` and `afps()` now accept diffraction data that has negative values for count
  intensities. In such cases Rwp cannot be used as the objective function and R will
  be used as the default instead.
# The `rwp` item in the outputs from `fps()` and `afps()` has been renamed `obj`, which
  contains a named vector of the values for three objective parameters: Rwp, R and
  Delta.
# `summarise_mineralogy` now accepts two additional arguments: `r` and `delta` which are
  logical parameters used to specify whether the R and Delta objective parameters,
  respectively,are included in the summary table.

# powdR 1.2.4

## New features
# `powdRlib()` now accepts a logical `check_names` argument. If `TRUE` (the default) then
  the names of the variables are checked to ensure that they are syntactically valid and
  are not duplicated.

## Bug fixes
# Default value for the normalise argument in `fps()` and `afps()` is now supplied
  (`FALSE`)
# `powdRlib()` now ensures that the `phases` object is a dataframe.

# powdR 1.2.3

## New features
* New function `regroup()` allows for an alternative mineral grouping structure to be applied to
  `powdRfps` and `powdRafps` objects.
* New function `tth_transform()` allows 2theta transformation between different monochromatic X-ray
  wavelengths.
* `fps()` and `afps()` now accept a logical `normalise` argument, which allows the internal standard
  concentration to be omitted and phase concentrations subsequently normalised to sum to 100 %.

# powdR 1.2.2

## New features
* `fps()` now accepts the `force` argument, forcing phases to remain in the final output even if their
  coefficients are negative.
* `fps()` and `afps()` will now stop if any of the phases specified in the `refs` argument are not
  in the library.
* Warning messages now generally do not include the call.
* Progress bar now included in the Shiny app when computing `fps()` and `afps()`.
* `summarise_mineralogy()` now accepts single samples (i.e. a list of 1 `powdRfps` or `powdRafps` object).

## Bug fixes
* `utils` no longer in imports (hence fixing associated note in CRAN checks).

# powdR 1.2.1

## New features
* `plot()` methods for `powdRfps` and `powdRafps` objects now include `mode`
  and `xlim` arguments, allowing for different plot types and x-axis adjustment.

## Bug fixes
* Dependency on packages `DT` and `shinyWidgets` now defined in namespace.
* Bug stopping the shiny app working in shiny versions >1.3.2 now fixed.
* Couple of other small edits to phase selection in the shiny app that should
  keep things simple.
* `powdRlib()` no longer orders the reference patterns alphabetically, and instead
  retains the original order that they are supplied in.
* Negative values from natural spline interpolation now removed from data as these
  can prevent the optimisation routines from working.
* Phases are no longer ordered alphabetically in outputs from `fps()` and `afps()`.

# powdR 1.2.0

## New features
* `fps()` and `afps()` no longer require the `shift_res` argument.
* Shifting is now calculated by optimisation of the objective function.
* Natural splines are now used instead of linear splines throughout `fps()` and `afps()`.
* The Full Pattern Summation tab in the Shiny application has been updated, as has the video
  tutorial for it.

# powdR 1.1.0

## New features
* The `refs` argument of `fps()` and `subset()` now accepts phase names as well as phase ID's. For example, if
  the phase name "Quartz" in supplied, then all phase ID's associated with Quartz will be selected.
  
* Similarly, the `force` argument of `afps()` now accepts both phase names and phase ID's.

* `summarise_mineralogy()` now contains an optional `rwp` argument (default = `FALSE`). This is a logical
  parameter used to define whether the Rwp should be included in the summary table as a measure of the
  difference between the measured and fitted patterns.
  
* When the `std_conc` argument is supplied to `fps()` or `afps()`, the computed phase concentrations now
  include that of the internal standard.

# powdR 1.0.0

## New features
*  Outputs from `fps()` and `afps()` (`powdRfps` and `powdRafps` objects, respectively) contain
   an `inputs` component. This provides a list of each of the arguments (including defaults) used to
   produce the fit.

*  `summarise_mineralogy()` is a new function that creates a summary table from lists containing
   multiple `powdRfps` and/or `powdRafps` objects.

*  A comprehensive reference library of pure phases from the RockJock computer software is now
   provided as an example `powdRlib` object called `rockjock`. This library covers most clay,
   non-clay and amorphous phases that may be encountered in soil samples. The library can be
   loaded into the global environment via `data(rockjock)`. Data of synthetic mineral mixtures
   are also now provided in the `rockjock_mixtures` data, which can be used to test the accuracy
   of full pattern summation via the `fps()` and `afps()` functions. 

*  `fps()` and `afps()` now accept "L-BFGS-B" in the `solver` argument. If selected, this uses
   L-BFGS-B optimisation constrained so that parameters cannot be lower than zero.

*  `fps()` now contains an optional `shift` argument, identical to that already implemented in
   `afps()`. This defines the 2$\theta$ range within with a grid-search algorithm can optimise the
    alignment of standards to the sample. If not defined in the function call it defaults to 0.
    
*  `fps()` and `afps()` now have a `shift_res` argument which accepts a single integer to define
   the increase in resolution used during grid search shifting. Higher values facilitate finer
   shifts at the expense of longer computation. If not defined in the function call it defaults
   to 4.
   
*  `fps()` and `afps()` now have a logical `manual_align` argument which specifies whether to
   manually align the sample to the value specified in the `align` argument (`manual_align = TRUE`),
   or optimise the alignment based on a maximum shift defined in the `align` argument 
   (`manual_align = FALSE`).
   
*  `fps()` and `afps()` now have a logical `harmonise` argument which specifies whether to
   automatically harmonise the sample and library onto the same 2$\theta$ scale via linear interpolation.

*  The `lod` argument of `afps()`, now simply represents an estimate of the limit of detection of
   the selected internal standard defined by the `std` argument. The function then uses the reference
   intensity ratios to estimate limits of detection for all other phases.
   
*  `fps()` now contains an optional `remove_trace` argument that allows the user to exclude phases
   below a small trace value that would unlikely be detected. Default = 0.

*  `subset()` is a new function that allows simple subsetting of a `powdRlib` object.

*  The `run_powdR()` shiny app now contains tabs for subsetting a `powdRlib` object via `subset()`
   function, editing`powdRfps` and `powdRafps` objects, and video tutorials.
   
# powdR 0.2.0

## New features

*   Suggests packages `nnls` (>=1.4), `baseline` (>= 1.2) and `shinyWidgets` (>= 0.4.3) in
    the DESCRIPTION.

*   `fps()` now accepts "NNLS" in the `solver` argument. If "NNLS" (non-negative least
    squares) is selected, the algorithm uses non negative least squares instead of
    minimising an objective function. This is a much faster alternative but less
    accurate for samples containing amorphous phases.
    
*   `bkg()` is a new function that allows for backgrounds to be fitted to XRPD data.
    It is a wrapper of the `baseline::baseline.fillPeaks()` method, and the output is
    a `powdRbkg` object.
    
*   `afps()` is a new function that automates the process of full pattern
    summation by firstly selecting samples from the reference library (using NNLS) and
    then excluding those estimated to be below detection limit. The output is a `powdRafps`
    object.
    
*   New `plot()` methods for `powdRbkg` and `powdRafps` objects 
    
*   The shiny application behind `run_powdR()` has been updated to accept "NNLS", and
    now includes tabs for background fitting (using `bkg()`) and automated full pattern
    summation (using `afps()`).
    
# powdR 0.1.0

*   Released on CRAN
