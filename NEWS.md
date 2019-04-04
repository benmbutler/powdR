# powdR 1.0.0.9000

## New features
*  Outputs from `fps()` and `afps()` (`powdRfps` and `powdRafps` objects, respectively) contain
   an `inputs` components which is a list of each of the arguments (including defaults) used to
   produce the fit.

*  `summarise_mineralogy` is a new function that creates a summary table from lists containing
   multiple `powdRfps` and `powdRafps` objects.

*  A comprehensive reference library of pure phases from the RockJock computer software is now
   provided as an example `powdRlib` object called `rockjock`. This library covers most clay,
   non-clay and amorphous phases that may be encountered in soil samples. The library can be
   loaded into the global environment via `data(rockjock)`. Data of synthetic mineral mixtures
   are also now provided in the `rockjock_mixtures` data, which can be used to test the accuracy
   of full pattern summation via the `fps()` and `afps()` functions. 

*  `fps()` and `afps()` now accept "L-BFGS-B" in the `solver` argument. If selected, this uses
   L-BFGS-B optimisation constrained so that parameters cannot be lower than zero.

*  `fps()` now contains an optional `shift` argument, identical to that already implemented in
   `afps()`. This defines the 2theta range within with a grid-search algorithm can optimise the
    aligment of standards to the sample. If not defined in the function call it defaults to 0.
    
*  `fps()` and `afps()` now have a `shift_res` argument which accepts a single integer to define
   the increase in resolution used during grid search shifting. Higher values facilitate finer
   shifts at the expense of longer computation. If not defined in the function call it defaults
   to 4.
   
*  `fps()` and `afps()` now have a logical `manual_align` argument which specifies whether to
   manually align the sample to the value specified in the `align` argument (`manual_align = TRUE`),
   or optimise the alignment based on a maximum shift defined in the `align` argument 
   (`manual_align = FALSE`).
   
*  `fps()` and `afps()` now have a logical `harmonise` argument which specifies whether to
   automatically harmonise the sample and library onto the same 2theta scale via linear interpolation.

*  The `lod` argument of `afps()`, now simply represents an estimate of the limit of detection of
   the selected internal standard defined by the `std` argument. The function then uses the reference
   intensity ratios to estimate limits of detection for all other phases.
   
*  `fps()` now contains an optional `remove_trace` argument that allows the user to exclude phases
   below a small trace value that would unlikely be detected. Default = 0.

*  `subset()` is a new function that allows simple subsetting of a `powdRlib` object.

*  The `run_powdR()` shiny app now contains a tab for subsetting a `powdRlib` object via `subset()`
   function. This replaces the background fitting tab previously available in `powdR` version 0.2.0.
   
*  The `run_powdR()` shiny app now contains a tab for editing results from `powdRfps` and `powdRafps`
   objects.
   
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
