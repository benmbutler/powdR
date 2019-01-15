# powdR 0.2.0.9000

## New features

*  `fps()` and `afps()` now accept "L-BFGS-B" in the `solver` argument. If selected, this uses
   L-BFGS-B optimisation constrained so that parameters cannot be lower than zero.

*  `fps()` now contains an optional `shift` argument, identical to that already implemented in
   `afps()`. This defines the 2theta range within with a grid-search algorithm can optimise the
    aligment of standards to sample. If not defined in the function call it defaults to 0.
    
*  `fps()` and `afps()` now have a `shift_res` argument which accepts a single integer to define
   the increase in resolution used during grid search shifting. Higher values facilitate finer
   shifts at the expense of longer computation. If not defined in the function call it defaults
   to 0.

*  Limit of detection estimation in `afps()` now uses a single peak instead of the whole pattern,
   and therefore `afps()` contains an additional argument, `tth_lod`, which defines the position of
   the internal standards major peak along the 2theta axis.
   
*  `fps()` now contains an optional `remove_trace` argument that allows the user to exclude phases
   below a small trace value that would unlikely be detected. Default = 0.

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
