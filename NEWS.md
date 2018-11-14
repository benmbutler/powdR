# powdR 0.1.0.9000

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
