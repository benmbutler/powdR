# powdR 0.1.0.9000

*   `powdR` now suggests the `nnls` (>=1.4) package in the DESCRIPTION.

*   `fps` now has accepts "NNLS" in the `solver` argument. If "NNLS" is selected,
    the algorithm uses non negative least squares instaed of minimising an objective
    function.
    
*   The shiny application behind `run_powdR` has been updated to accept "NNLS"

*   The shiny application now has tabs for background fitting and automated full
    pattern summation.
    
*   An `afps` function is now included that seeks to automated the process of full
    pattern summation by selecting samples from the reference library and excluding
    those estimated to be below detection limit.
    
*   The `bkg` function allows for backgrounds to be fitted to XRPD data
