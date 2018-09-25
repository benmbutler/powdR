# powdR 0.1.0.9000

*   `powdR` now suggests the `nnls` (>=1.4) package in the DESCRIPTION.

*   `fps` now has accepts "nnls" in the `solver` argument. If "nnls" is selected,
    the algorithm uses non negative least squares instaed of minimising an objective
    function.
    
*   The shiny application behind `run_powdR` has been updated to accept "nnls"
