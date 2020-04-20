#' @importFrom shinyWidgets dropdownButton
#' @importFrom DT dataTableOutput
.sns <- function(x) {

sw <- dropdownButton()

dt <- dataTableOutput(outputId = x)

return = list(sw, dt)

}


