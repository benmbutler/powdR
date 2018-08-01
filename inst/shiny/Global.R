## Global.R
library(shiny)
library(powdRcran)
library(plotly)

data(minerals)
data(minerals_xrd)
data(minerals_phases)

xrd <- list("minerals" = minerals)
