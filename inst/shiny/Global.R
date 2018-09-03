## Global.R
library(shiny)
library(powdR)
library(plotly)

data(minerals)
data(soils)
data(minerals_xrd)
data(minerals_phases)

xrd <- list("minerals" = minerals)
