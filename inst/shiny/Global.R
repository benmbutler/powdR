## Global.R
library(shiny)
library(powdR)
library(plotly)
library(shinyWidgets)

data(minerals)
data(soils)
data(minerals_xrd)
data(minerals_phases)

xrd <- list("minerals" = minerals)
