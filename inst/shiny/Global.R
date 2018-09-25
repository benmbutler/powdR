## Global.R
library(shiny)
library(powdR)
library(plotly)
library(shinyWidgets)
library(baseline)

data(minerals)
data(soils)
data(minerals_xrd)
data(minerals_phases)

xrd <- list("minerals" = minerals)
