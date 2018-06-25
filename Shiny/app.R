library(shiny)
library(ggplot2)
library(googleVis)
library(googlesheets)
library(DT)
library(shinythemes)
source('ui.R')
source('server.R')
load('gs_MRT3')

# Create a Shiny app object
shinyApp(ui = ui, server = server)
