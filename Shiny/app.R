library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)
source('ui.R')
source('server.R')

# Create a Shiny app object
shinyApp(ui = ui, server = server)
