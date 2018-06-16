library(shiny)
source("Server.R")
source("UI.R")
source("global.R")

# Run the app ----
shinyApp(ui = ui, server = server)