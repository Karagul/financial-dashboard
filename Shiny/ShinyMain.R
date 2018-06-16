library(shiny)
source("Server.R")
source("UI.R")

#What do we put here to load server and UI files?

# Run the app ----
shinyApp(ui = ui, server = server)