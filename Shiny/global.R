library(shiny)
source("Server.R")
source("UI.R")
source("global.R")

data <- readRDS("healthexp.Rds")
data$Region <- as.factor(data$Region)

# Run the app ----
shinyApp(ui = ui, server = server)
