library(shiny)
source('sheets_data.R')

# Define UI for application that plots features of movies 
ui <- fluidPage(
  fluidPage(
    fluidRow(
      column(3,
             actionButton(inputId = "reload", label = "Reload data"),
             textOutput('clics')),
      column(9,
             )
    ),
    fluidRow(
      
    )
  )
 
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  MRT <- reactiveValues()
  live <- reactiveValues()
  observeEvent(input$reload, {
    MRT$data <- gs_read( gs_key("1KdOJJ9rj3eWUKA4F6mX-MIEQKe5XJAMeJf4aZvQ1m8A"), ws = "Frozen Shiny Sheet")
    live$data <- MRT$data
  }, ignoreNULL=FALSE)
  output$clics <- renderText(input$reload)
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)