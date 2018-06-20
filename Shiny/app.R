library(shiny)
library(ggplot2)
library(googleVis)
library(googlesheets)


# Define UI for application that plots features of movies 
ui <- fluidPage(theme = shinytheme("sandstone"),
                
 #top bar
   navbarPage("Invisible",
              tabPanel("Investors",
                  titlePanel("Invisible Model 3.0", windowTitle = "TheMoodel3.0"), # App title
                  sidebarLayout( # Sidebar layout with a input and output definitions
                    sidebarPanel(
                      sliderInput(inputId = "Month",
                                  label = "Number of months:",
                                  min = 1,
                                  max = nrow(gs_MRT),
                                  value = c(3,nrow(gs_MRT) - 5)
                                  ),
                      
                      selectInput(inputId = "Growth Rate",
                                  label = "Percentage Growth:",
                                  choices = c("blah", "dfsg")
                                  )
                    ),
                       # Outputs
                      mainPanel(
                         tabsetPanel(type = "tab",
                                     tabPanel("Revenue over time", plotOutput(outputId = "Bar_graph")),
                                     tabPanel("Labor Cost over time", plotOutput(outputId ="line_graph"))
                                      
                          
                        )
                     )
                  )
              ),
              tabPanel("Partners"),
              navbarMenu("More",
                         tabPanel("Spreadsheet",
                                  shiny::dataTableOutput(outputId = "data")
                         ),
                         tabPanel("About")
              )
     )
  )


# Define server function required to create the scatterplot
server <- function(input, output) {
  

  
  # Create scatterplot object the plotOutput function is expecting
  output$Bar_graph <- renderPlot({
    ggplot(data = gs_MRT, aes(x = Month, y = Revenue)) +
      geom_bar(stat = "identity", colour = "yellow")
  })
  
  output$line_graph <- renderPlot({
    ggplot(data = gs_MRT, aes(x = Month, y = Comissions)) +
      geom_bar(stat = "identity")
  })
  
  output$data <- renderDataTable({
    datatable(data = gs_MRT,
              options = list(pageLength = 10, lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })
  
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
