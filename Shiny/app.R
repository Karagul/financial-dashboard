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
                                  choices = c("Exponential: 10%/Month", "Exponential: 20%/Month",
                                              "Exponential: 25%/Month","Exponential: 40%/Month", 
                                              "Exponential: 50%/Month","Exponential: 75%/Month",
                                              "Exponential: 100%/Month","Exponential: 150%/Month")
                                  ),
                      radioButtons(inputId = "Options",
                                   choices = c("Color","Year", "Type","Visualization"),
                                   label = "Other Options")
                    ),
                       # Outputs
                      mainPanel(
                         tabsetPanel(type = "tab",
                                     tabPanel("Profit", plotOutput(outputId = "Bar_graph")),
                                     tabPanel("Runway", plotOutput(outputId ="line_graph")),
                                     tabPanel("Revenue"),
                                     tabPanel("Labor Cost"),
                                     tabPanel("Partner Pay"),
                                     tabPanel("Other")
                        )
                     )
                  )
              ),
              tabPanel("Partners"),
              navbarMenu("More",
                         tabPanel("Spreadsheet",
                                  dataTableOutput(outputId = "data")
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
              options = list(pageLength = 20, lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })
  
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
