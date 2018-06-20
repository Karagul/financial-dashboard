library(shiny)
library(ggplot2)

# Define UI for application that plots features of movies 
ui <- fluidPage(theme = shinytheme("sandstone"),
                
           # App title
 titlePanel("Invisible Model 3.0", windowTitle = "TheMoodel3.0"),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    sidebarPanel(
    # Select variable for y-axis 
    selectInput(inputId = "y", 
                label = "Y-axis:",
                choices = c("Revenue"), 
                selected = "Revenue"),
    
    # Select variable for x-axis 
    selectInput(inputId = "x", 
                label = "X-axis:",
                choices = c("Month"), 
                selected = "Month"),
    sliderInput(inputId = "Month",
                label = "Number of months:",
                min = 1,
                max = nrow(gs_MRT),
                value = c(3,nrow(gs_MRT) - 5))

    ),
    
    # Outputs
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Scatterplot", plotOutput(outputId = "scatterplot")),
                  tabPanel("Data", dataTableOutput(outputId ="data")),
                  tabPanel("Histogram", plotOutput(outputId ="histogram"))
                  
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = gs_MRT, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  output$data <- renderDataTable({
    datatable(data = gs_MRT,
              options = list(pageLength = 10, lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })
  
  output$histogram <- renderPlot({
    x <- gs_MRT$Revenue
    x <- gsub(",", "", x)# remove comma
    x <- gsub("$", "", x)# remove dollar
    x <- as.numeric(x)
    hist(x, breaks = 20, col = "red")
  })
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
