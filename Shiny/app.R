library(shiny)
library(ggplot2)
library(googleVis)
library(googlesheets)
library(DT)
library(shinythemes)
load('gs_MRT')


# Define UI for application that plots features of movies 
ui <- fluidPage(theme = shinytheme("sandstone"),
                
 #top bar
   navbarPage("Invisible Technologies",
              tabPanel("Investors",
                  titlePanel("The Model", windowTitle = "TheMoodel3.0"), # App title
                  sidebarLayout( # Sidebar layout with a input and output definitions
                    sidebarPanel(
                      sliderInput(inputId = "Month",
                                  label = "Number of months:",
                                  step = 1,
                                  min = 1,
                                  max = nrow(gs_MRT),
                                  value = c(3,nrow(gs_MRT) - 5),
                                  animate = TRUE
                                  ),
                      
                      selectInput(inputId = "Growth Rate",
                                  label = "Percentage Growth:",
                                  choices = c("Flat", "Linear: 5k/mo",
                                              "Linear: 10k/mo","Linear 20k/mo", 
                                              "Exponential: 5%/mo","Exponential: 15%/mo",
                                              "Exponential: 20%/mo","Exponential: 30%/mo")
                                  ),
                      checkboxGroupInput(inputId = "Options",
                                         label = "Other Options:",
                                         choices = c("Inlcude Agent Guarantee of 30 hrs/mo",
                                                     "Monty Carlo Simulation", 
                                                     "Inlcude Varience in Projetions"),
                                        selected = c("Type"))
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
              tabPanel("Partners",
                       titlePanel("The Model 3.0", windowTitle = "TheMoodel3.0"), # App title
                       sidebarLayout( # Sidebar layout with a input and output definitions
                         sidebarPanel(
                           sliderInput(inputId = "Month",
                                       label = "Number of months:",
                                       step = 1,
                                       min = 1,
                                       max = nrow(gs_MRT),
                                       value = c(3,nrow(gs_MRT) - 5),
                                       animate = TRUE
                           ),
                           
                           selectInput(inputId = "Growth Rate",
                                       label = "Percentage Growth:",
                                       choices = c("Flat", "Linear: 5k/mo",
                                                   "Linear: 10k/mo","Linear 20k/mo", 
                                                   "Exponential: 5%/mo","Exponential: 15%/mo",
                                                   "Exponential: 20%/mo","Exponential: 30%/mo")
                           ),
                           checkboxGroupInput(inputId = "Options",
                                              label = "Other Options:",
                                              choices = c("Inlcude Agent Guarantee of 30 hrs/mo",
                                                          "Monty Carlo Simulation", 
                                                          "Inlcude Varience in Projetions"),
                                              selected = c("Type"))
                         ),
                         # Outputs
                         mainPanel(
                           tabsetPanel(type = "tab",
                                       tabPanel("Profit", plotOutput(outputId = "Bar_graph3.0")),
                                       tabPanel("Runway", plotOutput(outputId ="line_graph3.0")),
                                       tabPanel("Revenue"),
                                       tabPanel("Labor Cost"),
                                       tabPanel("Partner Pay"),
                                       tabPanel("Other")
                           )
                         )
                       )
              ),
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
  

  ###################################INVESTOR PAGE#############################################
  # Create scatterplot object the plotOutput function is expecting
  output$Bar_graph <- renderPlot({
    ggplot(data = gs_MRT, aes(x = Comissions, y = Revenue)) +
      geom_bar(stat = "identity")
  })
  
  output$line_graph <- renderPlot({
    ggplot(data = gs_MRT, aes(x = Month, y = Comissions)) +
      geom_bar(stat = "identity")
  })
  

  
######################################PARTNER PAGE################################################
  output$Bar_graph3.0 <- renderPlot({
    ggplot(data = gs_MRT, aes(x = Comissions, y = Revenue)) +
      geom_bar(stat = "identity", color = "yellow")
  })
  
  output$line_graph3.0 <- renderPlot({
    ggplot(data = gs_MRT, aes(x = Month, y = Comissions)) +
      geom_bar(stat = "identity", color = "red")
  })
  
#########################################MORE PAGE###############################################
  output$data <- renderDataTable({
    DT::datatable(data = gs_MRT,
                  options = list(pageLength = 20, lengthMenu = c(10, 25, 40)), 
                  rownames = FALSE)
  })
  
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
