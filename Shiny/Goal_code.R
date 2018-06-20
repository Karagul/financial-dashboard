library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(googlesheets)
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))
movies_codebook <- read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies_codebook.csv")

# Define UI for application that plots features of movies
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # App title
                titlePanel("Movie browser, 1970 - 2014", windowTitle = "Movies"),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    h3("Plotting"),      # Third level header: Plotting
                    
                    # Select variable for y-axis 
                    selectInput(inputId = "y", 
                                label = "Y-axis:",
                                choices = c("IMDB rating" = "imdb_rating", 
                                            "IMDB number of votes" = "imdb_num_votes", 
                                            "Critics Score" = "critics_score", 
                                            "Audience Score" = "audience_score", 
                                            "Runtime" = "runtime"), 
                                selected = "audience_score"),
                    
                    # Select variable for x-axis 
                    selectInput(inputId = "x", 
                                label = "X-axis:",
                                choices = c("IMDB rating" = "imdb_rating", 
                                            "IMDB number of votes" = "imdb_num_votes", 
                                            "Critics Score" = "critics_score", 
                                            "Audience Score" = "audience_score", 
                                            "Runtime" = "runtime"), 
                                selected = "critics_score"),
                    
                    # Enter text for plot title
                    textInput(inputId = "plot_title", 
                              label = "Plot title", 
                              placeholder = "Enter text to be used as plot title"),
                    
                    hr(),
                    
                    h3("Subsetting"),    # Third level header: Subsetting
                    
                    # Select which types of movies to plot
                    checkboxGroupInput(inputId = "selected_type",
                                       label = "Select movie type(s):",
                                       choices = c("Documentary", "Feature Film", "TV Movie"),
                                       selected = "Feature Film"),
                    
                    hr(),
                    
                    # Show data table
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table",
                                  value = TRUE),
                    
                    # Built with Shiny by RStudio
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                       ".")
                    
                  ),
                  
                  # Output:
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                id = "tabsetpanel",
                                tabPanel(title = "Plot", 
                                         plotOutput(outputId = "scatterplot"),
                                         br(),
                                         h5(textOutput("description"))),
                                tabPanel(title = "Data", 
                                         br(),
                                         DT::dataTableOutput(outputId = "moviestable")),
                                # New tab panel for Codebook
                                tabPanel("Codebook", 
                                         br(),
                                         dataTableOutput(outputId = "codebook"))
                    )
                  )
                )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  movies_selected <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(movies, title_type %in% input$selected_type)
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplot(data = movies_selected(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(x = x(),
           y = y(),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = toTitleCase(input$plot_title))
  })
  
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          x(),
          "and",
          y(),
          "for",
          nrow(movies_selected()),
          "movies.")
  })
  
  # Print data table if checked
  output$moviestable <- DT::renderDataTable(
    DT::datatable(data = movies_selected()[, 1:6], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
    } else {
      hideTab(inputId = "tabsetpanel", target = "Data")
    }
  })
  
  # Render data table for codebook
  output$codebook <- renderDataTable({
    datatable(data = movies_codebook,
              options = list(pageLength = 10, lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
      