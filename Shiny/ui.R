library(shiny)
library(ggplot2)
library(googleVis)
library(googlesheets)
library(DT)
library(shinythemes)
load('gs_MRT3')

mycss <- ".irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: #36454f;
border-color: #36454f;
} "

######date slider
# monthStart <- function(x) ({
#   x <- as.POSIXlt(x)
#   x$mday <- 1
#   as.Date(x)
# })

# Define UI for application that plots features of movies 
ui <- fluidPage(theme = shinytheme("sandstone"),
                #top bar
                navbarPage("Invisible Technologies",
                           tabPanel("Investors",
                                    titlePanel("The Model", windowTitle = "TheMoodel3.0"), # App title
                                    sidebarLayout(position = "right", # Sidebar layout with a input and output definitions
                                                  sidebarPanel(
                                                    tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar- edge, .js-irs-0 .irs-bar {
                                                                              background: #36454f;
                                                                              border-top: 1px solid #36454f ;
                                                                              border-bottom: 1px solid #36454f ;}
                                                                              .irs-from, .irs-to, .irs-single { background: #36454f }'
                                                    ))
                                                    ),
                                                    tags$style(mycss),
                                                    ###############date slider
                                                    # sliderInput(inputId = "moSlider",
                                                    #             label = "Month Range:",
                                                    #             min = as.Date("2018-05-01"),
                                                    #             max =as.Date("2019-11-01"),
                                                    #             value=c(as.Date("2018-12-01"), as.Date("2019-09-01")),
                                                    #             timeFormat="%b %Y",
                                                    #             animate = TRUE
                                                    #             ),
                                                    
                                                    sliderInput(inputId = "moSlider",
                                                                label = "Month Range:",
                                                                step = 1,
                                                                min = 1,
                                                                max = nrow(gs_MRT3),
                                                                value=c(3,16),
                                                                animate = TRUE
                                                    ),
                                                    
                                                    sliderInput(inputId = "mcost",
                                                                label = "Costs Multiplier:* (UNDER CONSTRUCTION)",
                                                                step = 1,
                                                                min = 1,
                                                                max = 10,
                                                                value = 5,
                                                                animate = TRUE
                                                    ),
                                                    
                                                    selectInput(inputId = "Growth Rate",
                                                                label = "Percentage Growth:",
                                                                choices = c("Flat", "Linear: 5k/mo",
                                                                            "Linear: 10k/mo","Linear: 20k/mo", 
                                                                            "Exponential: 5%/mo","Exponential: 15%/mo",
                                                                            "Exponential: 20%/mo","Exponential: 30%/mo")
                                                    ),
                                                    hr(),
                                                    
                                                    
                                                    # sliderInput(inputId = "vcost",
                                                    #             label = "Variable Costs:",
                                                    #             step = 1,
                                                    #             min = 1,
                                                    #             max = 10,
                                                    #             value = 5,
                                                    #             animate = TRUE
                                                    #             ),
                                                    # sliderInput(inputId = "Pbonus",
                                                    #             label = "Partner Bonuses:",
                                                    #             step = 1,
                                                    #             min = 1,
                                                    #             max = 10,
                                                    #             value = 5,
                                                    #             animate = TRUE
                                                    # ),
                                                    checkboxGroupInput(inputId = "Options",
                                                                       label = "Other Options:",
                                                                       choices = c("Include Agent Guarantee of 30 hrs/mo",
                                                                                   "Monty Carlo Simulation", 
                                                                                   "Include Varience in Projetions"),
                                                                       selected = c("Type")),
                                                    h6("* Cost multiplier refers to the percentage that Business Development Costs, 
                                                       Fixed Costs, Research and Development Costs, Labor Costs, 
                                                       and Sales/Marketing Costs increase at relative to revenue.")
                                                    ),
                                                  
                                                  # Outputs
                                                  mainPanel(
                                                    tabsetPanel(type = "tab",
                                                                tabPanel("Profit", htmlOutput("profit")),
                                                                tabPanel("Runway", htmlOutput("runway")),
                                                                tabPanel("Revenue", htmlOutput("gchart")),
                                                                tabPanel("Labor Cost", htmlOutput("Lcost")),
                                                                tabPanel("Partner Pay", htmlOutput("combo")),
                                                                tabPanel("Other", 
                                                                         fluidPage(
                                                                           fluidRow(
                                                                             column(6,htmlOutput("viz1"), htmlOutput("viz2"), htmlOutput("viz3")),
                                                                             column(6,htmlOutput("viz4"), htmlOutput("viz5"), htmlOutput("viz6"))
                                                                           )
                                                                         )
                                                                )
                                                    )
                                                  )
                                                    )
                                    ),
                           ###########################Partners######################################################
                           tabPanel("Partners",
                                    titlePanel("The Model 3.0", windowTitle = "TheMoodel3.0"), # App title
                                    sidebarLayout( # Sidebar layout with a input and output definitions
                                      sidebarPanel(
                                        sliderInput(inputId = "partMoSlide",
                                                    label = "Number of months:",
                                                    step = 1,
                                                    min = 1,
                                                    max = nrow(gs_MRT3),
                                                    value = c(3,7),
                                                    animate = TRUE
                                        ),
                                        hr(),
                                        
                                        selectInput(inputId = "Growth Rate",
                                                    label = "Percentage Growth:",
                                                    choices = c("Flat", "Linear: 5k/mo",
                                                                "Linear: 10k/mo","Linear 20k/mo", 
                                                                "Exponential: 5%/mo","Exponential: 15%/mo",
                                                                "Exponential: 20%/mo","Exponential: 30%/mo")
                                        ),
                                        hr(),
                                        checkboxGroupInput(inputId = "Options",
                                                           label = "Other Options:",
                                                           choices = c("Include Agent Guarantee of 30 hrs/mo",
                                                                       "Monty Carlo Simulation", 
                                                                       "Include Varience in Projetions"),
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
                                               htmlOutput("table")
                                      ),
                                      tabPanel("demo",
                                               HTML('<iframe width="1000" height="600" 
                                                    src="https://www.youtube.com/embed/tX1gtG--OGE" 
                                                    frameborder="0" allow="autoplay; encrypted-media" 
                                                    allowfullscreen></iframe>')
                                               )
                                               )
                                               )
                                      )

