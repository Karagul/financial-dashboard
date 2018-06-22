library(shiny)
library(ggplot2)
library(googleVis)
library(googlesheets)
library(DT)
library(shinythemes)
load('gs_MRT3')


# Define UI for application that plots features of movies 
ui <- fluidPage(theme = shinytheme("sandstone"),
   #top bar
   navbarPage("Invisible Technologies",
              tabPanel("Investors",
                  titlePanel("The Model", windowTitle = "TheMoodel3.0"), # App title
                  sidebarLayout(position = "right", # Sidebar layout with a input and output definitions
                    sidebarPanel(
                      sliderInput(inputId = "moSlider",
                                  label = "Number of months:",
                                  step = 1,
                                  min = 1,
                                  max = nrow(gs_MRT3),
                                  value = 10,
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
                                     tabPanel("Other", htmlOutput("gline"))
                        )
                     )
                  )
              ),
###########################Partners######################################################
              tabPanel("Partners",
                       titlePanel("The Model 3.0", windowTitle = "TheMoodel3.0"), # App title
                       sidebarLayout( # Sidebar layout with a input and output definitions
                         sidebarPanel(
                           sliderInput(inputId = "Month",
                                       label = "Number of months:",
                                       step = 1,
                                       min = 1,
                                       max = nrow(gs_MRT3),
                                       value = c(3,nrow(gs_MRT3) - 5),
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
                                  htmlOutput("table")#dataTableOutput(outputId = "data")
                         ),
                         tabPanel("demo",
                                  HTML('<iframe width="1000" height="600" src="https://www.youtube.com/embed/tX1gtG--OGE" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>')
                                  )
              )
     )
  )


# Define server function required to create the scatterplot
server <- function(input, output) {
  

  ###################################INVESTOR PAGE#############################################
  #####################################Profit Page##############################
  output$profit <- renderGvis({
    gross_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Net Profit`))
    df=data.frame(Month = gs_MRT3[input$moSlider,1], 
                  Gross_Profit = gross_profit_numeric[1:input$moSlider],
                  Net_Profits = net_profit_numeric[1:input$moSlider])
    gvisColumnChart(df, xvar = "Month", 
                   yvar = c("Gross_Profit", "Net_Profits"),
                   options=list(isStaked = TRUE, bar="{groupWidth:'100%'}", width = 800, height = 400))
  })
  
  ################################Runway page################################
  output$runway <- renderGvis({
    cash_in_bank_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Cash in Bank`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Net Profit`))
    funds_raised_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Cumulative Fundraising`))
    df=data.frame(Month= gs_MRT3[input$moSlider,1], 
                  Cash_in_Bank= cash_in_bank_numeric[1:input$moSlider],
                  Net_Profit = net_profit_numeric[1:input$moSlider],
                  Funds_Raised = funds_raised_numeric[1:input$moSlider])
    gvisLineChart(df, options=list(width = 800, height = 400))
  })
  
  ############################Labor Cost Page########################
  
  output$Lcost <- renderGvis({
    Agent_Labor_cost_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Agent Labor Costs`))
    operators_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Operators))
    RRR_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$RRRs))
    sentries_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Sentries))
    strategists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Strategists))
    specialists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Specialists))
    df=data.frame(Month = gs_MRT3[input$moSlider,1], 
                  Agent_Labor_Cost = Agent_Labor_cost_numeric[1:input$moSlider],
                  Operator_Labor_cost = operators_numeric[1:input$moSlider],
                  RRR_Labor_Cost = RRR_numeric[1:input$moSlider],
                  Sentry_Labor_Cost = sentries_numeric[1:input$moSlider],
                  Strategists_Labor_Cost = strategists_numeric[1:input$moSlider],
                  Specialists_Labor_Cost = specialists_numeric[1:input$moSlider]
                  
    )
    gvisComboChart(df, xvar="Month",
                            yvar=c("Operator_Labor_cost", "Agent_Labor_Cost", "RRR_Labor_Cost", 
                                   "Sentry_Labor_Cost", "Strategists_Labor_Cost", "Specialists_Labor_Cost"),
                            options=list(seriesType="line",
                                         series='{1: {type:"bars"}}', width = 800, height = 400))
  })

  ###########################Revenue Page############################
  output$gchart <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Revenue))
    df=data.frame(month= gs_MRT3[input$moSlider,1], 
                  Revenue= revenue_numeric[1:input$moSlider])
    gvisBarChart(df, options=list(width = 800, height = 400))
  })
  
  ##########Partner pay/gross revenue##################
  output$combo <- renderGvis({
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Partner Pay`))
    gross_profits_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Gross Profit`))
    df=data.frame(Month = gs_MRT3[input$moSlider,1], 
                  Partner_Pay = partner_pay_numeric[1:input$moSlider],
                  Gross_Profits = gross_profits_numeric[1:input$moSlider])
    gvisComboChart(df, xvar = "Month", 
                  yvar = c("Partner_Pay", "Gross_Profits"),
                  options=list(seriesType="bars",
                               series='{1: {type:"line"}}', width = 800, height = 400))
  })
  
#########other####################
  output$gline <- renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Revenue))
    df=data.frame(month= gs_MRT3[input$moSlider,1], 
                  Revenue= revenue_numeric[1:input$moSlider])
    gvisScatterChart(df)
  })

  
######################################PARTNER PAGE################################################
  output$Bar_graph3.0 <- renderPlot({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Revenue))
    ggplot(data = gs_MRT3, aes(x = Month, y = revenue_numeric)) +
      geom_bar(stat = "identity", color = "yellow")
  })
  
  output$line_graph3.0 <- renderPlot({
    Labor_Cost = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Labor Cost`))
    ggplot(data = gs_MRT3, aes(x = Month, y = Labor_Cost)) +
      geom_bar(stat = "identity", color = "red")
  })
  
#########################################MORE PAGE###############################################

  #old data frame way#
  #  output$data <- renderDataTable({
#    DT::datatable(data = gs_MRT,
#                  options = list(pageLength = 20, lengthMenu = c(10, 25, 40)), 
#                  rownames = FALSE)
#  })
  #######################################spreadsheet##############################################
    output$table <-renderGvis({
      gvisTable(gs_MRT3[,c(1:14, 22)])
    })
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
