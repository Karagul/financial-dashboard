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
                                         choices = c("Inlcude Agent Guarantee of 30 hrs/mo",
                                                     "Monty Carlo Simulation", 
                                                     "Inlcude Varience in Projetions"),
                                        selected = c("Type"))
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
                                  htmlOutput("table")#dataTableOutput(outputId = "data")
                         ),
                         tabPanel("demo",
                                  mainPanel(uiOutput("video"))
                                  )
              )
     )
  )


# Define server function required to create the scatterplot
server <- function(input, output) {
  

  ###################################INVESTOR PAGE#############################################
  #####################################Profit Page##############################
  output$profit <- renderGvis({
    gross_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Net Profit`))
    df=data.frame(Month = gs_MRT[,1], 
                  Gross_Profit = gross_profit_numeric[1:nrow(gs_MRT)],
                  Net_Profits = net_profit_numeric[1:nrow(gs_MRT)])
    gvisSteppedAreaChart(df, xvar = "Month", 
                   yvar = c("Gross_Profit", "Net_Profits"),
                   options=list(isStaked = TRUE, width = 800, height = 400))
  })
  
  ################################Runway page################################
  output$runway <- renderGvis({
    cash_in_bank_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Cash in bank at end of month`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Net Profit`))
    funds_raised_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Fundraising/external cash injection`))
    df=data.frame(Month= gs_MRT[,1], 
                  Cash_in_Bank= cash_in_bank_numeric[1:nrow(gs_MRT)],
                  Net_Profit = net_profit_numeric[1:nrow(gs_MRT)],
                  Funds_Raised = funds_raised_numeric[1:nrow(gs_MRT)])
    gvisLineChart(df, options=list(width = 800, height = 400))
  })
  
  ############################Labor Cost Page########################
  
  output$Lcost <- renderGvis({
    operators_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Operators))
    RRR_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$RRRs))
    df=data.frame(Month = gs_MRT[,1], 
                  Operator_Labor_cost = operators_numeric[1:nrow(gs_MRT)],
                  RRR_Labor_Cost = RRR_numeric[1:nrow(gs_MRT)]
    )
    gvisComboChart(df, xvar="Month",
                            yvar=c("Operator_Labor_cost", "RRR_Labor_Cost"),
                            options=list(seriesType="bars",
                                         series='{1: {type:"line"}}'))
  })

  ###########################Revenue Page############################
  output$gchart <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Revenue))
    df=data.frame(month= gs_MRT[,1], 
                  Revenue= revenue_numeric[1:nrow(gs_MRT)])
    gvisBarChart(df, options=list(width = 800, height = 400))
  })
  
  ##########Partner pay/gross revenue##################
  output$combo <- renderGvis({
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Partner Pay`))
    gross_profits_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Gross Profit`))
    df=data.frame(Month = gs_MRT[,1], 
                  Partner_Pay = partner_pay_numeric[1:nrow(gs_MRT)],
                  Gross_Profits = gross_profits_numeric[1:nrow(gs_MRT)])
    gvisComboChart(df, xvar = "Month", 
                  yvar = c("Partner_Pay", "Gross_Profits"),
                  options=list(seriesType="bars",
                               series='{1: {type:"line"}}', width = 800, height = 400))
  })
  
#########other####################
  output$gline <- renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Revenue))
    df=data.frame(month= gs_MRT[,1], 
                  Revenue= revenue_numeric[1:nrow(gs_MRT)])
    gvisLineChart(df)
  })

  
######################################PARTNER PAGE################################################
  output$Bar_graph3.0 <- renderPlot({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Revenue))
    ggplot(data = gs_MRT, aes(x = Month, y = revenue_numeric)) +
      geom_bar(stat = "identity", color = "yellow")
  })
  
  output$line_graph3.0 <- renderPlot({
    Labor_Cost = as.numeric(gsub("[\\$,]", "", gs_MRT$`Labor Cost`))
    ggplot(data = gs_MRT, aes(x = Month, y = Labor_Cost)) +
      geom_bar(stat = "identity", color = "red")
  })
  
#########################################MORE PAGE###############################################

  #old data frame way#
  #  output$data <- renderDataTable({
#    DT::datatable(data = gs_MRT,
#                  options = list(pageLength = 20, lengthMenu = c(10, 25, 40)), 
#                  rownames = FALSE)
#  })
    output$table <-renderGvis({
      gvisTable(gs_MRT)
    })
  
  #####################demo##########################
  output$video <- renderUI({
    tags$video(src = "https://www.youtube.com/watch?v=tX1gtG--OGE", type = "video/mp4", autoplay = NA, controls = NA)
  })
  
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
