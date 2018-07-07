library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)
library(DT)
curr_date <-format(Sys.Date(), "%b '%y")

#This is for the color of slide bar.
mycss <- ".irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: #36454f;
border-color: #36454f;
} "

# Define UI for application that plots features of movies 
ui <- fluidPage(theme = shinytheme("sandstone"),
         #top bar
         navbarPage("Invisible Technologies",
              tabPanel("General",  #general page
                 titlePanel(paste0("The Model: ",curr_date), windowTitle = "TheMoodel3.0"), # App title
                    sidebarLayout(position = "right", # Sidebar is on right side
                        sidebarPanel(
                              #this the code to change color of the slide bars(HTML)
                              tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar- edge, .js-irs-0 .irs-bar {
                                                      background: #36454f;
                                                      border-top: 1px solid #36454f ;
                                                      border-bottom: 1px solid #36454f ;}
                                                      .irs-from, .irs-to, .irs-single { background: #36454f }'
                                    ))
                                    ),
                                    tags$style(mycss),
                                    #############date slider
                                    # sliderInput(inputId = "moSlider_date",
                                    #             label = "Month Range:",
                                    #             min = as.Date("2018-05-01"),
                                    #             max =as.Date("2019-11-01"),
                                    #             value=c(as.Date("2018-12-01"), as.Date("2019-09-01")),
                                    #             timeFormat="%b %Y",
                                    #             animate = TRUE
                                    #             ),
                              
                                    #first slider bar for months
                                    sliderInput(inputId = "moSlider",
                                                label = "Month Range:",
                                                step = 1,
                                                min = 1,
                                                max = 72,
                                                value=c(5,24),
                                                animate = TRUE
                                    ),
                                    #slider for Client Growth
                                    sliderTextInput(
                                      inputId = "growslide",
                                      label = "Client Growth:(UNDER CONSTRUCTION)",
                                      grid = TRUE,
                                      choices = c("Custom","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                                  "55%","60%","65%","70%", "75%", "80%", "85%", "90%","95%", "100%"),
                                      selected = "Custom" 
                                    ),
                                    
                                    #slider for Cost Multiplier
                                    sliderTextInput(
                                      inputId = "mCost",
                                      label = "Costs Multiplier:(UNDER CONSTRUCTION)",
                                      grid = TRUE,
                                      choices = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                                                  "110%","125%","150%","200%"),
                                      selected = "30%" 
                                    ),
                                    hr(style="border-color: black;"),
                              
                                    #slider for Churn
                                    sliderTextInput(
                                      inputId = "churn",
                                      label = "Monthly Churn:(UNDER CONSTRUCTION)",
                                      grid = TRUE,
                                      choices = c("1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%","15%","16%",
                                                  "17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%","29%","30%",
                                                  "31%","32%","33%","34%","35%","36%","37%","38%","39%","40%"),
                                      selected = "29%" 
                                    ),
                              
                                    
                                    #slider for Cost Multiplier
                                    sliderTextInput(
                                      inputId = "Gross Margins",
                                      label = "Gross Margins:(UNDER CONSTRUCTION)",
                                      grid = TRUE,
                                      choices = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                                                  "110%","125%","150%","200%"),
                                      selected = "30%" 
                                    ),
                              
                                    sliderInput(inputId = "cacslide",
                                                label = "CAC Slider:(UNDER CONSTRUCTION)",
                                                step = 1,
                                                min = 1,
                                                max = 72,
                                                value=c(5,24),
                                                animate = TRUE
                                    ),
                                    
                                    sliderInput(inputId = "ltvslide",
                                                label = "LTV Slider:(UNDER CONSTRUCTION)",
                                                step = 1,
                                                min = 1,
                                                max = 36,
                                                value=c(4,12),
                                                animate = TRUE
                                    ),
                                    sliderInput(inputId = "partnerbonus",
                                                label = "Partner Bonuses:(UNDER CONSTRUCTION)",
                                                step = 1,
                                                min = 1,
                                                max = 36,
                                                value=c(4,12),
                                                animate = TRUE
                                    ),
                                    hr(),
                              
                                    checkboxGroupInput(inputId = "growthbox",
                                                       label = "Growth Options:(UNDER CONSTRUCTION)",
                                                       choices = c("Linear",
                                                                   "Exponential"),
                                                       selected = c("Type")),
                                    #check box for random options
                                    checkboxGroupInput(inputId = "Options",
                                                       label = "Other Options:(UNDER CONSTRUCTION)",
                                                       choices = c("Include Agent Guarantee of 30 hrs/mo",
                                                                   "Monte Carlo Simulation",
                                                                   "Include Varience in Projetions"),
                                                       selected = c("Type")),
                                    h6("* Cost multiplier refers to the percentage that Business Development Costs, 
                                       Subscription Costs, R&D Costs, 
                                       and Sales/Marketing Costs increase at relative to revenue."),
                                
                                    #reload data clicker
                                    actionButton(inputId = "reload", label = "Reload data"),
                                    textOutput('clics')
                                    ),
                                  
                                  
                                  # main panel outputs
                                  mainPanel(
                                    tabsetPanel(type = "tab",
                                                tabPanel("Profit", htmlOutput("profit"), htmlOutput("gross")),
                                                tabPanel("Runway", htmlOutput("runway"), htmlOutput("profRev")),
                                                tabPanel("Growth",htmlOutput("num_client"), htmlOutput("rev_client")),      
                                                tabPanel("Churn", htmlOutput("client")),
                                                tabPanel("Workforce", htmlOutput("Lcost"), htmlOutput("workForce")),
                                                tabPanel("Partner Pay", htmlOutput("combo"), htmlOutput("linechart")),
                                                tabPanel("Overhead", htmlOutput("overhead"), htmlOutput("econScale")),
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
                    ),
                 #this is the table at the bottom of the investor page. 
                 fluidRow(
                   column(12,
                          htmlOutput("main_table")
                   )
                 )
            ),
##################################################Partner page########################################################################

              tabPanel("Costs",
                    titlePanel("The Model", windowTitle = "TheMoodel3.0"), # App title
                          sidebarLayout(position = "right", # Sidebar is set to right side
                            sidebarPanel(
                              #this the code to change color of the slide bars
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
                              
                              #first slider bar for months
                              sliderInput(inputId = "part_moSlider",
                                          label = "Month Range:",
                                          step = 1,
                                          min = 1,
                                          max = 72,
                                          value=c(5,24),
                                          animate = TRUE
                              ),
                              #slider for Client Growth
                              sliderTextInput(
                                inputId = "part_growslide",
                                label = "Client Growth:(UNDER CONSTRUCTION)",
                                grid = TRUE,
                                choices = c("5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                            "55%","60%","65%","70%", "75%", "80%", "85%", "90%","95%", "100%"),
                                selected = "50%" 
                              ),
                              
                              #slider for Cost Multiplier
                              sliderTextInput(
                                inputId = "part_mCost",
                                label = "Costs Multiplier:(UNDER CONSTRUCTION)",
                                grid = TRUE,
                                choices = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                                            "110%","125%","150%","200%"),
                                selected = "30%" 
                              ),
                              #slider for Churn
                              sliderTextInput(
                                inputId = "part_churn",
                                label = "Monthly Churn:(UNDER CONSTRUCTION)",
                                grid = TRUE,
                                choices = c("1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%","15%","16%",
                                            "17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%","29%","30%",
                                            "31%","32%","33%","34%","35%","36%","37%","38%","39%","40%"),
                                selected = "29%" 
                              ),
                              
                              #slider for Cost Multiplier
                              sliderTextInput(
                                inputId = "part_gross_margins",
                                label = "Gross Margins:(UNDER CONSTRUCTION)",
                                grid = TRUE,
                                choices = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                                            "110%","125%","150%","200%"),
                                selected = "30%" 
                              ),
                              
                              sliderInput(inputId = "part_cacslide",
                                          label = "CAC Slider:(UNDER CONSTRUCTION)",
                                          step = 1,
                                          min = 1,
                                          max = 36,
                                          value=c(4,12),
                                          animate = TRUE
                              ),
                              
                              sliderInput(inputId = "part_ltvslide",
                                          label = "LTV Slider:(UNDER CONSTRUCTION)",
                                          step = 1,
                                          min = 1,
                                          max = 36,
                                          value=c(4,12),
                                          animate = TRUE
                              ),
                              sliderInput(inputId = "part_partnerbonus",
                                          label = "Partner Bonuses:(UNDER CONSTRUCTION)",
                                          step = 1,
                                          min = 1,
                                          max = 36,
                                          value=c(4,12),
                                          animate = TRUE
                              ),
                              hr(),
                              
                              checkboxGroupInput(inputId = "part_growthbox",
                                                 label = "Growth Options:(UNDER CONSTRUCTION)",
                                                 choices = c("Linear",
                                                             "Exponential"),
                                                 selected = c("Type")),
                              #check box for random options
                              checkboxGroupInput(inputId = "part_Options",
                                                 label = "Other Options:(UNDER CONSTRUCTION)",
                                                 choices = c("Include Agent Guarantee of 30 hrs/mo",
                                                             "Monte Carlo Simulation",
                                                             "Include Varience in Projetions"),
                                                 selected = c("Type")),
                              h6("* Cost multiplier refers to the percentage that Business Development Costs, 
                                 Subscription Costs, R&D Costs, 
                                 and Sales/Marketing Costs increase at relative to revenue."),
                              
                              #reload data clicker
                              actionButton(inputId = "part_reload", label = "Reload data"),
                              textOutput('part_clics')
                              ),
                            
                            
                            # main panel outputs for partners
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Profit", htmlOutput("part_profit"), htmlOutput("part_gross")),
                                          tabPanel("Runway", htmlOutput("part_runway"), htmlOutput("part_profRev")),
                                          tabPanel("Growth",htmlOutput("part_num_client"), htmlOutput("part_rev_client")),      
                                          tabPanel("Churn", htmlOutput("part_client")),
                                          tabPanel("Workforce", htmlOutput("part_Lcost"), htmlOutput("part_workForce")),
                                          tabPanel("Partner Pay", htmlOutput("part_combo"),htmlOutput("part_linechart")),
                                          tabPanel("Overhead", htmlOutput("part_overhead"), htmlOutput("part_econScale")),
                                          tabPanel("Other", 
                                                   fluidPage(
                                                     fluidRow(
                                                       column(6,htmlOutput("part_viz1"), htmlOutput("part_viz2"), 
                                                              htmlOutput("part_viz3")),
                                                       column(6,htmlOutput("part_viz4"), htmlOutput("part_viz5"), 
                                                              htmlOutput("part_viz6"))
                                                     )
                                                   )
                                          )
                                          )
                                        )
                                  ),
                    #bottom of the partner page data table
                    fluidRow(
                      column(12,
                             htmlOutput("part_main_table")
                      )
                    )
           ),

            # more page that had spreadsheet and demo dropdowns
           navbarMenu("More",
                      tabPanel("Further Customization",
                               dataTableOutput('table')
                      ),
                      #youtube video in demo dropdown 
                      tabPanel("demo",
                               HTML('<iframe width="1000" height="600" 
                                    src="https://www.youtube.com/embed/tX1gtG--OGE" 
                                    frameborder="0" allow="autoplay; encrypted-media" 
                                    allowfullscreen></iframe>')
                               )
                    )
           )

      )

