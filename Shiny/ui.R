library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)
curr_date <-format(Sys.Date(), "%b '%y")

#This is for the color of slide bar.
mycss <- ".irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: #36454f;
border-color: #36454f;
} "

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
    #top bar
    navbarPage(a("Invisible Technologies", href="https://inv.tech", target="_blank", color = "White"),

     
     #############################################Growth page###########################################################################  
     tabPanel("Growth",  #Growth page
              titlePanel(paste0("The Model"), windowTitle = "TheMoodel3.0"), # App title
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
                              
                              h3("Current Date: ", curr_date),
                              #slider bar for months
                              sliderInput(inputId = "grow_moSlider",
                                          label = "Month Range:",
                                          step = 1,
                                          min = 1,
                                          max = 72,
                                          value=c(9,27),
                                          animate = TRUE
                              ),
                              #slider for Client Growth
                              sliderTextInput(
                                inputId = "grow_growslide",
                                label = "Client Growth:",
                                grid = TRUE,
                                choices = c("Custom","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                            "55%","60%","65%","70%", "75%", "80%", "85%", "90%","95%", "100%"),
                                selected = "Custom" 
                              ),
                              
                              #slider for Cost Multiplier
                              sliderTextInput(
                                inputId = "grow_mCost",
                                label = "Costs Multiplier:",
                                grid = TRUE,
                                choices = c("Custom","10%", "20%","25%","30%","40%","50%","60%","70%","80%","90%","100%",
                                            "110%","125%","150%","200%"),
                                selected = "Custom" 
                              ),
                              #hr(style="border-color: black;"),
                              
                              #slider for Churn
                              sliderTextInput(
                                inputId = "grow_churn",
                                label = "Monthly Churn:",
                                grid = TRUE,
                                choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                            "15%","16%","17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%",
                                            "29%","30%","31%","32%","33%","34%","35%","36%","37%","38%","39%","40%"),
                                selected = "Custom" 
                              ),
                              sliderTextInput(
                                inputId = "grow_Partners",
                                label = "Partners added per month:",
                                grid = TRUE,
                                choices = c("Custom","1","2","3","4","5","6","7","8","9","10"),
                                selected = "Custom"
                              ),
                              radioButtons(
                                inputId = "grow_partbox",
                                label = "Choose one for Partner Growth:",
                                choices = c("Linear", "Exponential"),
                                inline = TRUE,
                                selected = "Linear"
                              ),
                              sliderTextInput(
                                inputId = "grow_auto",
                                label = "Automation Multiplier",
                                grid = TRUE,
                                choices = c("Custom", "0%","0.25%","0.5%","0.75%","1%","1.25%","1.5%","1.75%","2%","2.25%",
                                            "2.5%","2.75%","3%","3.25%","3.5%","3.75%","4%"),
                                selected = "Custom"
                              ),
                              h6("* Costs multiplier refers to the percentage that Business Development Costs, 
                                 Subscription Costs, R&D Costs, 
                                 and Sales/Marketing Costs increase at relative to revenue."),
                              
                              #reload data clicker
                              fluidRow(
                                column(6,
                                       actionButton(inputId = "grow_reload", label = "Reload data"),
                                       textOutput('grow_clics')
                                ),
                                column(6,
                                       actionButton(inputId = "grow_newTable", label = "Show Sheet")
                                )
                              )
                            ),
                            
                            # main panel outputs
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Profit", htmlOutput("grow_profit"), htmlOutput("grow_gross")),
                                          tabPanel("Runway", htmlOutput("grow_runway")),
                                          tabPanel("Cohort Analysis",htmlOutput("grow_num_client"), htmlOutput("grow_rev_client")),
                                          tabPanel("More Cohort Analysis", htmlOutput("grow_bubble"))
                                          
                              )
                            )
                            ),
              #this is the table at the bottom of the investor page. 
              fluidRow(
                column(12,
                       htmlOutput("grow_main_table")
                )
              )
     ),
     
     
     ##################################################Costs page########################################################################
     
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
                              h3("Current Date: ", curr_date),
                              #first slider bar for months
                              sliderInput(inputId = "part_moSlider",
                                          label = "Month Range:",
                                          step = 1,
                                          min = 1,
                                          max = 72,
                                          value=c(9,27),
                                          animate = TRUE
                              ),
                              #slider for Client Growth
                              sliderTextInput(
                                inputId = "part_growslide",
                                label = "Client Growth:",
                                grid = TRUE,
                                choices = c("Custom","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                            "55%","60%","65%","70%", "75%", "80%", "85%", "90%","95%", "100%"),
                                selected = "Custom" 
                              ),
                              
                              #slider for Cost Multiplier
                              sliderTextInput(
                                inputId = "part_mCost",
                                label = "Costs Multiplier:",
                                grid = TRUE,
                                choices = c("Custom","10%", "20%","25%","30%","40%","50%","60%","70%","80%","90%","100%",
                                            "110%","125%","150%","200%"),
                                selected = "Custom" 
                              ),
                              #slider for Churn
                              sliderTextInput(
                                inputId = "part_churn",
                                label = "Monthly Churn:",
                                grid = TRUE,
                                choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                            "15%","16%","17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%",
                                            "29%","30%","31%","32%","33%","34%","35%","36%","37%","38%","39%","40%"),
                                selected = "Custom" 
                              ),
                              sliderTextInput(
                                inputId = "part_Partners",
                                label = "Partners added per month:",
                                grid = TRUE,
                                choices = c("Custom","1","2","3","4","5","6","7","8","9","10"),
                                selected = "Custom"
                              ),
                              radioButtons(
                                inputId = "part_partbox",
                                label = "Choose one for Partner Growth:",
                                choices = c("Linear", "Exponential"),
                                inline = TRUE,
                                selected = "Linear"
                              ),
                              
                              sliderTextInput(
                                inputId = "part_auto",
                                label = "Automation Multiplier",
                                grid = TRUE,
                                choices = c("Custom", "0%","0.25%","0.5%","0.75%","1%","1.25%","1.5%","1.75%","2%","2.25%",
                                            "2.5%","2.75%","3%","3.25%","3.5%","3.75%","4%"),
                                selected = "Custom"
                              ),
                              h6("* Costs multiplier refers to the percentage that Business Development Costs, 
                                 Subscription Costs, R&D Costs, 
                                 and Sales/Marketing Costs increase at relative to revenue."),
                              
                              #reload data clicker
                              fluidRow(
                                column(6,
                                       actionButton(inputId = "part_reload", label = "Reload data"),
                                       textOutput('part_clics')
                                ),
                                column(6,
                                       actionButton(inputId = "part_newTable", label = "Show Sheet")
                                )
                              )
                            ),
                            
                            
                            # main panel outputs for partners
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Labor Costs", htmlOutput("part_Lcost"), htmlOutput("part_rev_vs_lab")),
                                          tabPanel("Partner Pay", htmlOutput("part_combo"),htmlOutput("part_linechart")),
                                          tabPanel("Overhead", htmlOutput("part_overhead"), htmlOutput("part_econScale"))
                                          
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
     
     # more page with visualizations and table
     navbarMenu("More",
                tabPanel("Visualizations",  #general page
                         titlePanel(paste0("The Model"), windowTitle = "TheMoodel3.0"), # App title
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
                                         
                                         h3("Current Date: ", curr_date),
                                         #first slider bar for months
                                         sliderInput(inputId = "moSlider",
                                                     label = "Month Range:",
                                                     step = 1,
                                                     min = 1,
                                                     max = 72,
                                                     value=c(9,27),
                                                     animate = TRUE
                                         ),
                                         #slider for Client Growth
                                         sliderTextInput(
                                           inputId = "growslide",
                                           label = "Client Growth:",
                                           grid = TRUE,
                                           choices = c("Custom","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                                       "55%","60%","65%","70%", "75%", "80%", "85%", "90%","95%", "100%"),
                                           selected = "Custom" 
                                         ),
                                         
                                         #slider for Cost Multiplier
                                         sliderTextInput(
                                           inputId = "mCost",
                                           label = "Costs Multiplier:",
                                           grid = TRUE,
                                           choices = c("Custom","10%", "20%","25%","30%","40%","50%","60%","70%","80%","90%","100%",
                                                       "110%","125%","150%","200%"),
                                           selected = "Custom" 
                                         ),
                                         #hr(style="border-color: black;"),
                                         
                                         #slider for Churn
                                         sliderTextInput(
                                           inputId = "churn",
                                           label = "Monthly Churn:",
                                           grid = TRUE,
                                           choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                                       "15%","16%","17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%",
                                                       "28%","29%","30%","31%","32%","33%","34%","35%","36%","37%","38%","39%","40%"),
                                           selected = "Custom" 
                                         ),
                                         sliderTextInput(
                                           inputId = "Partners",
                                           label = "Partners added per month:",
                                           grid = TRUE,
                                           choices = c("Custom","1","2","3","4","5","6","7","8","9","10"),
                                           selected = "Custom"
                                         ),
                                         radioButtons(
                                           inputId = "partbox",
                                           label = "Choose one for Partner Growth:",
                                           choices = c("Linear", "Exponential"),
                                           inline = TRUE,
                                           selected = "Linear"
                                         ),
                                         
                                         sliderTextInput(
                                           inputId = "auto",
                                           label = "Automation Multiplier",
                                           grid = TRUE,
                                           choices = c("Custom", "0%","0.25%","0.5%","0.75%","1%","1.25%","1.5%","1.75%","2%","2.25%",
                                                       "2.5%","2.75%","3%","3.25%","3.5%","3.75%","4%"),
                                           selected = "Custom"
                                         ),
                                         h6("* Costs multiplier refers to the percentage that Business Development Costs, 
                                            Subscription Costs, R&D Costs, 
                                            and Sales/Marketing Costs increase at relative to revenue."),
                                         
                                         #reload data clicker
                                         fluidRow(
                                           column(6,
                                                  actionButton(inputId = "reload", label = "Reload data"),
                                                  textOutput('clics')
                                           ),
                                           column(6,
                                                  actionButton(inputId = "newTable", label = "Show Sheet")
                                           )
                                         )
                                         ),
                                       
                                       
                                       # main panel outputs
                                       mainPanel(
                                         tabsetPanel(type = "tab",
                                                     tabPanel("Profit", htmlOutput("profRev"), htmlOutput("cltvcac")), 
                                                     tabPanel("Revenue per Head", htmlOutput("workForce")),
                                                     tabPanel("Churn", htmlOutput("client"))
                                                     
                                         )
                                       )
                                       ),
                         #this is the table at the bottom of the General page. 
                         fluidRow(
                           column(12,
                                  htmlOutput("main_table")
                           )
                         )
                ),
                ##Functional Mapping in more
                tabPanel("Functional Mapping",
                        htmlOutput('funcmap')
                        ),
                
                #Table dropdown in More 
                tabPanel("Table",
                         htmlOutput('table')
                         )
                         ),
     tabPanel("Demo",
              HTML('<iframe width="1000" height="600" 
                              src="https://www.youtube.com/embed/tX1gtG--OGE" 
                   frameborder="0" allow="autoplay; encrypted-media" 
                   allowfullscreen></iframe>')
              )
                         )

                )


