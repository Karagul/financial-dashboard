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

# Define UI for application Cosmo
ui <- fluidPage(theme = shinytheme("cosmo"),
                
    #top bar
    navbarPage(a(img(src="InvLogo.png", height = 30, width = 35, style="float:left"),
                 "Invisible Technologies", href="https://inv.tech", target="_blank", color = "White"), windowTitle = "The Model",
                  
     
     #############################################Growth page###########################################################################  
     tabPanel("Growth",  #Growth page
              titlePanel(paste0("The Model"), windowTitle = "The Model"), # App title
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
                              # fluidRow(
                              #   column(4,
                              #          submitButton("Conservative", width = '100%')
                              #   ),
                              #   column(4,
                              #          submitButton("Moderate", width = '100%')
                              #   ),
                              #   column(4,
                              #          submitButton("Aggressive", width = '100%')
                              #   )
                              # ),
                              # submitButton("Conservative", width = '100%'),
                              # submitButton("Moderate", width = '100%'),
                              # submitButton("Aggressive", width = '100%'),
                              
                              radioButtons(inputId = "grow_diff_types",
                                           label = "Preselected Model:",
                                           choices = c("Conservative","Moderate","Aggressive"),
                                           inline = TRUE,
                                           selected = "None"),
                              
                              fluidRow(
                                column(6,
                                       fluidRow(
                                         column(4,
                                                h6(strong("Start:"))
                                         ),
                                         
                                         column(6,
                                                h6(textOutput(outputId = "grow_montext"))
                                         ),
                                         column(4,
                                                h6(""))
                                       )
                                ),
                                column(6,
                                       fluidRow(
                                         column(3,
                                                h6(strong("End:"))
                                         ),
                                         column(6,
                                                h6(textOutput(outputId = "grow_montext2"))
                                         ),
                                         column(1,
                                                h6(""))
                                       )
                                )
                              ),
                              #slider bar for months
                              sliderInput(inputId = "grow_moSlider",
                                          label = "Month Range:",
                                          step = 1,
                                          min = 1,
                                          max = 72,
                                          value=c(1,27),
                                          animate = TRUE
                              ),
                             
                              #slider for Client Growth
                              sliderTextInput(
                                inputId = "grow_growslide",
                                label = "Client Growth:",
                                grid = TRUE,
                                choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                            "15%","16%","17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%",
                                            "29%","30%","31%","32%","33%","34%","35%","36%","37%","38%","39%","40%","41%","42%","43%",
                                            "44%","45%","46%","47%","48%","49%","50%"),
                                selected = "Custom" 
                              ),
                              
                              #slider for Cost Multiplier
                              sliderTextInput(
                                inputId = "grow_mCost",
                                label = "Costs Multiplier:",
                                grid = TRUE,
                                choices = c("Custom","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                            "55%","60%","65%","70%","75%"),
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
                                            "29%","30%"),
                                selected = "Custom" 
                              ),
                              sliderTextInput(
                                inputId = "grow_Partners",
                                label = "Partners added per month:",
                                grid = TRUE,
                                choices = c("Custom","1","2","3","4","5"),
                                selected = "Custom"
                              ),
                              
                              sliderTextInput(
                                inputId = "grow_auto",
                                label = "Automation Multiplier",
                                grid = TRUE,
                                choices = c("Custom", "0%","0.25%","0.5%","0.75%","1%","1.25%","1.5%","1.75%","2%","2.25%",
                                            "2.5%","2.75%","3%","3.25%","3.5%","3.75%","4%"),
                                selected = "Custom"
                              ),
                              
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
                                          tabPanel("More Cohort Analysis", htmlOutput("grow_bubble"), 
                                                   h6(tags$i("**Values dislayed in this chart are dictated by the location of the 
                                                             rightmost \"Month Range\" slider")))
                                          
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
              titlePanel("The Model", windowTitle = "The Model"), # App title
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
                              
                              radioButtons(inputId = "part_diff_types",
                                           label = "Preselected Model:",
                                           choices = c("Conservative","Moderate","Aggressive"),
                                           inline = TRUE,
                                           selected = "None"),
                              
                              fluidRow(
                                column(6,
                                       fluidRow(
                                         column(4,
                                                h6(strong("Start:"))
                                         ),
                                         
                                         column(6,
                                                h6(textOutput(outputId = "part_montext"))
                                         ),
                                         column(4,
                                                h6(""))
                                       )
                                ),
                                column(6,
                                       fluidRow(
                                         column(4,
                                                h6(strong("End:"))
                                         ),
                                         column(6,
                                                h6(textOutput(outputId = "part_montext2"))
                                         ),
                                         column(4,
                                                h6(""))
                                       )
                                )
                              ),
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
                                choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                            "15%","16%","17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%",
                                            "29%","30%","31%","32%","33%","34%","35%","36%","37%","38%","39%","40%","41%","42%","43%",
                                            "44%","45%","46%","47%","48%","49%","50%"),
                                selected = "Custom" 
                              ),
                              
                              #slider for Cost Multiplier
                              sliderTextInput(
                                inputId = "part_mCost",
                                label = "Costs Multiplier:",
                                grid = TRUE,
                                choices = c("Custom","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                            "55%","60%","65%","70%","75%"),
                                selected = "Custom" 
                              ),
                              #slider for Churn
                              sliderTextInput(
                                inputId = "part_churn",
                                label = "Monthly Churn:",
                                grid = TRUE,
                                choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                            "15%","16%","17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%",
                                            "29%","30%"),
                                selected = "Custom" 
                              ),
                              sliderTextInput(
                                inputId = "part_Partners",
                                label = "Partners added per month:",
                                grid = TRUE,
                                choices = c("Custom","1","2","3","4","5"),
                                selected = "Custom"
                              ),
                              
                              
                              sliderTextInput(
                                inputId = "part_auto",
                                label = "Automation Multiplier",
                                grid = TRUE,
                                choices = c("Custom", "0%","0.25%","0.5%","0.75%","1%","1.25%","1.5%","1.75%","2%","2.25%",
                                            "2.5%","2.75%","3%","3.25%","3.5%","3.75%","4%"),
                                selected = "Custom"
                              ),
                              
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
                         titlePanel(paste0("The Model"), windowTitle = "The Model"), # App title
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
                                         
                                         radioButtons(inputId = "diff_types",
                                                      label = "Preselected Model:",
                                                      choices = c("Conservative","Moderate","Aggressive"),
                                                      inline = TRUE,
                                                      selected = "None"),
                                         
                                         fluidRow(
                                           column(6,
                                                  fluidRow(
                                                    column(4,
                                                           h6(strong("Start:"))
                                                    ),
                                                    
                                                    column(6,
                                                           h6(textOutput(outputId = "montext"))
                                                    ),
                                                    column(4,
                                                           h6(""))
                                                  )
                                           ),
                                           column(6,
                                                  fluidRow(
                                                    column(4,
                                                           h6(strong("End:"))
                                                    ),
                                                    column(6,
                                                           h6(textOutput(outputId = "montext2"))
                                                    ),
                                                    column(4,
                                                           h6(""))
                                                  )
                                           )
                                         ),
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
                                           choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                                       "15%","16%","17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%",
                                                       "28%","29%","30%","31%","32%","33%","34%","35%","36%","37%","38%","39%","40%","41%",
                                                       "42%","43%","44%","45%","46%","47%","48%","49%","50%"),
                                           selected = "Custom" 
                                         ),
                                         
                                         #slider for Cost Multiplier
                                         sliderTextInput(
                                           inputId = "mCost",
                                           label = "Costs Multiplier:",
                                           grid = TRUE,
                                           choices = c("Custom","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%",
                                                       "55%","60%","65%","70%","75%"),
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
                                                       "28%","29%","30%"),
                                           selected = "Custom" 
                                         ),
                                         sliderTextInput(
                                           inputId = "Partners",
                                           label = "Partners added per month:",
                                           grid = TRUE,
                                           choices = c("Custom","1","2","3","4","5"),
                                           selected = "Custom"
                                         ),
                                         
                                         sliderTextInput(
                                           inputId = "auto",
                                           label = "Automation Multiplier",
                                           grid = TRUE,
                                           choices = c("Custom", "0%","0.25%","0.5%","0.75%","1%","1.25%","1.5%","1.75%","2%","2.25%",
                                                       "2.5%","2.75%","3%","3.25%","3.5%","3.75%","4%"),
                                           selected = "Custom"
                                         ),
                                         
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
                tabPanel("Client Growth Function",
                         withMathJax(),
                         h3("Below is how Client Growth increases or decreases over time in relation to churn:"),
                         h2(uiOutput("clifunc")),
                         h2(uiOutput("derivative_clifunc")),
                         h4("x = Time in Months"),
                         h4("y-intercept = Client Growth"),
                         h4("Horizontal Asymptote: y = Churn"),
                         h4("n = How rapidly Client Growth approaches Churn without changing exponential shape."),  
                         fluidRow(
                           column(1), 
                           column(11, h4("n > 1: Client growth will go to churn rapidly. f'(x) will be larger"))
                         ),
                         fluidRow(
                           column(1), 
                           column(11, h4("n < 1: Client growth will go to churn slowly. f'(x) will be smaller"))
                         ),
                         fluidRow(
                           column(1), 
                           column(11, 
                                  textInput(inputId = "n_num",
                                            label = "Custom n:",
                                            value = 1))
                         ),
                         h4("k = How Linear vs Exponential Client growth changes."),
                         fluidRow(
                           column(1), 
                           column(11, h4("As k aproaches infinity, Client Growth will be more horizontal and f'(x) will 
                                         approach a constant, g(x) = 0. Assumption: x is within our range"))
                           ),
                         fluidRow(
                           column(1), 
                           column(11, h4("As k approaches 0, Client Growth will be more vertical. f'(x) will 
                                         approach a constant, g(x) = infinity. Assumption: x is within our range"))
                           ),
                         fluidRow(
                           column(1), 
                           column(11, 
                                  textInput(inputId = "k_num",
                                            label = "Custom k:",
                                            value = 7))
                         )),
                
                #Table dropdown in More 
                tabPanel("Raw Data",
                         htmlOutput('table')
                         ),
                tabPanel("Demo",
                         HTML('<iframe width="1000" height="600" 
                              src="https://www.youtube.com/embed/tX1gtG--OGE" 
                              frameborder="0" allow="autoplay; encrypted-media" 
                              allowfullscreen></iframe>')
                         )),
    
     
####################################################Partner Pay     
      tabPanel("Partner Pay Calculator",
              titlePanel(paste0("Partner Pay Calculator"), windowTitle = "The Model"), # App title
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
                              
                        
                              #slider for Revenue
                              sliderInput(inputId = "pp_rev",
                                          label = "Revenue:",
                                          step = 5000,
                                          min = 5000,
                                          max = 300000,
                                          value= 50000,
                                          animate = TRUE
                              ),
                              
                              #slider for Gross Margins
                              sliderInput(inputId = "pp_gmargin",
                                          label = "Gross Margins:(%)",
                                          step = 1,
                                          min = 1,
                                          max = 100,
                                          value= 50,
                                          animate = TRUE
                              ),
                              
                              #first slider bar for months
                              sliderInput(inputId = "pp_salcap",
                                          label = "Salary Cap:",
                                          step = 500,
                                          min = 1000,
                                          max = 12000,
                                          value= 1000,
                                          animate = TRUE
                              ),
                              
                              htmlOutput("pp_gprof"),
                              h4(" "),
                              
                              
                              #reload data clicker
                              fluidRow(
                                column(6,
                                       actionButton(inputId = "pp_reload", label = "Reload data"),
                                       textOutput('pp_clics')
                                ),
                                column(6,
                                       actionButton(inputId = "pp_newTable", label = "Show Sheet")
                                )
                              )
                            ),
                            
                            
                            # main panel outputs
                            mainPanel(
                              fluidRow(
                                column(12,
                                       selectInput(
                                         inputId = "pp_partnernames",
                                         label = "Partner",
                                         choices = c("Francis Pedraza",
                                                     "Keenahn Jung",
                                                     "Kamron Palizban",
                                                     "Gunar Gessner",
                                                     "Corey Breier",
                                                     "Leo Kewitz",
                                                     "Ric Pedraza",
                                                     "Melissa Smith",
                                                     "Marshall Sutherland",
                                                     "Rodrigo de Oliveira",
                                                     "Shane Mileham",
                                                     "Erinn Woodside",
                                                     "Jarvis Martin",
                                                     "Steven Luibrand",
                                                     "Tiffany Bellah",
                                                     "Mark Firth",
                                                     "Hamza Ouaghad",
                                                     "Thomas Chen",
                                                     "Zach O'Brien",
                                                     "Egor Dzezhyts",
                                                     "Alex Poole",
                                                     "Stijn Hendrikse",
                                                     "Horia Veselin",
                                                     "Skyler Stevens",
                                                     "Bilal Qureshi",
                                                     "Gabriel Altemann",
                                                     "Jay Kumar"),
                                         selected = "Francis Pedraza"
                                         
                                       )
                                      )
                                ),
                              htmlOutput("pp_doughchart")
                            )
              ),
              #this is the table at the bottom of the General page. 
              fluidRow(
                column(12,
                       htmlOutput("pp_main_table")
                )
              )
            )
        )
    )


