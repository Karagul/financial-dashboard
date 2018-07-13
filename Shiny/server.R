library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)


curr_date <- format(Sys.Date(), "%b '%y")

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  source('sheets_data.R', local = TRUE)
  
  
  #reload data button. This creates MRT$data which is the data frame with the main data we use from google speadsheets. 
  MRT <- reactiveValues()
  live <- reactiveValues()
  observeEvent(input$reload, {
    MRT$data <- gs_read( gs_key("1KdOJJ9rj3eWUKA4F6mX-MIEQKe5XJAMeJf4aZvQ1m8A"), ws = "Shiny sheet")
    live$data <- MRT$data
  }, ignoreNULL=FALSE)
  output$clics <- renderText(input$reload)
  ###########################################General PAGE#########################################################################
  #bottom page table
  observeEvent(input$newTable, {
    output$main_table <-renderGvis({
      gvisTable(live$data[,c("Month","Revenue", "Revenue percent change", "Total clients", "Total monthly ARPA", 
                          "Churn percentage weighted by number of clients","Client growth after churn", "Avg Customer Lifetime (months)",
                          "Company head count", "Number of agents", "Labor costs", "Revenue per head", "Gross profit",
                          "Gross margins", "Automation multiplier", "Number of partners", "Total partner pay", "Partner bonuses", 
                          "Sales and marketing costs", "Additional subscription costs", "Total R and D costs", "BD costs", 
                          "Discretionary spending", "Overhead","Overhead to opex", "Net profit", "Net margins", "CLTV", 
                          "CAC", "CLTV to CAC ratio", "Funds raised", "Cash in bank")])
    })
  }, ignoreNULL = TRUE)
  
  #Effects the slider for the client growth slider
  
  observeEvent(input$growslide, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$growslide == "Custom"){
            live$data$`Client growth percentage`[row_after_curr_mon] <- live$data$`Custom client growth`[row_after_curr_mon]
            Revenue_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Revenue_percent_change_fn(row_after_curr_mon)
            Total_monthly_ARPA_fn(row_after_curr_mon)
            Client_growth_after_churn_fn(row_after_curr_mon)
            CLTV_fn(row_after_curr_mon)
            CLTV_to_CAC_ratio_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Actual_labor_costs_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Gross_profit_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
            Total_partner_pay_fn(row_after_curr_mon)
            Sales_and_marketing_costs_fn(row_after_curr_mon)
            
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          else {
            growslide_numeric = as.numeric(gsub("[\\%,]", "", input$growslide))
            growslide_numeric = growslide_numeric / 100
            live$data$`Client growth percentage`[row_after_curr_mon] <- growslide_numeric
            Revenue_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Revenue_percent_change_fn(row_after_curr_mon)
            Total_monthly_ARPA_fn(row_after_curr_mon)
            Client_growth_after_churn_fn(row_after_curr_mon)
            CLTV_fn(row_after_curr_mon)
            CLTV_to_CAC_ratio_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Actual_labor_costs_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Gross_profit_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
            Total_partner_pay_fn(row_after_curr_mon)
            Sales_and_marketing_costs_fn(row_after_curr_mon)
            
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  
  #Effects the slider for the cost multiplier slider
  observeEvent(input$mCost, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$mCost == "Custom"){
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          else{
            mCost_numeric = as.numeric(gsub("[\\%,]", "", input$mCost))
            mCost_numeric = mCost_numeric / 100
            Composite_costs_multiplier_fn(row_after_curr_mon, mCost_numeric)
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$churn, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$churn == "Custom"){
            live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <-
              live$data$`Custom client churn`[row_after_curr_mon]
            Revenue_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Revenue_percent_change_fn(row_after_curr_mon)
            Total_monthly_ARPA_fn(row_after_curr_mon)
            Client_growth_after_churn_fn(row_after_curr_mon)
            CLTV_fn(row_after_curr_mon)
            CLTV_to_CAC_ratio_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Actual_labor_costs_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Gross_profit_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
            Total_partner_pay_fn(row_after_curr_mon)
            Sales_and_marketing_costs_fn(row_after_curr_mon)

            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          else {
            churn_numeric = as.numeric(gsub("[\\%,]", "", input$churn))
            churn_numeric = churn_numeric / 100
            live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <- churn_numeric
            Revenue_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Revenue_percent_change_fn(row_after_curr_mon)
            Total_monthly_ARPA_fn(row_after_curr_mon)
            Client_growth_after_churn_fn(row_after_curr_mon)
            CLTV_fn(row_after_curr_mon)
            CLTV_to_CAC_ratio_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Actual_labor_costs_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Gross_profit_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
            Total_partner_pay_fn(row_after_curr_mon)
            Sales_and_marketing_costs_fn(row_after_curr_mon)

            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$partbox, {
    if(input$partbox == "Linear"){
      updateSliderTextInput(session,
                            inputId = "Partners",
                            choices = c("Custom","1","2","3","4","5","6","7","8","9","10"),
                            selected = "Custom"
      )
    }
    else {
      updateSliderTextInput(session,
                            inputId = "Partners",
                            choices = c("Custom", "0.1%", "0.5%", "1%", "2%", "3%", "5%", "7.5%", "10%", "15%", "20%", "25%", "50%"),
                            selected = "Custom"
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$Partners, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$Partners == "Custom"){
            live$data$`Number of partners`[row_after_curr_mon] <- live$data$`Custom number of partners`[row_after_curr_mon]
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
          }
          else if(input$partbox == "Linear"){
            part_numeric = as.numeric(input$Partners)
            live$data$`Number of partners`[row_after_curr_mon] <- (live$data$`Number of partners`[row_after_curr_mon - 1] + part_numeric)
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
          }
          else if(input$partbox == "Exponential"){
            part_per_numeric = as.numeric(gsub("[\\%,]", "", input$Partners))
            part_per_numeric = part_per_numeric / 100
            live$data$`Number of partners`[row_after_curr_mon] <-
              (live$data$`Number of partners`[row_after_curr_mon - 1] * (1 + part_per_numeric))
            round(live$data$`Number of partners`[row_after_curr_mon], 0)
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
            Company_head_count_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$auto, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$auto == "Custom"){
            live$data$`Automation multiplier`[row_after_curr_mon] <- live$data$`Custom automation multiplier`[row_after_curr_mon]
            live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 1 / 
              live$data$`Automation multiplier`[row_after_curr_mon]
            Actual_labor_costs_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
          }
          else {
            auto_numeric = as.numeric(gsub("[\\%,]", "", input$auto))
            auto_numeric = auto_numeric / 100
            live$data$`Automation multiplier`[row_after_curr_mon] <- live$data$`Automation multiplier`[row_after_curr_mon - 1] * 
              (1 + auto_numeric)
            live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 1 / 
              live$data$`Automation multiplier`[row_after_curr_mon]
            Actual_labor_costs_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)


 ##################################################Profits#######################################################################
  output$profRev <-renderGvis({
    df=data.frame(Revenue= live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Overhead= live$data$Overhead[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profit= live$data$`Gross profit`[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit= live$data$`Net profit`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]", 
                                   title = "Profit/Revenue"))
  })
  
  ##########################################CLTV to CAC######################
  output$cltvcac <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  CLTV = live$data$CLTV[input$moSlider[1]:input$moSlider[2]],
                  CAC = live$data$CAC[input$moSlider[1]:input$moSlider[2]])
    gvisComboChart(df, xvar="Month",
                   yvar=c("CLTV","CAC"),
                   options=list(pointSize = 3, seriesType="line",
                                series="[{type:'bars',targetAxisIndex:0, color:'b8e986'}, 
                                {type:'bars',targetAxisIndex:1, color:'8497e5'}]",
                                vAxes="[{title:'CLTV'},{title:'CAC'}]", 
                                width = 800, height = 400, title = "CLTV and CAC", hAxis="{title:'Months'}"))
  })
  

 
  #######################################churn/client growth#################################################################################
  #churn vs client growth visual
  output$client <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Churn = live$data$`Churn percentage weighted by number of clients`[input$moSlider[1]:input$moSlider[2]],
                  Client_Growth = live$data$`Client growth percentage`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Percent'}",
                                   hAxis="{title:'Months'}", title = "Churn Vs Client Growth", 
                                   series = "[{color:'8497e5'}, {color: 'b8e986'}]"))
  })
  

  ############################Revenue per Head##########################################################################################
  
  #Revenue per head visual
  output$workForce <-renderGvis({
    df=data.frame(Revenue= live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Company_Head_Count= live$data$`Company head count`[input$moSlider[1]:input$moSlider[2]],
                  Revenue_per_head= live$data$`Revenue per head`[input$moSlider[1]:input$moSlider[2]],
                  Partners= live$data$`Number of partners`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Head Count'}",
                                   hAxis="{title:'Revenue'}", 
                                   series = "[{type:'line', 
                                   targetAxisIndex:0,
                                   color:'b8e986'}, 
                                   {type:'line', 
                                   targetAxisIndex:1,
                                   color:'8497e5'}]", title = "Revenue per Head",
                                   vAxes="[{title:'Company Head Count'}, {title:'Revenue per Head'}]"))
})

  
  
  
  ############################################Growth Page############################################################################
  MRT <- reactiveValues()
  live <- reactiveValues()
  observeEvent(input$grow_reload, {
    MRT$data <- gs_read( gs_key("1KdOJJ9rj3eWUKA4F6mX-MIEQKe5XJAMeJf4aZvQ1m8A"), ws = "Shiny sheet")
    live$data <- MRT$data
  }, ignoreNULL = TRUE)
  output$grow_clics <- renderText(input$grow_reload)
  #bottoom page table
  
  observeEvent(input$grow_newTable, {
    output$grow_main_table <-renderGvis({
      gvisTable(live$data[,c("Month","Revenue", "Revenue percent change", "Total clients", "Total monthly ARPA", 
                             "Churn percentage weighted by number of clients","Client growth after churn", "Avg Customer Lifetime (months)",
                             "Company head count", "Number of agents", "Labor costs", "Revenue per head", "Gross profit",
                             "Gross margins", "Automation multiplier", "Number of partners", "Total partner pay", "Partner bonuses", 
                             "Sales and marketing costs", "Additional subscription costs", "Total R and D costs", "BD costs", 
                             "Discretionary spending", "Overhead","Overhead to opex", "Net profit", "Net margins", "CLTV", 
                             "CAC", "CLTV to CAC ratio", "Funds raised", "Cash in bank")])
    })
  }, ignoreNULL = TRUE)
  
  #####################################Profit Page####################################################################################
  #profit visual
  output$grow_profit <- renderGvis({
    df=data.frame(Month = live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Gross_Profit = live$data$`Gross profit`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Net_Profit = live$data$`Net profit`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisColumnChart(df, xvar = "Month", 
                    yvar = c("Gross_Profit", "Net_Profit"),
                    options=list(bar="{groupWidth:'70%'}", width = "800", height = "400",
                                 isStacked = TRUE, vAxis="{title:'Dollars($)'}",
                                 hAxis="{title:'Months'}", title = "Profit",
                                 series = "[{color:'8497e5'}, {color:'b8e986'}]"))
  })
  #gross/ net margins visual
  output$grow_gross <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Gross_Margins= live$data$`Gross margins`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Net_Margins= live$data$`Net margins`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit Margins'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}]", 
                                   title = "Margins(%)"))
  })
  
  ################################Runway page########################################################################################
  #runway visual
  output$grow_runway <- renderGvis({
    df=data.frame(Month= live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Cash_in_Bank= live$data$`Cash in bank`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Net_Profit = live$data$`Net profit`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Funds_Raised = live$data$`Cumulative funds raised`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisLineChart(df, options=list(pointSize=6,width = 800, height = 400,vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", title = "Runway", 
                                   series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]"))
  })
  
  
  ###########################Growth Page##############################################################################################
  #client growth and revenue visual
  output$grow_num_client <- renderGvis({
    df=data.frame(Month = live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Revenue = live$data$Revenue[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Enterprise_Clients = live$data$`Enterprise clients`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Small_Business_Clients = live$data$`Small business clients`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Personal_Clients = live$data$`Personal clients`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisComboChart(df, xvar="Month",
                   yvar=c("Personal_Clients", "Revenue", "Small_Business_Clients", 
                          "Enterprise_Clients"),
                   options=list(pointSize = 3, seriesType="bar",
                                series="[{type:'bars',targetAxisIndex:0, color:'b8e986'}, 
                                {type:'line', targetAxisIndex:1, color:'8497e5'},
                                {type:'bars', color:'grey'},
                                {type: 'bars', color: 'black'}]",
                                vAxes="[{title:'Number of Clients'}, {title:'Revenue'}]",  isStacked = TRUE,
                                width = 800, height = 400, title = "Client Growth and Revenue", hAxis="{title:'Months'}"))
  })
  #revenue from clients visual
  output$grow_rev_client <- renderGvis({
    df=data.frame(Month = live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Revenue = live$data$Revenue[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Enterprise_Revenue = live$data$`Enterprise revenue`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Small_Business_Revenue = live$data$`Small business revenue`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Personal_Revenue = live$data$`Personal revenue`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisComboChart(df, xvar="Month",
                   yvar=c("Personal_Revenue", "Revenue", "Small_Business_Revenue", 
                          "Enterprise_Revenue"),
                   options=list(pointSize = 3, seriesType="bar",
                                series="[{type:'bars', color:'b8e986'}, 
                                {type:'line', color:'8497e5'},
                                {type:'bars', color:'grey'},
                                {type: 'bars', color: 'black'}]",
                                vAxes="[{title:'Revenue'}]",  isStacked = TRUE,
                                width = 800, height = 400, title = "Revenue from Clients", hAxis="{title:'Months'}"))
  })
  
  
  
  ######################################Costs PAGE##################################################################################
  MRT <- reactiveValues()
  live <- reactiveValues()
  observeEvent(input$part_reload, {
    MRT$data <- gs_read( gs_key("1KdOJJ9rj3eWUKA4F6mX-MIEQKe5XJAMeJf4aZvQ1m8A"), ws = "Shiny sheet")
    live$data <- MRT$data
  }, ignoreNULL = TRUE)
  output$part_clics <- renderText(input$part_reload)
  #bottom page table
  observeEvent(input$part_newTable, {
    output$part_main_table <-renderGvis({
      gvisTable(live$data[,c("Month","Revenue", "Revenue percent change", "Total clients", "Total monthly ARPA", 
                             "Churn percentage weighted by number of clients","Client growth after churn", "Avg Customer Lifetime (months)",
                             "Company head count", "Number of agents", "Labor costs", "Revenue per head", "Gross profit",
                             "Gross margins", "Automation multiplier", "Number of partners", "Total partner pay", "Partner bonuses", 
                             "Sales and marketing costs", "Additional subscription costs", "Total R and D costs", "BD costs", 
                             "Discretionary spending", "Overhead","Overhead to opex", "Net profit", "Net margins", "CLTV", 
                             "CAC", "CLTV to CAC ratio", "Funds raised", "Cash in bank")])
    })
  }, ignoreNULL = TRUE)
  
  
  ##############################################Labor Costs #######################################################################
  output$part_Lcost <- renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Actual_Labor_Cost = live$data$`Actual labor costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Expected_Operator_Labor_cost = live$data$`Expected operator labor costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Expected_RRR_Labor_Cost = 
                    live$data$`Expected RRR labor costs for assistants`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Expected_Specialist_and_Strategist_Labor_Costs = 
                    live$data$`Expected specialist and strategist labor costs`[input$part_moSlider[1]:input$part_moSlider[2]]
    )
    gvisComboChart(df, xvar="Month",
                   yvar=c("Expected_Operator_Labor_cost", "Actual_Labor_Cost", "Expected_RRR_Labor_Cost", 
                          "Expected_Specialist_and_Strategist_Labor_Costs"),
                   options=list(pointSize = 3, seriesType="line",
                                series="[{type:'line', 
                                targetAxisIndex:0,
                                color:'b8e986'}, 
                                {type:'bars', 
                                targetAxisIndex:1,
                                color:'8497e5'},
                                {color:'grey'}, {color:'black'}]",
                                vAxes="[{title:'Expected_Operator_Labor_cost'}, {title:'Actual_Labor_Cost'}]", 
                                width = 800, height = 400, title = "Labor Costs", hAxis="{title:'Months'}"))
  })
  
  
  ############################Partner pay########################################################################################
  #partner pay visual
  output$part_combo <- renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Partner_Pay = live$data$`Total partner pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profit = live$data$`Gross profit`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisComboChart(df, xvar = "Month", 
                   yvar = c("Partner_Pay", "Gross_Profit"),
                   options=list(seriesType="bars",
                                series="[{type:'line', color: '8497e5'},
                                {type:'bars', color: 'b8e986'}]", 
                                width = 800, height = 400, vAxis="{title:'Dollars($)'}", pointSize = 4,
                                hAxis="{title:'Months'}", title = "Partner Pay"))
  })
  
  #partner pay compared to Revenue visual
  output$part_linechart <- renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]], 
                  Partner_Pay = live$data$`Total partner pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profit = live$data$`Gross profit`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = live$data$`Net profit`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Black'}, {color:'Red'}]", title = "Partner Pay Compared to Revenue"))
  })
  
  ########################################Overhead Page##############################################################################
  #overhead visual
  output$part_overhead <-renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"],
                  Total_Overhead = live$data$Overhead[input$part_moSlider[1]:input$part_moSlider[2]],
                  BD_Costs= live$data$`BD costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  RD_Costs= live$data$`Total R and D costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Additional_Subscription_Costs= live$data$`Additional subscription costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Discretionary_Spending= live$data$`Discretionary spending`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partner_Pay= live$data$`Total partner pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partner_Bonuses= live$data$`Partner bonuses`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Red'}]", title = "Overhead"))
  })
  
  #Ecoomies of Scale visual
  output$part_econScale <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  Overhead_to_Opex_ratio= live$data$`Overhead to opex`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Overhead/Opex %'}",
                                   hAxis="{title:'Revenue'}", series = "[{color: '8497e5'}]", title = "Economies of Scale"))
  })
  
  #########################################MORE PAGE###############################################
  
  #######################################spreadsheet##############################################
  
  output$table <-renderGvis({
    gvisTable(live$data)
  })
  
############################################Updating all the sliders####################################################################
  
  ######Month
  observeEvent(input$moSlider,{
    updateSliderInput(session, "grow_moSlider", value = c(input$moSlider[1],input$moSlider[2]))
    updateSliderInput(session, "part_moSlider", value = c(input$moSlider[1],input$moSlider[2]))
  }, ignoreInit = TRUE)
  observeEvent(input$part_moSlider,{
    updateSliderInput(session, "moSlider", value = c(input$part_moSlider[1],input$part_moSlider[2]))
    updateSliderInput(session, "grow_moSlider", value = c(input$part_moSlider[1],input$part_moSlider[2]))
  }, ignoreInit = TRUE)
  observeEvent(input$grow_moSlider,{
    updateSliderInput(session, "moSlider", value = c(input$grow_moSlider[1],input$grow_moSlider[2]))
    updateSliderInput(session, "part_moSlider", value = c(input$grow_moSlider[1],input$grow_moSlider[2]))
  }, ignoreInit = TRUE)
  
  ######Client Growth
  observeEvent(input$part_growslide,{
    updateSliderTextInput(session, "grow_growslide", selected = input$part_growslide)
    updateSliderTextInput(session, "growslide", selected = input$part_growslide)
  }, ignoreInit = TRUE)
  observeEvent(input$grow_growslide,{
    updateSliderTextInput(session, "part_growslide", selected = input$grow_growslide)
    updateSliderTextInput(session, "growslide", selected = input$grow_growslide)
  }, ignoreInit = TRUE)
  observeEvent(input$growslide,{
    updateSliderTextInput(session, "part_growslide", selected = input$growslide)
    updateSliderTextInput(session, "grow_growslide", selected = input$growslide)
  }, ignoreInit = TRUE)
  
  ###### Costs Multiplier
  observeEvent(input$grow_mCost,{
    updateSliderTextInput(session, "mCost", selected = input$grow_mCost)
    updateSliderTextInput(session, "part_mCost", selected = input$grow_mCost)
  }, ignoreInit = TRUE)
  observeEvent(input$part_mCost,{
    updateSliderTextInput(session, "grow_mCost", selected = input$part_mCost)
    updateSliderTextInput(session, "mCost", selected = input$part_mCost)
  }, ignoreInit = TRUE)
  observeEvent(input$mCost,{
    updateSliderTextInput(session, "grow_mCost", selected = input$mCost)
    updateSliderTextInput(session, "part_mCost", selected = input$mCost)
  }, ignoreInit = TRUE)

  ###### Churn
  observeEvent(input$grow_churn,{
    updateSliderTextInput(session, "churn", selected = input$grow_churn)
    updateSliderTextInput(session, "part_churn", selected = input$grow_churn)
  }, ignoreInit = TRUE)
  observeEvent(input$part_churn,{
    updateSliderTextInput(session, "churn", selected = input$part_churn)
    updateSliderTextInput(session, "grow_churn", selected = input$part_churn)
  }, ignoreInit = TRUE)
  observeEvent(input$churn,{
    updateSliderTextInput(session, "grow_churn", selected = input$churn)
    updateSliderTextInput(session, "part_churn", selected = input$churn)
  }, ignoreInit = TRUE)
  
  ##### partbox
  observeEvent(input$partbox,{
    updateRadioButtons(session, "grow_partbox", selected = input$partbox)
    updateRadioButtons(session, "part_partbox", selected = input$partbox)
  }, ignoreInit = TRUE)
  observeEvent(input$grow_partbox,{
    updateRadioButtons(session, "part_partbox", selected = input$grow_partbox)
    updateRadioButtons(session, "partbox", selected = input$grow_partbox)
  }, ignoreInit = TRUE)
  observeEvent(input$part_partbox,{
    updateRadioButtons(session, "partbox", selected = input$part_partbox)
    updateRadioButtons(session, "grow_partbox", selected = input$part_partbox)
  }, ignoreInit = TRUE)
  
  ###### number of partners
  observeEvent(input$grow_Partners,{
    updateSliderTextInput(session, "Partners", selected = input$grow_Partners)
    updateSliderTextInput(session, "part_Partners", selected = input$grow_Partners)
  }, ignoreInit = TRUE)
  observeEvent(input$part_Partners,{
    updateSliderTextInput(session, "Partners", selected = input$part_Partners)
    updateSliderTextInput(session, "grow_Partners", selected = input$part_Partners)
  }, ignoreInit = TRUE)
  observeEvent(input$Partners,{
    updateSliderTextInput(session, "part_Partners", selected = input$Partners)
    updateSliderTextInput(session, "grow_Partners", selected = input$Partners)
  }, ignoreInit = TRUE)
  
  observeEvent(input$grow_auto,{
    updateSliderTextInput(session, "part_auto", selected = input$grow_auto)
    updateSliderTextInput(session, "auto", selected = input$grow_auto)
  }, ignoreInit = TRUE)
  observeEvent(input$part_auto,{
    updateSliderTextInput(session, "auto", selected = input$part_auto)
    updateSliderTextInput(session, "grow_auto", selected = input$part_auto)
  }, ignoreInit = TRUE)
  observeEvent(input$auto,{
    updateSliderTextInput(session, "part_auto", selected = input$auto)
    updateSliderTextInput(session, "grow_auto", selected = input$auto)
  }, ignoreInit = TRUE)
  
  }