library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)


curr_date <- format(Sys.Date(), "%b '%y")
#end_date <-

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  source('General_page_functions.R', local = TRUE)
  source("Growth_page_functions.R", local = TRUE)
  source("Part_page_functions.R", local = TRUE)
  
  
  #reload data button. This creates MRT$data which is the data frame with the main data we use from google speadsheets. 
  MRT <- reactiveValues()
  live <- reactiveValues()
  grow_live <-reactiveValues()
  part_live <-reactiveValues()
  observeEvent(input$reload, {
    MRT$data <- gs_read( gs_key("1K4hHyfnJhuWijJpDT6KRZtI1bLHlukTb4svKT4HKVeA"), ws = "Shiny sheet")
    live$data <- MRT$data
    grow_live$data <- MRT$data
    part_live$data <- MRT$data
  }, ignoreNULL=FALSE)
  output$clics <- renderText(input$reload)
  observeEvent(input$reload, {
    updateSliderTextInput(session, inputId = "growslide", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "mCost", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "churn", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "Partners", selected = isolate("Custom"))
    updateRadioButtons(session, inputId = "partbox", selected = isolate("Linear"))
    updateSliderTextInput(session, inputId = "auto", selected = isolate("Custom"))
  },ignoreInit = TRUE)  
  
  
  ################################################################################################################################### 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########################################General PAGE#########################################################################
  #bottom page table
  output$montext <- renderText({live$data$Month[input$moSlider[1]]})
  output$montext2 <- renderText({live$data$Month[input$moSlider[2]]})
  
  observeEvent(input$newTable, {
    output$main_table <-renderGvis({
      gvisTable(live$data[,c("Month","Revenue", "Revenue percent change", "Total clients", "Churn percentage weighted by number of clients",
                          "Hrs client demand","Client growth after churn","Total monthly ARPA", "Avg Customer Lifetime (months)","CLTV",
                          "Company head count", "Number of agents", "Labor costs", "Revenue per head", "Gross profit",
                          "Gross margins", "Automation multiplier", "Number of partners", "Total partner pay", "Partner bonuses",
                          "Sales and marketing costs", "Additional subscription costs", "Total R and D costs", "BD costs",
                          "Discretionary spending", "Overhead", "Opex","Overhead to opex", "Operating profit", "CAC", "CLTV to CAC ratio", 
                          "Net profit", "Net margins", "Funds raised", "Cash in bank")], options = list(frozenColumns = 1))
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
            Company_head_count_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Total_partner_pay_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          else if(input$partbox == "Linear"){
            part_numeric = as.numeric(input$Partners)
            live$data$`Number of partners`[row_after_curr_mon] <- (live$data$`Number of partners`[row_after_curr_mon - 1] + part_numeric)
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Total_partner_pay_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          else if(input$partbox == "Exponential"){
            part_per_numeric = as.numeric(gsub("[\\%,]", "", input$Partners))
            part_per_numeric = part_per_numeric / 100
            live$data$`Number of partners`[row_after_curr_mon] <-
              round((live$data$`Number of partners`[row_after_curr_mon - 1] * (1 + part_per_numeric)), 0)
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Total_partner_pay_fn(row_after_curr_mon)
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
  
  observeEvent(input$auto, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$auto == "Custom"){
            live$data$`Automation multiplier`[row_after_curr_mon] <- live$data$`Custom automation multiplier`[row_after_curr_mon]
            live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 
              live$data$`Automation multiplier`[row_after_curr_mon]
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Company_head_count_fn(row_after_curr_mon)
            Actual_labor_costs_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
            
          }
          else {
            auto_numeric = as.numeric(gsub("[\\%,]", "", input$auto))
            auto_numeric = auto_numeric / 100
            live$data$`Automation multiplier`[row_after_curr_mon] <- live$data$`Automation multiplier`[row_after_curr_mon - 1] * 
              (1 + auto_numeric)
            live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 1 / 
              live$data$`Automation multiplier`[row_after_curr_mon]
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_fn(row_after_curr_mon)
            Actual_labor_costs_fn(row_after_curr_mon)
            Revenue_per_head_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Gross_margins_fn(row_after_curr_mon)
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


 ##################################################Profits#######################################################################
  output$profRev <-renderGvis({
    df=data.frame(Revenue= live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Overhead= live$data$Overhead[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profit= live$data$`Gross profit`[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit= live$data$`Net profit`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
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
                                series="[{type:'bars', color:'b8e986'}, 
                                {type:'bars', color:'8497e5'}]",
                                vAxes="[{title:'Dollars($)'},{title:'Dollars($)'}]", 
                                width = 800, height = 400, title = "CLTV and CAC", hAxis="{title:'Months'}"))
  })
  

 
  #######################################churn/client growth###############################################################################
  #churn vs client growth visual
  output$client <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Churn = live$data$`Churn percentage weighted by number of clients`[input$moSlider[1]:input$moSlider[2]],
                  Client_Growth = live$data$`Client growth percentage`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Percent(%)'}",
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
                                   vAxes="[{title:'Number of Emplyees'}, {title:'Dollars($)'}]"))
})

  
  
 ################################################################################################################################### 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################Growth Page############################################################################
  output$grow_montext <- renderText({live$data$Month[input$grow_moSlider[1]]})
  output$grow_montext2 <- renderText({live$data$Month[input$grow_moSlider[2]]})
  
  observeEvent(input$grow_reload, {
    MRT$data <- gs_read( gs_key("1K4hHyfnJhuWijJpDT6KRZtI1bLHlukTb4svKT4HKVeA"), ws = "Shiny sheet")
    grow_live$data <- MRT$data
  }, ignoreNULL = TRUE)
  output$grow_clics <- renderText(input$grow_reload)
  observeEvent(input$grow_reload, {
    updateSliderTextInput(session, inputId = "grow_growslide", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "grow_mCost", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "grow_churn", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "grow_Partners", selected = isolate("Custom"))
    updateRadioButtons(session, inputId = "grow_partbox", selected = isolate("Linear"))
    updateSliderTextInput(session, inputId = "grow_auto", selected = isolate("Custom"))
  },ignoreInit = TRUE)
  
  #bottoom page table
  
  observeEvent(input$grow_newTable, {
    output$grow_main_table <-renderGvis({
      gvisTable(grow_live$data[,c("Month","Revenue", "Revenue percent change", "Total clients", "Churn percentage weighted by number of clients",
                             "Hrs client demand","Client growth after churn","Total monthly ARPA", "Avg Customer Lifetime (months)","CLTV",
                             "Company head count", "Number of agents", "Labor costs", "Revenue per head", "Gross profit",
                             "Gross margins", "Automation multiplier", "Number of partners", "Total partner pay", "Partner bonuses",
                             "Sales and marketing costs", "Additional subscription costs", "Total R and D costs", "BD costs",
                             "Discretionary spending", "Overhead", "Opex","Overhead to opex", "Operating profit", "CAC", "CLTV to CAC ratio", 
                             "Net profit", "Net margins", "Funds raised", "Cash in bank")], options = list(frozenColumns = 1))
    })
  }, ignoreNULL = TRUE)
  
  
  #Effects the slider for the client growth slider
  
  observeEvent(input$grow_growslide, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$grow_growslide == "Custom"){
            grow_live$data$`Client growth percentage`[row_after_curr_mon] <- grow_live$data$`Custom client growth`[row_after_curr_mon]
            Revenue_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Revenue_percent_change_growfn(row_after_curr_mon)
            Total_monthly_ARPA_growfn(row_after_curr_mon)
            Client_growth_after_churn_growfn(row_after_curr_mon)
            CLTV_growfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Actual_labor_costs_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Gross_profit_growfn(row_after_curr_mon)
            Gross_margins_growfn(row_after_curr_mon)
            Total_partner_pay_growfn(row_after_curr_mon)
            Sales_and_marketing_costs_growfn(row_after_curr_mon)
            
            Composite_costs_multiplier_growfn(row_after_curr_mon, 
                                              grow_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          else {
            growslide_numeric = as.numeric(gsub("[\\%,]", "", input$grow_growslide))
            growslide_numeric = growslide_numeric / 100
            grow_live$data$`Client growth percentage`[row_after_curr_mon] <- growslide_numeric
            Revenue_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Revenue_percent_change_growfn(row_after_curr_mon)
            Total_monthly_ARPA_growfn(row_after_curr_mon)
            Client_growth_after_churn_growfn(row_after_curr_mon)
            CLTV_growfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Actual_labor_costs_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Gross_profit_growfn(row_after_curr_mon)
            Gross_margins_growfn(row_after_curr_mon)
            Total_partner_pay_growfn(row_after_curr_mon)
            Sales_and_marketing_costs_growfn(row_after_curr_mon)
            
            Composite_costs_multiplier_growfn(row_after_curr_mon, grow_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  
  #Effects the slider for the cost multiplier slider
  observeEvent(input$grow_mCost, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$grow_mCost == "Custom"){
            Composite_costs_multiplier_growfn(row_after_curr_mon, 
                                              grow_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          else{
            mCost_numeric = as.numeric(gsub("[\\%,]", "", input$grow_mCost))
            mCost_numeric = mCost_numeric / 100
            Composite_costs_multiplier_growfn(row_after_curr_mon, mCost_numeric)
            Overhead_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$grow_churn, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$grow_churn == "Custom"){
            grow_live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <-
              grow_live$data$`Custom client churn`[row_after_curr_mon]
            Revenue_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Revenue_percent_change_growfn(row_after_curr_mon)
            Total_monthly_ARPA_growfn(row_after_curr_mon)
            Client_growth_after_churn_growfn(row_after_curr_mon)
            CLTV_growfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Actual_labor_costs_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Gross_profit_growfn(row_after_curr_mon)
            Gross_margins_growfn(row_after_curr_mon)
            Total_partner_pay_growfn(row_after_curr_mon)
            Sales_and_marketing_costs_growfn(row_after_curr_mon)
            
            Composite_costs_multiplier_growfn(row_after_curr_mon, 
                                              grow_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          else {
            churn_numeric = as.numeric(gsub("[\\%,]", "", input$grow_churn))
            churn_numeric = churn_numeric / 100
            grow_live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <- churn_numeric
            Revenue_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Revenue_percent_change_growfn(row_after_curr_mon)
            Total_monthly_ARPA_growfn(row_after_curr_mon)
            Client_growth_after_churn_growfn(row_after_curr_mon)
            CLTV_growfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_growfn(row_after_curr_mon)
            Company_head_count_growfn(row_after_curr_mon)
            Actual_labor_costs_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Gross_profit_growfn(row_after_curr_mon)
            Gross_margins_growfn(row_after_curr_mon)
            Total_partner_pay_growfn(row_after_curr_mon)
            Sales_and_marketing_costs_growfn(row_after_curr_mon)
            
            Composite_costs_multiplier_growfn(row_after_curr_mon, grow_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$grow_partbox, {
    if(input$grow_partbox == "Linear"){
      updateSliderTextInput(session,
                            inputId = "grow_Partners",
                            choices = c("Custom","1","2","3","4","5","6","7","8","9","10"),
                            selected = "Custom"
      )
    }
    else {
      updateSliderTextInput(session,
                            inputId = "grow_Partners",
                            choices = c("Custom", "0.1%", "0.5%", "1%", "2%", "3%", "5%", "7.5%", "10%", "15%", "20%", "25%", "50%"),
                            selected = "Custom"
      )
    }
  }, ignoreInit = TRUE)
 
  
  observeEvent(input$grow_Partners, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$grow_Partners == "Custom"){
            grow_live$data$`Number of partners`[row_after_curr_mon] <- grow_live$data$`Custom number of partners`[row_after_curr_mon]
            Composite_costs_multiplier_growfn(row_after_curr_mon, 
                                              grow_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Company_head_count_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Total_partner_pay_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          else if(input$grow_partbox == "Linear"){
            part_numeric = as.numeric(input$grow_Partners)
            grow_live$data$`Number of partners`[row_after_curr_mon] <- (grow_live$data$`Number of partners`[row_after_curr_mon - 1] + 
                                                                          part_numeric)
            Composite_costs_multiplier_growfn(row_after_curr_mon, grow_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Total_partner_pay_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          else if(input$grow_partbox == "Exponential"){
            part_per_numeric = as.numeric(gsub("[\\%,]", "", input$grow_Partners))
            part_per_numeric = part_per_numeric / 100
            grow_live$data$`Number of partners`[row_after_curr_mon] <-
              round((grow_live$data$`Number of partners`[row_after_curr_mon - 1] * (1 + part_per_numeric)), 0)
            Composite_costs_multiplier_growfn(row_after_curr_mon, grow_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Total_partner_pay_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$grow_auto, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$grow_auto == "Custom"){
            grow_live$data$`Automation multiplier`[row_after_curr_mon] <- grow_live$data$`Custom automation multiplier`[row_after_curr_mon]
            grow_live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 
              grow_live$data$`Automation multiplier`[row_after_curr_mon]
            Composite_costs_multiplier_growfn(row_after_curr_mon, grow_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Company_head_count_growfn(row_after_curr_mon)
            Actual_labor_costs_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Gross_margins_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          else {
            auto_numeric = as.numeric(gsub("[\\%,]", "", input$grow_auto))
            auto_numeric = auto_numeric / 100
            grow_live$data$`Automation multiplier`[row_after_curr_mon] <- grow_live$data$`Automation multiplier`[row_after_curr_mon - 1] * 
              (1 + auto_numeric)
            grow_live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 1 / 
              grow_live$data$`Automation multiplier`[row_after_curr_mon]
            Composite_costs_multiplier_growfn(row_after_curr_mon, grow_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_growfn(row_after_curr_mon)
            Actual_labor_costs_growfn(row_after_curr_mon)
            Revenue_per_head_growfn(row_after_curr_mon)
            Overhead_to_opex_growfn(row_after_curr_mon)
            Gross_margins_growfn(row_after_curr_mon)
            Net_profit_growfn(row_after_curr_mon)
            Net_margins_growfn(row_after_curr_mon)
            Cash_in_bank_growfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  
  #####################################Profit Page####################################################################################
  #profit visual
  output$grow_profit <- renderGvis({
    df=data.frame(Month = grow_live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Gross_Profit = grow_live$data$`Gross profit`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Net_Profit = grow_live$data$`Net profit`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisColumnChart(df, xvar = "Month", 
                    yvar = c("Gross_Profit", "Net_Profit"),
                    options=list(bar="{groupWidth:'70%'}", width = "800", height = "400",
                                 isStacked = TRUE, vAxis="{title:'Dollars($)'}",
                                 hAxis="{title:'Months'}", title = "Profit",
                                 series = "[{color:'8497e5'}, {color:'b8e986'}]"))
  })
  #gross/ net margins visual
  output$grow_gross <-renderGvis({
    df=data.frame(Revenue = grow_live$data$Revenue[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Gross_Margins= grow_live$data$`Gross margins`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Net_Margins= grow_live$data$`Net margins`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Percent(%)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}]", 
                                   title = "Margins(%)"))
  })
  
  ################################Runway page########################################################################################
  #runway visual
  output$grow_runway <- renderGvis({
    df=data.frame(Month= grow_live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Cash_in_Bank= grow_live$data$`Cash in bank`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Net_Profit = grow_live$data$`Net profit`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Cumulative_Funds_Raised = grow_live$data$`Cumulative funds raised`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisLineChart(df, options=list(pointSize=6,width = 800, height = 400,vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", title = "Runway", 
                                   series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]"))
  })
  
  
  ###########################Growth Page##############################################################################################
  #client growth and revenue visual
  output$grow_num_client <- renderGvis({
    df=data.frame(Month = grow_live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Revenue = grow_live$data$Revenue[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Enterprise_Clients = grow_live$data$`Enterprise clients`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Small_Business_Clients = grow_live$data$`Small business clients`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Personal_Clients = grow_live$data$`Personal clients`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisComboChart(df, xvar="Month",
                   yvar=c("Personal_Clients", "Revenue", "Small_Business_Clients", 
                          "Enterprise_Clients"),
                   options=list(pointSize = 3, seriesType="bar",
                                series="[{type:'bars',targetAxisIndex:0, color:'b8e986'}, 
                                {type:'line', targetAxisIndex:1, color:'8497e5'},
                                {type:'bars', color:'grey'},
                                {type: 'bars', color: 'black'}]",
                                vAxes="[{title:'Number of Clients'}, {title:'Dollars($)'}]",  isStacked = TRUE,
                                width = 800, height = 400, title = "Cohort Analysis by Number of Clients", hAxis="{title:'Months'}"))
  })
  #revenue from clients visual
  output$grow_rev_client <- renderGvis({
    df=data.frame(Month = grow_live$data[input$grow_moSlider[1]:input$grow_moSlider[2],"Month"], 
                  Revenue = grow_live$data$Revenue[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Enterprise_Revenue = grow_live$data$`Enterprise revenue`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Small_Business_Revenue = grow_live$data$`Small business revenue`[input$grow_moSlider[1]:input$grow_moSlider[2]],
                  Personal_Revenue = grow_live$data$`Personal revenue`[input$grow_moSlider[1]:input$grow_moSlider[2]])
    gvisComboChart(df, xvar="Month",
                   yvar=c("Personal_Revenue", "Revenue", "Small_Business_Revenue", 
                          "Enterprise_Revenue"),
                   options=list(pointSize = 3, seriesType="bar",
                                series="[{type:'bars', color:'b8e986'}, 
                                {type:'line', color:'8497e5'},
                                {type:'bars', color:'grey'},
                                {type: 'bars', color: 'black'}]",
                                vAxes="[{title:'Dollars($)'}]",  isStacked = TRUE,
                                width = 800, height = 400, title = "Cohort Analysis by Revenue", hAxis="{title:'Months'}"))
  })
  
  output$grow_bubble <- renderGvis({
    df=data.frame(
        Name = c("Enterprise_Clients", "Small_Business_Clients", "Personal_Clients"),
        Client_Percentage = c((100 * grow_live$data$`Percent enterprise clients`[input$grow_moSlider[2]]), 
                              (100 * grow_live$data$`Percent small business clients`[input$grow_moSlider[2]]), 
                              (100 * grow_live$data$`Percent personal clients`[input$grow_moSlider[2]])),
        Client_Percentage_Weighted_by_Dollar_Value = 
          c((100 * grow_live$data$`Percent enterprise clients weighted by dollar value`[input$grow_moSlider[2]]), 
            (100 * grow_live$data$`Percent small business clients weighted by dollar value`[input$grow_moSlider[2]]),
            (100 * grow_live$data$`Percent personal clients weighted by dollar value`[input$grow_moSlider[2]])),
        Revenue = c((grow_live$data$`Enterprise revenue`[input$grow_moSlider[2]]), 
                    (grow_live$data$`Small business revenue`[input$grow_moSlider[2]]),
                    (grow_live$data$`Personal revenue`[input$grow_moSlider[2]])))
    
    gvisBubbleChart(df,  idvar = "Name", 
                    xvar = "Client_Percentage_Weighted_by_Dollar_Value", 
                    yvar = "Client_Percentage", 
                    colorvar = "Name",
                    sizevar = "Revenue",
                    options=list(title = "Cohort Analysis of Clients", width = 850, height = 500,
                                 colors = "['b8e986', '8497e5', 'Grey']",
                                 hAxis="{title:'Client Percentage Weighted by Dollar Value'}",
                                 vAxis="{title:'Client Percentage'}")
    )
  })
  
  
  
  ################################################################################################################################### 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######################################Costs PAGE##################################################################################
  output$part_montext <- renderText({live$data$Month[input$part_moSlider[1]]})
  output$part_montext2 <- renderText({live$data$Month[input$part_moSlider[2]]})
  
  observeEvent(input$part_reload, {
    MRT$data <- gs_read( gs_key("1K4hHyfnJhuWijJpDT6KRZtI1bLHlukTb4svKT4HKVeA"), ws = "Shiny sheet")
    part_live$data <- MRT$data
  }, ignoreNULL = TRUE)
  output$part_clics <- renderText(input$part_reload)
  observeEvent(input$part_reload, {
    updateSliderTextInput(session, inputId = "part_growslide", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "part_mCost", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "part_churn", selected = isolate("Custom"))
    updateSliderTextInput(session, inputId = "part_Partners", selected = isolate("Custom"))
    updateRadioButtons(session, inputId = "part_partbox", selected = isolate("Linear"))
    updateSliderTextInput(session, inputId = "part_auto", selected = isolate("Custom"))
  },ignoreInit = TRUE) 
  
  #bottom page table
  observeEvent(input$part_newTable, {
    output$part_main_table <-renderGvis({
      gvisTable(part_live$data[,c("Month","Revenue", "Revenue percent change", "Total clients", "Churn percentage weighted by number of clients",
                             "Hrs client demand","Client growth after churn","Total monthly ARPA", "Avg Customer Lifetime (months)","CLTV",
                             "Company head count", "Number of agents", "Labor costs", "Revenue per head", "Gross profit",
                             "Gross margins", "Automation multiplier", "Number of partners", "Total partner pay", "Partner bonuses",
                             "Sales and marketing costs", "Additional subscription costs", "Total R and D costs", "BD costs",
                             "Discretionary spending", "Overhead", "Opex","Overhead to opex", "Operating profit", "CAC", "CLTV to CAC ratio", 
                             "Net profit", "Net margins", "Funds raised", "Cash in bank")], options = list(frozenColumns = 1))
    })
  }, ignoreNULL = TRUE)
  
  
  #Effects the slider for the client growth slider
  
  observeEvent(input$part_growslide, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$part_growslide == "Custom"){
            part_live$data$`Client growth percentage`[row_after_curr_mon] <- part_live$data$`Custom client growth`[row_after_curr_mon]
            Revenue_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Revenue_percent_change_partfn(row_after_curr_mon)
            Total_monthly_ARPA_partfn(row_after_curr_mon)
            Client_growth_after_churn_partfn(row_after_curr_mon)
            CLTV_partfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Actual_labor_costs_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Gross_profit_partfn(row_after_curr_mon)
            Gross_margins_partfn(row_after_curr_mon)
            Total_partner_pay_partfn(row_after_curr_mon)
            Sales_and_marketing_costs_partfn(row_after_curr_mon)
            
            Composite_costs_multiplier_partfn(row_after_curr_mon, 
                                              part_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          else {
            growslide_numeric = as.numeric(gsub("[\\%,]", "", input$part_growslide))
            growslide_numeric = growslide_numeric / 100
            part_live$data$`Client growth percentage`[row_after_curr_mon] <- growslide_numeric
            Revenue_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Revenue_percent_change_partfn(row_after_curr_mon)
            Total_monthly_ARPA_partfn(row_after_curr_mon)
            Client_growth_after_churn_partfn(row_after_curr_mon)
            CLTV_partfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Actual_labor_costs_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Gross_profit_partfn(row_after_curr_mon)
            Gross_margins_partfn(row_after_curr_mon)
            Total_partner_pay_partfn(row_after_curr_mon)
            Sales_and_marketing_costs_partfn(row_after_curr_mon)
            
            Composite_costs_multiplier_partfn(row_after_curr_mon, part_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  
  #Effects the slider for the cost multiplier slider
  observeEvent(input$part_mCost, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$part_mCost == "Custom"){
            Composite_costs_multiplier_partfn(row_after_curr_mon, 
                                              part_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          else{
            mCost_numeric = as.numeric(gsub("[\\%,]", "", input$part_mCost))
            mCost_numeric = mCost_numeric / 100
            Composite_costs_multiplier_partfn(row_after_curr_mon, mCost_numeric)
            Overhead_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$part_churn, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$part_churn == "Custom"){
            part_live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <-
              part_live$data$`Custom client churn`[row_after_curr_mon]
            Revenue_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Revenue_percent_change_partfn(row_after_curr_mon)
            Total_monthly_ARPA_partfn(row_after_curr_mon)
            Client_growth_after_churn_partfn(row_after_curr_mon)
            CLTV_partfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Actual_labor_costs_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Gross_profit_partfn(row_after_curr_mon)
            Gross_margins_partfn(row_after_curr_mon)
            Total_partner_pay_partfn(row_after_curr_mon)
            Sales_and_marketing_costs_partfn(row_after_curr_mon)
            
            Composite_costs_multiplier_partfn(row_after_curr_mon, 
                                              part_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          else {
            churn_numeric = as.numeric(gsub("[\\%,]", "", input$part_churn))
            churn_numeric = churn_numeric / 100
            part_live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <- churn_numeric
            Revenue_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Revenue_percent_change_partfn(row_after_curr_mon)
            Total_monthly_ARPA_partfn(row_after_curr_mon)
            Client_growth_after_churn_partfn(row_after_curr_mon)
            CLTV_partfn(row_after_curr_mon)
            CLTV_to_CAC_ratio_partfn(row_after_curr_mon)
            Company_head_count_partfn(row_after_curr_mon)
            Actual_labor_costs_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Gross_profit_partfn(row_after_curr_mon)
            Gross_margins_partfn(row_after_curr_mon)
            Total_partner_pay_partfn(row_after_curr_mon)
            Sales_and_marketing_costs_partfn(row_after_curr_mon)
            
            Composite_costs_multiplier_partfn(row_after_curr_mon, part_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Overhead_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$part_partbox, {
    if(input$part_partbox == "Linear"){
      updateSliderTextInput(session,
                            inputId = "part_Partners",
                            choices = c("Custom","1","2","3","4","5","6","7","8","9","10"),
                            selected = "Custom"
      )
    }
    else {
      updateSliderTextInput(session,
                            inputId = "part_Partners",
                            choices = c("Custom", "0.1%", "0.5%", "1%", "2%", "3%", "5%", "7.5%", "10%", "15%", "20%", "25%", "50%"),
                            selected = "Custom"
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$part_Partners, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$part_Partners == "Custom"){
            part_live$data$`Number of partners`[row_after_curr_mon] <- part_live$data$`Custom number of partners`[row_after_curr_mon]
            Composite_costs_multiplier_partfn(row_after_curr_mon, 
                                              part_live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Company_head_count_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Total_partner_pay_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          else if(input$part_partbox == "Linear"){
            part_numeric = as.numeric(input$part_Partners)
            part_live$data$`Number of partners`[row_after_curr_mon] <- (part_live$data$`Number of partners`[row_after_curr_mon - 1] + 
                                                                          part_numeric)
            Composite_costs_multiplier_partfn(row_after_curr_mon, part_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Total_partner_pay_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          else if(input$part_partbox == "Exponential"){
            part_per_numeric = as.numeric(gsub("[\\%,]", "", input$part_Partners))
            part_per_numeric = part_per_numeric / 100
            part_live$data$`Number of partners`[row_after_curr_mon] <-
              round((part_live$data$`Number of partners`[row_after_curr_mon - 1] * (1 + part_per_numeric)), 0)
            Composite_costs_multiplier_partfn(row_after_curr_mon, part_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Total_partner_pay_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$part_auto, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$part_auto == "Custom"){
            part_live$data$`Automation multiplier`[row_after_curr_mon] <- part_live$data$`Custom automation multiplier`[row_after_curr_mon]
            part_live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 
              part_live$data$`Automation multiplier`[row_after_curr_mon]
            Composite_costs_multiplier_partfn(row_after_curr_mon, part_live$data$`Overhead to revenue growth ratio`[row_after_curr_mon])
            Company_head_count_partfn(row_after_curr_mon)
            Actual_labor_costs_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Gross_margins_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          else {
            auto_numeric = as.numeric(gsub("[\\%,]", "", input$part_auto))
            auto_numeric = auto_numeric / 100
            part_live$data$`Automation multiplier`[row_after_curr_mon] <- part_live$data$`Automation multiplier`[row_after_curr_mon - 1] * 
              (1 + auto_numeric)
            part_live$data$`Reduction in agent task completion times relative to price benchmark`[row_after_curr_mon] <- 1 / 
              part_live$data$`Automation multiplier`[row_after_curr_mon]
            Composite_costs_multiplier_partfn(row_after_curr_mon, part_live$data$`Overhead to revenue ratio`[row_after_curr_mon])
            Company_head_count_partfn(row_after_curr_mon)
            Actual_labor_costs_partfn(row_after_curr_mon)
            Revenue_per_head_partfn(row_after_curr_mon)
            Overhead_to_opex_partfn(row_after_curr_mon)
            Gross_margins_partfn(row_after_curr_mon)
            Net_profit_partfn(row_after_curr_mon)
            Net_margins_partfn(row_after_curr_mon)
            Cash_in_bank_partfn(row_after_curr_mon)
          }
          row_after_curr_mon = row_after_curr_mon + 1
        }
      }
      i = i + 1
    }
  }, ignoreInit = TRUE)
  
  
  
  
  ##############################################Labor Costs #######################################################################
  output$part_Lcost <- renderGvis({
    df=data.frame(Month = part_live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Total_Labor_Costs = part_live$data$`Actual labor costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Operator_Labor_Costs = 
                    part_live$data$`Expected operator labor costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  RRR_Labor_Costs = 
                    part_live$data$`Expected RRR labor costs for assistants`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Specialist_Labor_Costs = 
                    part_live$data$`Expected specialist labor costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Strategist_Labor_Costs = 
                    part_live$data$`Expected strategist labor costs`[input$part_moSlider[1]:input$part_moSlider[2]]
    )
    gvisComboChart(df, xvar="Month",
                   yvar=c("Total_Labor_Costs","Operator_Labor_Costs", "RRR_Labor_Costs", 
                          "Specialist_Labor_Costs", "Strategist_Labor_Costs"),
                   options=list(pointSize = 3, seriesType="line",
                                series="[{type:'bars', 
                                targetAxisIndex:0,
                                color:'b8e986'}, 
                                {type:'line', 
                                targetAxisIndex:1,
                                color:'8497e5'},
                                {color:'grey'}, {color:'black'}]",
                                vAxes="[{title:'Dollars($)'}, {title:'Dollars($)'}]", 
                                width = 800, height = 400, title = "Labor Costs Breakdown", hAxis="{title:'Months'}"))
  })
  ######################################Revenue vs Labor Costs#########################################
  
  output$part_rev_vs_lab <- renderGvis({
    df=data.frame(Revenue = part_live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]], 
                  Total_Labor_Costs = part_live$data$`Actual labor costs`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'b8e986'}]", title = "Revenue Vs. Labor Costs"))
  })
  
  
  ############################Partner pay########################################################################################
  #partner pay visual
  output$part_combo <- renderGvis({
    df=data.frame(Month = part_live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Partner_Pay = part_live$data$`Total partner pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profit = part_live$data$`Gross profit`[input$part_moSlider[1]:input$part_moSlider[2]])
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
    df=data.frame(Revenue = part_live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]], 
                  Partner_Pay = part_live$data$`Total partner pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profit = part_live$data$`Gross profit`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = part_live$data$`Net profit`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Black'}, {color:'Red'}]", title = "Partner Pay Vs. Revenue"))
  })
  
  ########################################Overhead Page##############################################################################
  #overhead visual
  output$part_overhead <-renderGvis({
    df=data.frame(Month = part_live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"],
                  Total_Overhead = part_live$data$Overhead[input$part_moSlider[1]:input$part_moSlider[2]],
                  BD_Costs= part_live$data$`BD costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  RD_Costs= part_live$data$`Total R and D costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Additional_Subscription_Costs= 
                    part_live$data$`Additional subscription costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Discretionary_Spending= part_live$data$`Discretionary spending`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisComboChart(df, xvar="Month", yvar=c("Total_Overhead", "BD_Costs", "RD_Costs","Additional_Subscription_Costs", 
                                            "Discretionary_Spending"),
                                     options=list(pointSize = 3, width = 800, height = 400, 
                                     vAxis="[{title:'Dollars($)'}, {title: 'Dollars($)'}]",
                                     hAxis="{title:'Months'}",
                                     seriesType="line",
                                     series = "[{type: 'bars', targetAxisIndex:1, color:'b8e986'},
                                     {type: 'Line',targetAxisIndex:0, color:'8497e5'},  
                                     {color:'grey'}, 
                                     {color: 'Black'}, 
                                     {color:'Red'}]", title = "Overhead Breakdown"))
  })
  
  #Ecoomies of Scale visual
  output$part_econScale <-renderGvis({
    df=data.frame(Revenue = part_live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  Overhead_to_Opex_ratio= part_live$data$`Overhead to opex`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color: '8497e5'}]", title = "Economies of Scale"))
  })
  
  #########################################MORE PAGE###############################################
  
  ########################################Functional mapping#######################################################
  output$funcmap <-renderGvis({
    df=data.frame(From = c(#'Avg_customer_lifetime_months',
                           'Total_monthly_ARPA',
                           'Total_monthly_ARPA',
                           'Revenue_percent_change',
                           'Revenue_percent_change',
                           'Month_over_month_revenue',
                           'Revenue',
                           'Revenue',
                           'Revenue',
                           'Enterprise Revenue',
                           'Enterprise Revenue',
                           'Enterprise Revenue',
                           'Enterprise Revenue',
                           'Small Business Revenue',
                           'Small Business Revenue',
                           'Small Business Revenue',
                           'Small Business Revenue',
                           'Personal Revenue',
                           'Personal Revenue',
                           'Personal Revenue',
                           'Personal Revenue',
                           'Total enterprise monthly operator fees',
                           'Total enterprise monthly assistant fees',
                           'Total enterprise monthly specialist fees',
                           'Total enterprise monthly strategist fees',
                           'Total small business monthly operator fees', 
                           'Total small business monthly assistant fees',
                           'Total small business monthly specialist fees',
                           'Total small business monthly strategist fees',
                           'Total personal monthly operator fees', 
                           'Total personal monthly assistant fees',
                           'Total personal monthly specialist fees',
                           'Total personal monthly strategist fees',
                           'Enterprise_monthly_strategist_hrs',
                           'Enterprise clients',
                           'Enterprise clients',
                           'Enterprise_monthly_strategist_hrs',
                           'Enterprise_monthly_specialist_hrs',
                           'Enterprise clients',
                           'Enterprise clients',
                           'Enterprise_monthly_specialist_hrs',
                           'Enterprise_monthly_assistant_hrs',
                           'Enterprise clients',
                           'Enterprise clients',
                           'Enterprise_monthly_assistant_hrs',
                           'Enterprise_monthly_operator_hrs',
                           'Enterprise clients',
                           'Enterprise clients',
                           'Enterprise_monthly_operator_hrs',
                           'Small Business monthly_strategist_hrs',
                           'Small Business clients',
                           'Small Business clients',
                           'Small Business monthly_strategist_hrs',
                           'Small Business monthly_specialist_hrs',
                           'Small Business clients',
                           'Small Business clients',
                           'Small Business monthly_specialist_hrs',
                           'Small Business monthly_assistant_hrs',
                           'Small Business clients',
                           'Small Business clients',
                           'Small Business monthly_assistant_hrs',
                           'Small Business monthly_operator_hrs',
                           'Small Business clients',
                           'Small Business clients',
                           'Personal monthly_operator_hrs',
                           'Personal monthly_strategist_hrs',
                           'Personal clients',
                           'Personal clients',
                           'Personal monthly_strategist_hrs',
                           'Personal monthly_specialist_hrs',
                           'Personal clients',
                           'Personal clients',
                           'Personal monthly_specialist_hrs',
                           'Personal monthly_assistant_hrs',
                           'Personal clients',
                           'Personal clients',
                           'Personal monthly_assistant_hrs',
                           'Personal monthly_operator_hrs',
                           'Personal clients',
                           'Personal clients',
                           'Personal monthly_operator_hrs',
                           'Total_clients',
                           'Total_clients',
                           'Avg_customer_lifetime_months',
                           'CLTV',
                           'CLTV',
                           'CLTV',
                           'CLTV_to_CAC_ratio',
                           'CLTV_to_CAC_ratio',
                           'Company_head_count',
                           'Company_head_count',
                           'Number of Agents',
                              'Total_agents_not_including_set_managers',
                              'Total_agents_not_including_set_managers',
                              'Total_agents_not_including_set_managers',
                              'Total_agents_not_including_set_managers',
                                'Number_of_operators_invisible_decides_to_employ_this_month',
                                'Number_of_operators_invisible_decides_to_employ_this_month',
                                'Number_of_operators_invisible_decides_to_employ_this_month',
                                'Non_billable_operator_RRR_and_sentry_hrs',
                                'Non_billable_operator_RRR_and_sentry_hrs',
                                'Actual_client_hrs_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked',
                                'Actual_client_hrs_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked',
                                  'Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs',
                                  'Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs',
                                  'Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs',
                                'Number_of_RRRs_invisible_decides_to_employ_this_month',
                                'Number_of_RRRs_invisible_decides_to_employ_this_month',
                                  'Actual_client_hrs_RRRs_worked',
                                  'Actual_client_hrs_RRRs_worked',
                                    'Billable_RRR_hrs',
                                    'Billable_RRR_hrs',
                                    'Billable_RRR_hrs',
                                'Number_of_specialists_invisible_decides_to_employ_this_month',
                                'Number_of_specialists_invisible_decides_to_employ_this_month',
                                  'Actual_client_hrs_specialists_worked',
                                  'Actual_client_hrs_specialists_worked',
                                    'Billable_specialist_hrs',
                                    'Billable_specialist_hrs',
                                    'Billable_specialist_hrs',
                                'Number_of_strategists_invisible_decides_to_employ_this_month',
                                'Number_of_strategists_invisible_decides_to_employ_this_month',
                                  'Actual_client_hrs_strategists_worked',
                                  'Actual_client_hrs_strategists_worked',
                                    'Billable_strategist_hrs',
                                    'Billable_strategist_hrs',
                                    'Billable_strategist_hrs',
                           'Actual_labor_costs',
                           'Actual_labor_costs',
                           'Actual_labor_costs',
                           'Actual_labor_costs',
                              'Expected_operator_labor_costs',
                              'Expected_operator_labor_costs',
                              'Expected_operator_labor_costs',
                              'Expected_RRR_labor_costs_for_assistants',
                              'Expected_RRR_labor_costs_for_assistants',
                              'Expected_specialist_labor_costs',
                              'Expected_specialist_labor_costs',
                              'Expected_strategist_labor_costs',
                              'Expected_strategist_labor_costs',
                           'Revenue_per_head',
                           'Revenue_per_head',
                           'Revenue_per_head',
                           'Gross_profit',
                           'Gross_profit',
                           'Gross_margins',
                           'Gross_margins',
                           'Total_partner_pay',
                           'Total_partner_pay',
                              'Avg_partner_salary',
                              'Avg_partner_salary',
                           'Sales_and_marketing_costs',
                           'Sales_and_marketing_costs',
                           'Sales_and_marketing_costs',
                           'Sales_and_marketing_costs',
                           'Sales_and_marketing_costs',
                           'Commissions',
                           'Commissions',
                           'Growth_software_costs',
                           'Advertising_costs',
                           'Sales_agents',
                           'Invisible_sales_processes',
                           'Additional_subscription_costs',
                           'Total_R_and_D_costs',
                           'BD_costs',
                           'Discretionary_spending',
                           'Overhead',
                           'Overhead',
                           'Overhead',
                           'Overhead',
                           'Overhead',
                           'Overhead_to_opex',
                           'Overhead_to_opex',
                           'Opex',
                           'Opex',
                           'Net_profit',
                           'Net_profit',
                           'Net_profit',
                           'Net_margins',
                           'Net_margins',
                           'Cash_in_bank',
                           'Cash_in_bank',
                           'Burn',
                           'Burn',
                           'Burn',
                           'Burn',
                           'Burn',
                           'Burn'
                           ),
                  To =   c(#'Churn',
                           'Revenue',
                           'Total_clients',
                           'Month_over_month_revenue',
                           'Revenue',
                           'Revenue',
                           'Enterprise Revenue',
                           'Small Business Revenue',
                           'Personal Revenue',
                             'Total enterprise monthly operator fees',
                             'Total enterprise monthly assistant fees',
                             'Total enterprise monthly specialist fees',
                             'Total enterprise monthly strategist fees',
                             'Total small business monthly operator fees', 
                             'Total small business monthly assistant fees',
                             'Total small business monthly specialist fees',
                             'Total small business monthly strategist fees',
                             'Total personal monthly operator fees', 
                             'Total personal monthly assistant fees',
                             'Total personal monthly specialist fees',
                             'Total personal monthly strategist fees',
                                'Enterprise_monthly_operator_hrs',
                                'Enterprise_monthly_assistant_hrs',
                                'Enterprise_monthly_specialist_hrs',
                                'Enterprise_monthly_strategist_hrs',
                                'Small Business monthly_operator_hrs',
                                'Small Business monthly_assistant_hrs',
                                'Small Business monthly_specialist_hrs',
                                'Small Business monthly_strategist_hrs',
                                'Personal monthly_operator_hrs',
                                'Personal monthly_assistant_hrs',
                                'Personal monthly_specialist_hrs',
                                'Personal monthly_strategist_hrs',
                                  'Enterprise clients',
                                    'Percent enterprise clients',
                                    'Total_clients',
                                  'Avg enterprise monthly strategist hrs',
                                  'Enterprise clients',
                                     'Percent enterprise clients',
                                     'Total_clients',
                                  'Avg enterprise monthly specialist hrs',
                                  'Enterprise clients',
                                    'Percent enterprise clients',
                                    'Total_clients',
                                  'Avg enterprise monthly assistant hrs',
                                  'Enterprise clients',
                                    'Percent enterprise clients',
                                    'Total_clients',
                                  'Avg enterprise monthly operator hrs',
                                  'Small Business clients',
                                    'Percent Small Business clients',
                                    'Total_clients',
                                 'Avg Small Business monthly strategist hrs',
                                 'Small Business clients',
                                    'Percent Small Business clients',
                                    'Total_clients',
                                 'Avg Small Business monthly specialist hrs',
                                 'Small Business clients',
                                    'Percent Small Business clients',
                                    'Total_clients',
                                 'Avg Small Business monthly assistant hrs',
                                 'Small Business clients',
                                    'Percent Small Business clients',
                                    'Total_clients',
                                 'Avg Small Business monthly operator hrs',
                                 'Personal clients',
                                    'Percent Personal clients',
                                    'Total_clients',
                                 'Avg Personal monthly strategist hrs',
                                 'Personal clients',
                                    'Percent Personal clients',
                                    'Total_clients',
                                 'Avg Personal monthly specialist hrs',
                                 'Personal clients',
                                    'Percent Personal clients',
                                    'Total_clients',
                                 'Avg Personal monthly assistant hrs',
                                 'Personal clients',
                                    'Percent Personal clients',
                                    'Total_clients',
                                 'Avg Personal monthly operator hrs',
                                    'Churn',
                                    'Client Growth',
                           'Churn',
                           'Total_monthly_ARPA',
                           'Avg_customer_lifetime_months',
                           'CAC',
                           'CLTV',
                           'CAC',
                           'Number of Partners',
                           'Number of Agents',
                           'Total_agents_not_including_set_managers',
                              'Number_of_operators_invisible_decides_to_employ_this_month',
                              'Number_of_RRRs_invisible_decides_to_employ_this_month',
                              'Number_of_specialists_invisible_decides_to_employ_this_month',
                              'Number_of_strategists_invisible_decides_to_employ_this_month',
                                'Actual_client_hrs_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked',
                                'Non_billable_operator_RRR_and_sentry_hrs',
                                'Avg hrs per week per operator',
                                  'Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs',
                                  'Percent of operator sentry and RRR hrs that are NOT client billable',
                                  'Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs',
                                  'Reduction in agent task completion times relative to price benchmark',
                                    'Enterprise_monthly_operator_hrs',
                                    'Small Business monthly_operator_hrs',
                                    'Personal monthly_operator_hrs',
                                'Actual_client_hrs_RRRs_worked',
                                'Avg hrs per week per RRR',
                                  'Billable_RRR_hrs',
                                  'Reduction in agent task completion times relative to price benchmark',
                                    'Enterprise_monthly_operator_hrs',
                                    'Small Business monthly_operator_hrs',
                                    'Personal monthly_operator_hrs',
                                'Actual_client_hrs_specialists_worked',
                                'Avg hrs per week per specialists',
                                  'Billable_specialist_hrs',
                                  'Reduction in agent task completion times relative to price benchmark',
                                    'Enterprise_monthly_operator_hrs',
                                    'Small Business monthly_operator_hrs',
                                    'Personal monthly_operator_hrs',
                                'Actual_client_hrs_strategists_worked',
                                'Avg hrs per week per strategists',
                                  'Billable_strategist_hrs',
                                  'Reduction in agent task completion times relative to price benchmark',
                                    'Enterprise_monthly_operator_hrs',
                                    'Small Business monthly_operator_hrs',
                                    'Personal monthly_operator_hrs',
                           'Expected_operator_labor_costs',
                           'Expected_RRR_labor_costs_for_assistants',
                           'Expected_specialist_labor_costs',
                           'Expected_strategist_labor_costs',
                              'Actual_client_hrs_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked',
                              'Non_billable_operator_RRR_and_sentry_hrs',
                              'Avg rate for operators sentries and RRRs working for clients at 10 dollars per hr',
                              'Actual_client_hrs_RRRs_worked',
                              'Avg RRR rate working for clients at 20 dollars per hr',
                              'Actual_client_hrs_specialists_worked',
                              'Avg specialist rate',
                              'Actual_client_hrs_strategists_worked',
                              'Avg strategist rate',
                           'Revenue',
                           'Number of Partners',
                           'Number of Agents',
                           'Revenue',
                           'Actual_labor_costs',
                           'Revenue',
                           'Gross_profit',
                           'Number of Partners',
                           'Avg_partner_salary',
                              'Avg salary cap per partner',
                              'Avg dollars shy of partner salary cap',
                           'Commissions',
                           'Growth_software_costs',
                           'Advertising_costs',
                           'Sales_agents',
                           'Invisible_sales_processes',
                              'Revenue',
                              'Percent Commission',
                              'Composite costs multiplier',
                              'Composite costs multiplier',
                              'Composite costs multiplier',
                              'Composite costs multiplier',
                              'Composite costs multiplier',
                              'Composite costs multiplier',
                              'Composite costs multiplier',
                              'Composite costs multiplier',
                           'Sales_and_marketing_costs',
                           'Additional_subscription_costs',
                           'Total_R_and_D_costs',
                           'BD_costs',
                           'Discretionary_spending',
                           'Overhead',
                           'Opex',
                              'Overhead',
                              'Actual_labor_costs',
                           'Gross_profit',
                           'Total_partner_pay',
                           'Burn',
                           'Revenue',
                           'Net_profit',
                           'Net_profit',
                           'Funds raised',
                           'Partner bonuses',
                           'Sales_and_marketing_costs',
                           'Additional_subscription_costs',
                           'Total_R_and_D_costs',
                           'BD_costs',
                           'Discretionary_spending'
                           
                  ),
                  Weight = c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                             3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                             3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                             3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3))
    gvisSankey(df, from = "From", to = "To", weight = "Weight", 
               options=list(width = 1800, height = 1000,
                 sankey="{link: {colorMode: 'gradient', color: { fill: '8497e5' } },
                 node: { color: { fill: 'b8e986' },
                 label: { color: 'Grey' } }}")
               )
  })
  #######################################Table##############################################
  
  output$table <-renderGvis({
    gvisTable(MRT$data, options = list(frozenColumns = 1))
  })
}
  