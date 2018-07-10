library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)


curr_date <- format(Sys.Date(), "%b '%y")

# Define server function required to create the scatterplot
server <- function(input, output) {
  source('sheets_data.R', local = TRUE)
  
  
  #reload data button. This creates MRT$data which is the data frame with the main data we use from google speadsheets. 
  MRT <- reactiveValues()
  live <- reactiveValues()
  observeEvent(input$reload, {
    MRT$data <- gs_read( gs_key("1KdOJJ9rj3eWUKA4F6mX-MIEQKe5XJAMeJf4aZvQ1m8A"), ws = "Frozen Shiny Sheet")
    live$data <- MRT$data
  }, ignoreNULL=FALSE)
  output$clics <- renderText(input$reload)
  ###########################################General PAGE#########################################################################
  #bottoom page table
  observeEvent(input$newTable, {
    output$main_table <-renderGvis({
      gvisTable(live$data)
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
              Commissions_fn(row_after_curr_mon)
              
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
              Commissions_fn(row_after_curr_mon)
              
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
  }, ignoreNULL=FALSE)


  #effects the slider for the cost multiplier slider
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
  }, ignoreNULL=FALSE)
  
  # observeEvent(input$churn, {
  #   i <- 1
  #   while(i <= nrow(MRT$data)){
  #     if(MRT$data$Month[i] == curr_date){
  #       row_after_curr_mon<-i+1
  #       while(row_after_curr_mon <= nrow(MRT$data)){
  #         if(input$churn == "Custom"){
  #           live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <- 
  #             live$data$`Custom client churn`[row_after_curr_mon]
  #           Revenue_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Revenue_percent_change_fn(row_after_curr_mon)
  #           Total_monthly_ARPA_fn(row_after_curr_mon)
  #           Client_growth_after_churn_fn(row_after_curr_mon)
  #           CLTV_fn(row_after_curr_mon)
  #           CLTV_to_CAC_ratio_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Actual_labor_costs_fn(row_after_curr_mon)
  #           Revenue_per_head_fn(row_after_curr_mon)
  #           Gross_profit_fn(row_after_curr_mon)
  #           Gross_margins_fn(row_after_curr_mon)
  #           Total_partner_pay_fn(row_after_curr_mon)
  #           Commissions_fn(row_after_curr_mon)
  #           
  #           Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
  #           Overhead_fn(row_after_curr_mon)
  #           Overhead_to_opex_fn(row_after_curr_mon)
  #           Net_profit_fn(row_after_curr_mon)
  #           Net_margins_fn(row_after_curr_mon)
  #           Cash_in_bank_fn(row_after_curr_mon)
  #         }
  #         else {
  #           churn_numeric = as.numeric(gsub("[\\%,]", "", input$churn))
  #           churn_numeric = churn_numeric / 100
  #           live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <- churn_numeric
  #           Revenue_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Revenue_percent_change_fn(row_after_curr_mon)
  #           Total_monthly_ARPA_fn(row_after_curr_mon)
  #           Client_growth_after_churn_fn(row_after_curr_mon)
  #           CLTV_fn(row_after_curr_mon)
  #           CLTV_to_CAC_ratio_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Actual_labor_costs_fn(row_after_curr_mon)
  #           Revenue_per_head_fn(row_after_curr_mon)
  #           Gross_profit_fn(row_after_curr_mon)
  #           Gross_margins_fn(row_after_curr_mon)
  #           Total_partner_pay_fn(row_after_curr_mon)
  #           Commissions_fn(row_after_curr_mon)
  #           
  #           Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
  #           Overhead_fn(row_after_curr_mon)
  #           Overhead_to_opex_fn(row_after_curr_mon)
  #           Net_profit_fn(row_after_curr_mon)
  #           Net_margins_fn(row_after_curr_mon)
  #           Cash_in_bank_fn(row_after_curr_mon)
  #         }
  #         row_after_curr_mon = row_after_curr_mon + 1
  #       }
  #     }
  #     i = i + 1
  #   }
  # }, ignoreNULL=FALSE)

  #####################################Profit Page####################################################################################
  #profit visual
  output$profit <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Gross_Profit = live$data$`Gross profit`[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit = live$data$`Net profit`[input$moSlider[1]:input$moSlider[2]])
    gvisColumnChart(df, xvar = "Month", 
                    yvar = c("Gross_Profit", "Net_Profit"),
                    options=list(bar="{groupWidth:'70%'}", width = "800", height = "400",
                                 isStacked = TRUE, vAxis="{title:'Dollars($)'}",
                                 hAxis="{title:'Months'}", title = "Profit",
                                 series = "[{color:'8497e5'}, {color:'b8e986'}]"))
  })
  #gross/ net margins visual
  output$gross <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Gross_Margins= live$data$`Gross margins`[input$moSlider[1]:input$moSlider[2]],
                  Net_Margins= live$data$`Net margins`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit Margins'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}]", 
                                   title = "Margins(%)"))
  })
  
  ################################Runway page########################################################################################
  #runway visual
  output$runway <- renderGvis({
    df=data.frame(Month= live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Cash_in_Bank= live$data$`Cash in bank`[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit = live$data$`Net profit`[input$moSlider[1]:input$moSlider[2]],
                  Funds_Raised = live$data$`Cumulative funds raised`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize=6,width = 800, height = 400,vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", title = "Runway", 
                                   series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]"))
  })
  
  #Profit and Revenue visual
  output$profRev <-renderGvis({
    df=data.frame(Revenue= live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Overhead= live$data$Overhead[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profit= live$data$`Gross profit`[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit= live$data$`Net profit`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]", 
                                   title = "Profit/Revenue"))
  })
  
  ###########################Growth Page##############################################################################################
  #client growth and revenue visual
  output$num_client <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Revenue = live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Enterprise_Clients = live$data$`Enterprise clients`[input$moSlider[1]:input$moSlider[2]],
                  Small_Business_Clients = live$data$`Small business clients`[input$moSlider[1]:input$moSlider[2]],
                  Personal_Clients = live$data$`Personal clients`[input$moSlider[1]:input$moSlider[2]])
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
  output$rev_client <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Revenue = live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Enterprise_Revenue = live$data$`Enterprise revenue`[input$moSlider[1]:input$moSlider[2]],
                  Small_Business_Revenue = live$data$`Small business revenue`[input$moSlider[1]:input$moSlider[2]],
                  Personal_Revenue = live$data$`Personal revenue`[input$moSlider[1]:input$moSlider[2]])
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
  
  #######################################churn page#################################################################################
  #churn vs client growth visual
  output$client <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Churn = live$data$`Churn percentage weighted by number of clients`[input$moSlider[1]:input$moSlider[2]],
                  Client_Growth = live$data$`Client growth percentage`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Percent'}",
                                   hAxis="{title:'Months'}", title = "Churn Vs Client Growth", 
                                   series = "[{color:'8497e5'}, {color: 'b8e986'}]"))
  })
  
  
  ############################Workforce Page##########################################################################################
  
  #Revenue per head visual
  output$workForce <-renderGvis({
    df=data.frame(Revenue= live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  Company_Head_Count= live$data$`Company head count`[input$moSlider[1]:input$moSlider[2]],
                  Revenue_per_Employee= live$data$`Revenue per head`[input$moSlider[1]:input$moSlider[2]],
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
  
  #Labor costs Visual
  output$Lcost <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Actual_Labor_Cost = live$data$`Actual labor costs`[input$moSlider[1]:input$moSlider[2]],
                  Expected_Operator_Labor_cost = live$data$`Expected operator labor costs`[input$moSlider[1]:input$moSlider[2]],
                  Expected_RRR_Labor_Cost = live$data$`Expected RRR labor costs for assistants`[input$moSlider[1]:input$moSlider[2]],
                  Expected_Specialist_and_Strategist_Labor_Costs = 
                    live$data$`Expected specialist and strategist labor costs`[input$moSlider[1]:input$moSlider[2]]
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
  output$combo <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Partner_Pay = live$data$`Total partner pay`[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profits = live$data$`Gross profit`[input$moSlider[1]:input$moSlider[2]])
    gvisComboChart(df, xvar = "Month", 
                   yvar = c("Partner_Pay", "Gross_Profits"),
                   options=list(seriesType="bars",
                                series="[{type:'line', color: '8497e5'},
                                {type:'bars', color: 'b8e986'}]", 
                                width = 800, height = 400, vAxis="{title:'Dollars($)'}", pointSize = 4,
                                hAxis="{title:'Months'}", title = "Partner Pay"))
  })
  
  #partner pay comapred to revenue visual
  output$linechart <- renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$moSlider[1]:input$moSlider[2]], 
                  Partner_Pay = live$data$`Total partner pay`[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profits = live$data$`Gross profit`[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit = live$data$`Net profit`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Black'}, {color:'Red'}]", title = "Partner Pay Compared to Revenue"))
  })
  
  ########################################Overhead Page##############################################################################
  #overhead visual
  output$overhead <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"],
                  Total_Overhead = live$data$Overhead[input$moSlider[1]:input$moSlider[2]],
                  BD_Costs = live$data$`BD costs`[input$moSlider[1]:input$moSlider[2]],
                  RD_Costs = live$data$`Total R and D costs`[input$moSlider[1]:input$moSlider[2]],
                  Subscription_Costs = live$data$`Subscription costs`[input$moSlider[1]:input$moSlider[2]],
                  Discretionary_Spending = live$data$`Discretionary spending`[input$moSlider[1]:input$moSlider[2]],
                  Partner_Pay = live$data$`Total partner pay`[input$moSlider[1]:input$moSlider[2]],
                  Partner_Bonuses = live$data$`Partner bonuses`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Red'}]", title = "Overhead"))
  })
  
  #Ecoomies of Scale visual
  output$econScale <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$moSlider[1]:input$moSlider[2]],
                  `Overhead/Opex %`= live$data$`Overhead to opex`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Overhead/Opex %'}",
                                   hAxis="{title:'Revenue'}", series = "[{color: '8497e5'}]", title = "Economies of Scale"))
  })
  
  
  # #######################################other################################################################################
  # #gross margins visual
  # output$viz1 <-renderGvis({
  #   df=data.frame(Month= live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
  #                 Gross_Margins= live$data$`Gross margins`[input$moSlider[1]:input$moSlider[2]])
  #   gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Perecent of Dollars($)'}",
  #                                  hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Gross Margins"))
  # })
  # #amount of partners visual
  # output$viz2 <-renderGvis({
  #   df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
  #                 Partner = live$data$`Number of partners`[input$moSlider[1]:input$moSlider[2]])
  #   gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'# of Partners'}",
  #                                  hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Amount of Partners"))
  # })
  # #partner bonuses visual
  # output$viz3 <-renderGvis({
  #   df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
  #                 Partner_Bonuses = live$data$`Partner bonuses`[input$moSlider[1]:input$moSlider[2]])
  #   gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
  #                                  hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Partner Bonues"))
  # })
  # #comissons visual
  # output$viz4 <-renderGvis({
  #   df=data.frame(Month= live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
  #                 Commissions = live$data$Commissions[input$moSlider[1]:input$moSlider[2]])
  #   gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
  #                                  hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Comissions"))
  # })
  # #total fixed costs visual
  # output$viz5 <-renderGvis({
  #   df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
  #                 Subscription_Costs = live$data$`Subscription costs`[input$moSlider[1]:input$moSlider[2]])
  #   gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
  #                                  hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Subscription Costs"))
  # })
  # #R&D costs Visual
  # output$viz6 <-renderGvis({
  #   df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
  #                 RD_Costs = live$data$`Total R and D costs`[input$moSlider[1]:input$moSlider[2]])
  #   gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
  #                                  hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "R&D Costs"))
  # })
  
  
  
  
  
  
  ############################################Growth Page############################################################################
  #bottoom page table
  
    output$grow_main_table <-renderGvis({
      gvisTable(live$data)
    })
  
  
  #Effects the slider for the client growth slider
  
  observeEvent(input$grow_growslide, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$grow_growslide == "Custom"){
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
            Commissions_fn(row_after_curr_mon)
            
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          else {
            growslide_numeric = as.numeric(gsub("[\\%,]", "", input$grow_growslide))
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
            Commissions_fn(row_after_curr_mon)
            
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
  }, ignoreNULL=FALSE)
  
  
  #effects the slider for the cost multiplier slider
  observeEvent(input$grow_mCost, {
    i <- 1
    while(i <= nrow(MRT$data)){
      if(MRT$data$Month[i] == curr_date){
        row_after_curr_mon<-i+1
        while(row_after_curr_mon <= nrow(MRT$data)){
          if(input$grow_mCost == "Custom"){
            Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
            Overhead_fn(row_after_curr_mon)
            Overhead_to_opex_fn(row_after_curr_mon)
            Net_profit_fn(row_after_curr_mon)
            Net_margins_fn(row_after_curr_mon)
            Cash_in_bank_fn(row_after_curr_mon)
          }
          else{
            mCost_numeric = as.numeric(gsub("[\\%,]", "", input$grow_mCost))
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
  }, ignoreNULL=FALSE)
  
  # observeEvent(input$churn, {
  #   i <- 1
  #   while(i <= nrow(MRT$data)){
  #     if(MRT$data$Month[i] == curr_date){
  #       row_after_curr_mon<-i+1
  #       while(row_after_curr_mon <= nrow(MRT$data)){
  #         if(input$churn == "Custom"){
  #           live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <- 
  #             live$data$`Custom client churn`[row_after_curr_mon]
  #           Revenue_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Revenue_percent_change_fn(row_after_curr_mon)
  #           Total_monthly_ARPA_fn(row_after_curr_mon)
  #           Client_growth_after_churn_fn(row_after_curr_mon)
  #           CLTV_fn(row_after_curr_mon)
  #           CLTV_to_CAC_ratio_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Actual_labor_costs_fn(row_after_curr_mon)
  #           Revenue_per_head_fn(row_after_curr_mon)
  #           Gross_profit_fn(row_after_curr_mon)
  #           Gross_margins_fn(row_after_curr_mon)
  #           Total_partner_pay_fn(row_after_curr_mon)
  #           Commissions_fn(row_after_curr_mon)
  #           
  #           Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Custom overhead to revenue growth ratio`[row_after_curr_mon])
  #           Overhead_fn(row_after_curr_mon)
  #           Overhead_to_opex_fn(row_after_curr_mon)
  #           Net_profit_fn(row_after_curr_mon)
  #           Net_margins_fn(row_after_curr_mon)
  #           Cash_in_bank_fn(row_after_curr_mon)
  #         }
  #         else {
  #           churn_numeric = as.numeric(gsub("[\\%,]", "", input$churn))
  #           churn_numeric = churn_numeric / 100
  #           live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] <- churn_numeric
  #           Revenue_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Revenue_percent_change_fn(row_after_curr_mon)
  #           Total_monthly_ARPA_fn(row_after_curr_mon)
  #           Client_growth_after_churn_fn(row_after_curr_mon)
  #           CLTV_fn(row_after_curr_mon)
  #           CLTV_to_CAC_ratio_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Actual_labor_costs_fn(row_after_curr_mon)
  #           Revenue_per_head_fn(row_after_curr_mon)
  #           Gross_profit_fn(row_after_curr_mon)
  #           Gross_margins_fn(row_after_curr_mon)
  #           Total_partner_pay_fn(row_after_curr_mon)
  #           Commissions_fn(row_after_curr_mon)
  #           
  #           Composite_costs_multiplier_fn(row_after_curr_mon, live$data$`Overhead to revenue ratio`[row_after_curr_mon])
  #           Overhead_fn(row_after_curr_mon)
  #           Overhead_to_opex_fn(row_after_curr_mon)
  #           Net_profit_fn(row_after_curr_mon)
  #           Net_margins_fn(row_after_curr_mon)
  #           Cash_in_bank_fn(row_after_curr_mon)
  #         }
  #         row_after_curr_mon = row_after_curr_mon + 1
  #       }
  #     }
  #     i = i + 1
  #   }
  # }, ignoreNULL=FALSE)
  
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
  #bottom page table
    output$part_main_table <-renderGvis({
      gvisTable(live$data)
    })
  
  #Effects the slider for the client growth slider
  
  # observeEvent(input$part_growslide, {
  #   i <- 1
  #   while(i <= nrow(MRT$data)){
  #     if(MRT$data$Month[i] == curr_date){
  #       row_after_curr_mon<-i+1
  #       while(row_after_curr_mon <= nrow(MRT$data)){
  #         if(input$part_growslide == "Custom"){
  #           live$data <- MRT$data
  #         }
  #         else {
  #           growslide_numeric = as.numeric(gsub("[\\%,]", "", input$part_growslide))
  #           growslide_numeric = growslide_numeric / 100
  #           live$data$`Client growth percentage`[row_after_curr_mon] <- growslide_numeric
  #           Revenue_fn(row_after_curr_mon)
  #           Company_head_count_fn(row_after_curr_mon)
  #           Actual_labor_costs_fn(row_after_curr_mon)
  #           Total_partner_pay_fn(row_after_curr_mon)
  #           Revenue_percent_change_fn(row_after_curr_mon)
  #           Total_monthly_ARPA_fn(row_after_curr_mon)
  #           Client_growth_after_churn_fn(row_after_curr_mon)
  #           CLTV_fn(row_after_curr_mon)
  #           CLTV_to_CAC_ratio_fn(row_after_curr_mon)
  #           Revenue_per_head_fn(row_after_curr_mon)
  #           Gross_profit_fn(row_after_curr_mon)
  #           Gross_margins_fn(row_after_curr_mon)
  #           Commissions_fn(row_after_curr_mon)
  #         }
  #         row_after_curr_mon = row_after_curr_mon + 1
  #       }
  #     }
  #     i = i + 1
  #   }
  # }, ignoreNULL=FALSE)
  # 
  # 
  # #effects the slider for the cost multiplier slider
  # observeEvent(input$part_mCost, {
  #   i <- 1
  #   while(i <= nrow(MRT$data)){
  #     if(MRT$data$Month[i] == curr_date){
  #       row_after_curr_mon<-i+1
  #       while(row_after_curr_mon <= nrow(MRT$data)){
  #         if(input$part_mCost == "Custom"){
  #           live$data = MRT$data
  #         }
  #         else{
  #           mCost_numeric = as.numeric(gsub("[\\%,]", "", input$part_mCost))
  #           mCost_numeric = mCost_numeric / 100
  #           Composite_costs_multiplier_fn(row_after_curr_mon, mCost_numeric)
  #           Overhead_fn(row_after_curr_mon)
  #           Overhead_to_opex_fn(row_after_curr_mon)
  #           Burn_fn(row_after_curr_mon)
  #           Net_profit_fn(row_after_curr_mon)
  #           Net_margins_fn(row_after_curr_mon)
  #           Cash_in_bank_fn(row_after_curr_mon)
  #         }
  #         row_after_curr_mon = row_after_curr_mon + 1
  #       }
  #     }
  #     i = i + 1
  #   }
  # }, ignoreNULL=FALSE)
  

  ##############################################Labor Costs #######################################################################
  output$part_Lcost <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Actual_Labor_Cost = live$data$`Actual labor costs`[input$moSlider[1]:input$moSlider[2]],
                  Expected_Operator_Labor_cost = live$data$`Expected operator labor costs`[input$moSlider[1]:input$moSlider[2]],
                  Expected_RRR_Labor_Cost = live$data$`Expected RRR labor costs for assistants`[input$moSlider[1]:input$moSlider[2]],
                  Expected_Specialist_and_Strategist_Labor_Costs = 
                    live$data$`Expected specialist and strategist labor costs`[input$moSlider[1]:input$moSlider[2]]
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
                  Subscription_Costs= live$data$`Subscription costs`[input$part_moSlider[1]:input$part_moSlider[2]],
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
                  `Overhead/Opex %`= live$data$`Overhead to opex`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Overhead/Opex %'}",
                                   hAxis="{title:'Revenue'}", series = "[{color: '8497e5'}]", title = "Economies of Scale"))
  })
  
  #########################################MORE PAGE###############################################
  
  #######################################spreadsheet##############################################
  
  output$table <-renderGvis({
    gvisTable(live$data)
  })

  }
