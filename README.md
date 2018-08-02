# TheModel
Invisible Technology's Financial Projections Model

This is where important information about TheModel3.0 can be written.

gs_auth
gs_ls() # not neccessary
gs_mock_runway_title <- gs_title("Mock Runway Table")
gs_MRT <- gs_read(gs_mock_runway_title, ws=1)



x = -((row_after_curr_mon - 11)^(.9))
            growslide_numeric = (((100 * live$data$`Churn percentage weighted by number of clients`[row_after_curr_mon] * x) - 
                                    const_growslide_numeric) / (x - 1)) / 100



operators_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Operators))
RRR_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$RRRs))
sentries_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Sentries))
strategists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Strategists))
specialists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Specialists))
Agent_Labor_cost_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Agent Labor Costs`))

df=data.frame(Month = gs_MRT[,1], 
              Operator_Labor_cost = operators_numeric[1:nrow(gs_MRT)],
              #RRR_Labor_Cost = RRR_numeric[1:nrow(gs_MRT)],
              Sentry_Labor_Cost = sentries_numeric[1:nrow(gs_MRT)])#,
#Strategists_Labor_Cost = strategists_numeric[1:nrow(gs_MRT)],
#Specialists_Labor_Cost = specialists_numeric[1:nrow(gs_MRT)],
#Agent_Labor_Cost = Agent_Labor_cost_numeric[1:nrow(gs_MRT)])


tabPanel("Client Growth",
         fluidPage(
           fluidRow(
             column(6, htmlOutput("num_client")), 
             column(6, htmlOutput("rev_client"))),
           fluidRow( htmlOutput("client")))),
           
           
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

# Define UI for application that plots features of movies 
ui <- fluidPage(theme = shinytheme("sandstone"),
         #top bar
         navbarPage("Invisible Technologies",
              tabPanel("General",  #general page
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
                                    #############date slider
                                    # sliderInput(inputId = "moSlider_date",
                                    #             label = "Month Range:",
                                    #             min = as.Date("2018-05-01"),
                                    #             max =as.Date("2019-11-01"),
                                    #             value=c(as.Date("2018-12-01"), as.Date("2019-09-01")),
                                    #             timeFormat="%b %Y",
                                    #             animate = TRUE
                                    #             ),
                              
                                    h3("Current Date: ", curr_date),
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
                                      selected = "25%" 
                                    ),
                                    #hr(style="border-color: black;"),
                              
                                    #slider for Churn
                                    sliderTextInput(
                                      inputId = "churn",
                                      label = "Monthly Churn:",
                                      grid = TRUE,
                                      choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%","14%",
                                                  "15%","16%",
                                                  "17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%","29%","30%",
                                                  "31%","32%","33%","34%","35%","36%","37%","38%","39%","40%"),
                                      selected = "Custom" 
                                    ),
                              
                                    
                                    # #slider for Cost Multiplier
                                    # sliderTextInput(
                                    #   inputId = "Gross Margins",
                                    #   label = "Gross Margins:(UNDER CONSTRUCTION)",
                                    #   grid = TRUE,
                                    #   choices = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                                    #               "110%","125%","150%","200%"),
                                    #   selected = "30%" 
                                    # ),
                                    # 
                                    # sliderInput(inputId = "cacslide",
                                    #             label = "CAC Slider:(UNDER CONSTRUCTION)",
                                    #             step = 1,
                                    #             min = 1,
                                    #             max = 72,
                                    #             value=c(5,24),
                                    #             animate = TRUE
                                    # ),
                                    # 
                                    # sliderInput(inputId = "ltvslide",
                                    #             label = "LTV Slider:(UNDER CONSTRUCTION)",
                                    #             step = 1,
                                    #             min = 1,
                                    #             max = 36,
                                    #             value=c(4,12),
                                    #             animate = TRUE
                                    # ),
                                    # sliderInput(inputId = "partnerbonus",
                                    #             label = "Partner Bonuses:(UNDER CONSTRUCTION)",
                                    #             step = 1,
                                    #             min = 1,
                                    #             max = 36,
                                    #             value=c(4,12),
                                    #             animate = TRUE
                                    # ),
                                    # hr(),
                                    # 
                                    # checkboxGroupInput(inputId = "growthbox",
                                    #                    label = "Growth Options:(UNDER CONSTRUCTION)",
                                    #                    choices = c("Linear",
                                    #                                "Exponential"),
                                    #                    selected = c("Type")),
                                    # #check box for random options
                                    # checkboxGroupInput(inputId = "Options",
                                    #                    label = "Other Options:(UNDER CONSTRUCTION)",
                                    #                    choices = c("Include Agent Guarantee of 30 hrs/mo",
                                    #                                "Monte Carlo Simulation",
                                    #                                "Include Varience in Projetions"),
                                    #                    selected = c("Type")),
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
                                                tabPanel("Profit", htmlOutput("profit"), htmlOutput("gross")),
                                                tabPanel("Runway", htmlOutput("runway"), htmlOutput("profRev")),
                                                tabPanel("Growth",htmlOutput("num_client"), htmlOutput("rev_client")),      
                                                tabPanel("Churn", htmlOutput("client")),
                                                tabPanel("Workforce", htmlOutput("Lcost"), htmlOutput("workForce")),
                                                tabPanel("Partner Pay", htmlOutput("combo"), htmlOutput("linechart")),
                                                tabPanel("Overhead", htmlOutput("overhead"), htmlOutput("econScale"))
                                               
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
            
#############################################Growth page###########################################################################  
            tabPanel("Growth",  #general page
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
                                     sliderInput(inputId = "grow_moSlider",
                                                 label = "Month Range:",
                                                 step = 1,
                                                 min = 1,
                                                 max = 72,
                                                 value=c(5,24),
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
                                       selected = "25%" 
                                     ),
                                     #hr(style="border-color: black;"),
                                     
                                     #slider for Churn
                                     sliderTextInput(
                                       inputId = "grow_churn",
                                       label = "Monthly Churn:",
                                       grid = TRUE,
                                       choices = c("Custom","1%","2%","3%","4%","5%","6%","7%","8%","9%","10%","11%","12%","13%",
                                                   "14%","15%","16%",
                                                   "17%","18%","19%","20%","21%", "22%", "23%", "24%","25%","26%","27%","28%","29%","30%",
                                                   "31%","32%","33%","34%","35%","36%","37%","38%","39%","40%"),
                                       selected = "Custom" 
                                     ),
                                     
                                     
                                     # #slider for Cost Multiplier
                                     # sliderTextInput(
                                     #   inputId = "Gross Margins",
                                     #   label = "Gross Margins:(UNDER CONSTRUCTION)",
                                     #   grid = TRUE,
                                     #   choices = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                                     #               "110%","125%","150%","200%"),
                                     #   selected = "30%" 
                                     # ),
                                     # 
                                     # sliderInput(inputId = "cacslide",
                                     #             label = "CAC Slider:(UNDER CONSTRUCTION)",
                                     #             step = 1,
                                     #             min = 1,
                                     #             max = 72,
                                     #             value=c(5,24),
                                     #             animate = TRUE
                                     # ),
                                     # 
                                     # sliderInput(inputId = "ltvslide",
                                     #             label = "LTV Slider:(UNDER CONSTRUCTION)",
                                     #             step = 1,
                                     #             min = 1,
                                     #             max = 36,
                                     #             value=c(4,12),
                                     #             animate = TRUE
                                     # ),
                                     # sliderInput(inputId = "partnerbonus",
                                     #             label = "Partner Bonuses:(UNDER CONSTRUCTION)",
                                     #             step = 1,
                                     #             min = 1,
                                     #             max = 36,
                                     #             value=c(4,12),
                                     #             animate = TRUE
                                     # ),
                                     # hr(),
                                     # 
                                     # checkboxGroupInput(inputId = "growthbox",
                                     #                    label = "Growth Options:(UNDER CONSTRUCTION)",
                                     #                    choices = c("Linear",
                                     #                                "Exponential"),
                                     #                    selected = c("Type")),
                                     # #check box for random options
                                     # checkboxGroupInput(inputId = "Options",
                                     #                    label = "Other Options:(UNDER CONSTRUCTION)",
                                     #                    choices = c("Include Agent Guarantee of 30 hrs/mo",
                                     #                                "Monte Carlo Simulation",
                                     #                                "Include Varience in Projetions"),
                                     #                    selected = c("Type")),
                                     h6("* Costs multiplier refers to the percentage that Business Development Costs, 
                                        Subscription Costs, R&D Costs, 
                                        and Sales/Marketing Costs increase at relative to revenue."),
                                     
                                     #reload data clicker
                                     fluidRow(
                                       column(6,
                                              actionButton(inputId = "grow_reload", label = "Reload data"),
                                              textOutput('clics')
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
                                                 tabPanel("Client Growth",htmlOutput("grow_num_client"), htmlOutput("grow_rev_client"))
                                                 
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
                              ###############date slider
                              # sliderInput(inputId = "moSlider",
                              #             label = "Month Range:",
                              #             min = as.Date("2018-05-01"),
                              #             max =as.Date("2019-11-01"),
                              #             value=c(as.Date("2018-12-01"), as.Date("2019-09-01")),
                              #             timeFormat="%b %Y",
                              #             animate = TRUE
                              #             ),
                              h3("Current Date: ", curr_date),
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
                                choices = c("Custom","10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                                            "110%","125%","150%","200%"),
                                selected = "Custom" 
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
                              
                              # #slider for Cost Multiplier
                              # sliderTextInput(
                              #   inputId = "part_gross_margins",
                              #   label = "Gross Margins:(UNDER CONSTRUCTION)",
                              #   grid = TRUE,
                              #   choices = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%","100%",
                              #               "110%","125%","150%","200%"),
                              #   selected = "30%" 
                              # ),
                              # 
                              # sliderInput(inputId = "part_cacslide",
                              #             label = "CAC Slider:(UNDER CONSTRUCTION)",
                              #             step = 1,
                              #             min = 1,
                              #             max = 36,
                              #             value=c(4,12),
                              #             animate = TRUE
                              # ),
                              # 
                              # sliderInput(inputId = "part_ltvslide",
                              #             label = "LTV Slider:(UNDER CONSTRUCTION)",
                              #             step = 1,
                              #             min = 1,
                              #             max = 36,
                              #             value=c(4,12),
                              #             animate = TRUE
                              # ),
                              # sliderInput(inputId = "part_partnerbonus",
                              #             label = "Partner Bonuses:(UNDER CONSTRUCTION)",
                              #             step = 1,
                              #             min = 1,
                              #             max = 36,
                              #             value=c(4,12),
                              #             animate = TRUE
                              # ),
                              # hr(),
                              # 
                              # checkboxGroupInput(inputId = "part_growthbox",
                              #                    label = "Growth Options:(UNDER CONSTRUCTION)",
                              #                    choices = c("Linear",
                              #                                "Exponential"),
                              #                    selected = c("Type")),
                              # #check box for random options
                              # checkboxGroupInput(inputId = "part_Options",
                              #                    label = "Other Options:(UNDER CONSTRUCTION)",
                              #                    choices = c("Include Agent Guarantee of 30 hrs/mo",
                              #                                "Monte Carlo Simulation",
                              #                                "Include Varience in Projetions"),
                              #                    selected = c("Type")),
                              h6("* Costs multiplier refers to the percentage that Business Development Costs, 
                                 Subscription Costs, R&D Costs, 
                                 and Sales/Marketing Costs increase at relative to revenue."),
                              
                              #reload data clicker
                              fluidRow(
                                column(6,
                                       actionButton(inputId = "part_reload", label = "Reload data"),
                                       textOutput('clics')
                                ),
                                column(6,
                                       actionButton(inputId = "part_newTable", label = "Show Sheet")
                                )
                              )
                              ),
                            
                            
                            # main panel outputs for partners
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Labor Costs", htmlOutput("part_Lcost")),
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

            # more page that had spreadsheet and demo dropdowns
           navbarMenu("More",
                      tabPanel("Further Customization",
                               htmlOutput('table')
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

  

