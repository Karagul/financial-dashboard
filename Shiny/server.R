library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)

#####date slider
# monthStart <- function(x) ({
#   x <- as.POSIXlt(x)
#   x$mday <- 1
#   as.Date(x)
# })

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  ##date slider
  # sliderMonth <- reactiveValues()
  # observe({
  #   full.date <- as.POSIXct(input$moSlider_date, tz="GMT")
  #   sliderMonth$Month <- as.character(monthStart(full.date))
  # })
  
  ###################################INVESTOR PAGE#############################################
  #bottoom page table
  output$main_table <-renderGvis({
    gvisTable(MRT$data[,c(1:26,31:33,35:37,42,44,47:50,61)])
  })
  #reload data button#
  MRT <- reactiveValues()
  observeEvent(input$reload, {
    MRT$data <- gs_read( gs_key("1ZtE7i3lJCNaCBiU26mYFLEOL9ZqLE8bNyLeh1woCuck"), ws = "Monthly Shiny and Data Studio Launch Pad") 
  }, ignoreNULL=FALSE)
  output$clics <- renderText(input$reload)
  
  #slider for costs multiplier
  # observeEvent(input$mCost, {
  # if(input$mCost == "Revenue/3"){
  #   MRT$data[,"Subscription Costs"] <- 4/3 * MRT$data[,"Subscription Costs"]
  #   MRT$data[,"R&D Costs"] <- 4/3 * MRT$data[,"R&D Costs"]
  #   MRT$data[,"BD Costs"] <- 4/3 * MRT$data[,"BD Costs"]
  #   MRT$data[,"Discretionary Spending"] <- 4/3 * MRT$data[,"Discretionary Spending"]
  # }
  # else if(input$mCost == "Revenue/2"){
  #   MRT$data[,"Subscription Costs"] <- 2 * MRT$data[,"Subscription Costs"]
  #   MRT$data[,"R&D Costs"] <- 2 * MRT$data[,"R&D Costs"]
  #   MRT$data[,"BD Costs"] <- 2 * MRT$data[,"BD Costs"]
  #   MRT$data[,"Discretionary Spending"] <- 2 * MRT$data[,"Discretionary Spending"]
  # }
  # else if(input$mCost == "Revenue"){
  #   MRT$data[,"Subscription Costs"] <- 4 * MRT$data[,"Subscription Costs"]
  #   MRT$data[,"R&D Costs"] <- 4 * MRT$data[,"R&D Costs"]
  #   MRT$data[,"BD Costs"] <- 4 * MRT$data[,"BD Costs"]
  #   MRT$data[,"Discretionary Spending"] <- 4 * MRT$data[,"Discretionary Spending"]
  # }
  # else{
  #   MRT$data[,"Subscription Costs"] <-  MRT$data[,"Subscription Costs"]
  #   MRT$data[,"R&D Costs"] <-  MRT$data[,"R&D Costs"]
  #   MRT$data[,"BD Costs"] <-  MRT$data[,"BD Costs"]
  #   MRT$data[,"Discretionary Spending"] <-  MRT$data[,"Discretionary Spending"]
  # }
  # }, ignoreNULL=FALSE)
  
  #####################################Profit Page####################################################################################
  output$profit <- renderGvis({
    gross_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],1], 
                  Gross_Profit = gross_profit_numeric[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit = net_profit_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisColumnChart(df, xvar = "Month", 
                    yvar = c("Gross_Profit", "Net_Profit"),
                    options=list(bar="{groupWidth:'70%'}", width = "800", height = "400",
                                 isStacked = TRUE, vAxis="{title:'Dollars($)'}",
                                 hAxis="{title:'Months'}", title = "Profit",
                                 series = "[{color:'8497e5'}, {color:'b8e986'}]"))
  })
  #gross/ net margins
  output$gross <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    gross_margins_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Gross Margins`))
    net_margins_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Net Margins`))
    df=data.frame(Revenue = revenue_numeric[input$moSlider[1]:input$moSlider[2]],
                  Gross_Margins= gross_margins_numeric[input$moSlider[1]:input$moSlider[2]],
                  Net_Margins= net_margins_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit Margins'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}]", 
                                   title = "Margins(%)"))
  })
  
  ################################Runway page########################################################################################
  output$runway <- renderGvis({
    cash_in_bank_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Cash in Bank`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    funds_raised_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Cumulative Fundraising`))
    df=data.frame(Month= MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Cash_in_Bank= cash_in_bank_numeric[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit = net_profit_numeric[input$moSlider[1]:input$moSlider[2]],
                  Funds_Raised = funds_raised_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize=6,width = 800, height = 400,vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", title = "Runway", 
                                   series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]"))
  })
  
  #Profit and Revenue
  output$profRev <-renderGvis({
    gross_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    overhead_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Overhead))
    df=data.frame(Revenue= gross_profit_numeric[input$moSlider[1]:input$moSlider[2]],
                  Overhead= overhead_numeric[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profit= gross_profit_numeric[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit= net_profit_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]", 
                                   title = "Profit/Revenue"))
  })
  
  ###########################Growth Page##############################################################################################
  output$num_client <- renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    ent_client_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Enterprise Clients`))
    small_buis_cli_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Small Business Clients`))
    personal_clients_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Personal Clients`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Revenue = revenue_numeric[input$moSlider[1]:input$moSlider[2]],
                  Enterprise_Clients = ent_client_numeric[input$moSlider[1]:input$moSlider[2]],
                  Small_Business_Clients = small_buis_cli_numeric[input$moSlider[1]:input$moSlider[2]],
                  Personal_Clients = personal_clients_numeric[input$moSlider[1]:input$moSlider[2]])
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
  
  output$rev_client <- renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    ent_rev_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Enterprise Revenue`))
    small_buis_rev_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Small Business Revenue`))
    personal_rev_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Personal Revenue`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Revenue = revenue_numeric[input$moSlider[1]:input$moSlider[2]],
                  Enterprise_Clients_Revenue = ent_rev_numeric[input$moSlider[1]:input$moSlider[2]],
                  Small_Business_Clients_Revenue = small_buis_rev_numeric[input$moSlider[1]:input$moSlider[2]],
                  Personal_Clients_Revenue = personal_rev_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisComboChart(df, xvar="Month",
                   yvar=c("Personal_Clients_Revenue", "Revenue", "Small_Business_Clients_Revenue", 
                          "Enterprise_Clients_Revenue"),
                   options=list(pointSize = 3, seriesType="bar",
                                series="[{type:'bars', color:'b8e986'}, 
                                {type:'line', color:'8497e5'},
                                {type:'bars', color:'grey'},
                                {type: 'bars', color: 'black'}]",
                                vAxes="[{title:'Revenue'}]",  isStacked = TRUE,
                                width = 800, height = 400, title = "Revenue from Clients", hAxis="{title:'Months'}"))
  })
  
  #######################################churn page#################################################################################
  
  output$client <-renderGvis({
    churn_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$Churn))
    client_growth_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Client Growth Percentage`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Churn = churn_numeric[input$moSlider[1]:input$moSlider[2]],
                  Client_Growth_Percentage = client_growth_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Percent'}",
                                   hAxis="{title:'Months'}", title = "Churn Vs Client Growth", 
                                   series = "[{color:'8497e5'}, {color: 'b8e986'}]"))
  })
  
  # output$cltv_cac <-renderGvis({
  #   cltv_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$CLTV))
  #   cac_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$CAC))
  #   df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
  #                 CLTV = cltv_numeric[input$moSlider[1]:input$moSlider[2]],
  #                 CAC = cac_numeric[input$moSlider[1]:input$moSlider[2]])
  #   gvisColumnChart(df, options=list( width = 800, height = 400, vAxis="{title:'Dollars($)'}",
  #                                    hAxis="{title:'Months'}", title = "Client Growth", 
  #                                    series = "[{color:'8497e5'}, {color: 'b8e986'}]"))
  # })
  
  
  ############################Workforce Page##########################################################################################
  
  #work Force
  output$workForce <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    head_count_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Company Head Count`))
    revperhead_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Revenue per Head`))
    partner_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$Partners))
    df=data.frame(Revenue= revenue_numeric[input$moSlider[1]:input$moSlider[2]],
                  Company_Head_Count= head_count_numeric[input$moSlider[1]:input$moSlider[2]],
                  Revenue_per_Head= revperhead_numeric[input$moSlider[1]:input$moSlider[2]],
                  Partners= partner_numeric[input$moSlider[1]:input$moSlider[2]])
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
  
  output$Lcost <- renderGvis({
    Agent_Labor_cost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Agent Labor Cost`))
    operators_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Operators))
    RRR_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$RRRs))
    sentries_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Sentries))
    strategists_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Strategists))
    specialists_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Specialists))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Agent_Labor_Cost = Agent_Labor_cost_numeric[input$moSlider[1]:input$moSlider[2]],
                  Operator_Labor_cost = operators_numeric[input$moSlider[1]:input$moSlider[2]],
                  RRR_Labor_Cost = RRR_numeric[input$moSlider[1]:input$moSlider[2]],
                  Sentry_Labor_Cost = sentries_numeric[input$moSlider[1]:input$moSlider[2]],
                  Strategists_Labor_Cost = strategists_numeric[input$moSlider[1]:input$moSlider[2]],
                  Specialists_Labor_Cost = specialists_numeric[input$moSlider[1]:input$moSlider[2]]
                  
    )
    gvisComboChart(df, xvar="Month",
                   yvar=c("Operator_Labor_cost", "Agent_Labor_Cost", "RRR_Labor_Cost", 
                          "Sentry_Labor_Cost", "Strategists_Labor_Cost", "Specialists_Labor_Cost"),
                   options=list(pointSize = 3, seriesType="line",
                                series="[{type:'line', 
                                targetAxisIndex:0,
                                color:'b8e986'}, 
                                {type:'bars', 
                                targetAxisIndex:1,
                                color:'8497e5'},
                                {color:'grey'}, {color:'black'}, 
                                {color:'orange'},{color:'blue'}]",
                                vAxes="[{title:'Labor_Costs'}, {title:'Agent_Labor_Cost'}]", 
                                width = 800, height = 400, title = "Labor Costs", hAxis="{title:'Months'}"))
})
  
  
  ############################Partner pay########################################################################################
  output$combo <- renderGvis({
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Pay`))
    gross_profits_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Partner_Pay = partner_pay_numeric[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profits = gross_profits_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisComboChart(df, xvar = "Month", 
                   yvar = c("Partner_Pay", "Gross_Profits"),
                   options=list(seriesType="bars",
                                series="[{type:'line', color: '8497e5'},
                                {type:'bars', color: 'b8e986'}]", 
                                width = 800, height = 400, vAxis="{title:'Dollars($)'}", pointSize = 4,
                                hAxis="{title:'Months'}", title = "Partner Pay"))
  })
  
  output$linechart <- renderGvis({
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Pay`))
    gross_profits_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    df=data.frame(Revenue = revenue_numeric[input$moSlider[1]:input$moSlider[2]], 
                  Partner_Pay = partner_pay_numeric[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profits = gross_profits_numeric[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit = net_profit_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Black'}, {color:'Red'}]", title = "Partner Pay Compared to Revenue"))
  })
  
  ########################################Overhead Page##############################################################################

  output$overhead <-renderGvis({
    overhead_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Overhead))
    RDcost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`R&D Costs`))
    BDcost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`BD Costs`))
    subsCost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Subscription Costs`))
    Disc_spend_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Discretionary Spending`))
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Pay`))
    partner_bonuses_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Bonuses`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"],
                  Total_Overhead = overhead_numeric[input$moSlider[1]:input$moSlider[2]],
                  BD_Costs= BDcost_numeric[input$moSlider[1]:input$moSlider[2]],
                  Subscription_Costs= subsCost_numeric[input$moSlider[1]:input$moSlider[2]],
                  Discretionary_Spending= Disc_spend_numeric[input$moSlider[1]:input$moSlider[2]],
                  Partner_Pay= partner_pay_numeric[input$moSlider[1]:input$moSlider[2]],
                  Partner_Bonuses= partner_bonuses_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Red'}]", title = "Overhead"))
  })
  
  #Ecoomies of Scale
  output$econScale <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    overhead_opex_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Overhead/Opex %`))
    df=data.frame(Revenue = revenue_numeric[input$moSlider[1]:input$moSlider[2]],
                  `Overhead/Opex %`= overhead_opex_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Overhead/Opex %'}",
                                   hAxis="{title:'Revenue'}", series = "[{color: '8497e5'}]", title = "Economies of Scale"))
  })
  
  
  #######################################other################################################################################
  output$viz1 <-renderGvis({
    Gross_Margins = as.numeric(gsub("[\\%,]", "", MRT$data$`Gross Margins`))
    df=data.frame(Month= MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Gross_Margins= Gross_Margins[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Perecent of Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Gross Margins"))
  })
  output$viz2 <-renderGvis({
    partner = as.numeric(gsub("[\\%,]", "", MRT$data$Partners))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Partner = partner[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'# of Partners'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Amount of Partners"))
  })
  
  output$viz3 <-renderGvis({
    part_bonus = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Bonuses`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Partner_Bonuses = part_bonus[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Partner Bonues"))
  })
  output$viz4 <-renderGvis({
    comissions = as.numeric(gsub("[\\$,]", "", MRT$data$Comissions))
    df=data.frame(Month= MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Comissions = comissions[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Comissions"))
  })
  
  output$viz5 <-renderGvis({
    total_fixed_cost = as.numeric(gsub("[\\$,]", "", MRT$data$`Subscription Costs`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Subscription_Costs= total_fixed_cost[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Total Fixed Costs"))
  })
  output$viz6 <-renderGvis({
    RDCost = as.numeric(gsub("[\\$,]", "", MRT$data$`R&D Costs`))
    df=data.frame(Month = MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  RD_Costs = RDCost[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "R&D Costs"))
  })
  
  ######################################PARTNER PAGE##################################################################################
  #bottom page table
  output$part_main_table <-renderGvis({
    gvisTable(MRT$data[,c(1:26,31:33,35:37,42,44,47:50,61)])
  })
  
  #reload data button#
  MRT <- reactiveValues()
  observeEvent(input$part_reload, {
    MRT$data <- gs_read( gs_key("1ZtE7i3lJCNaCBiU26mYFLEOL9ZqLE8bNyLeh1woCuck"), ws = "Monthly Shiny and Data Studio Launch Pad")
  }, ignoreNULL=FALSE)
  output$part_clics <- renderText(input$part_reload)
  
  # observeEvent(input$part_mCost, {
  #   if(input$mCost == "Revenue/3"){
  #     MRT$data[,"Subscription Costs"] <- 4/3 * MRT$data[,"Subscription Costs"]
  #     MRT$data[,"R&D Costs"] <- 4/3 * MRT$data[,"R&D Costs"]
  #     MRT$data[,"BD Costs"] <- 4/3 * MRT$data[,"BD Costs"]
  #     MRT$data[,"Discretionary Spending"] <- 4/3 * MRT$data[,"Discretionary Spending"]
  #   }
  #   else if(input$mCost == "Revenue/2"){
  #     MRT$data[,"Subscription Costs"] <- 2 * MRT$data[,"Subscription Costs"]
  #     MRT$data[,"R&D Costs"] <- 2 * MRT$data[,"R&D Costs"]
  #     MRT$data[,"BD Costs"] <- 2 * MRT$data[,"BD Costs"]
  #     MRT$data[,"Discretionary Spending"] <- 2 * MRT$data[,"Discretionary Spending"]
  #   }
  #   else if(input$mCost == "Revenue"){
  #     MRT$data[,"Subscription Costs"] <- 4 * MRT$data[,"Subscription Costs"]
  #     MRT$data[,"R&D Costs"] <- 4 * MRT$data[,"R&D Costs"]
  #     MRT$data[,"BD Costs"] <- 4 * MRT$data[,"BD Costs"]
  #     MRT$data[,"Discretionary Spending"] <- 4 * MRT$data[,"Discretionary Spending"]
  #   }
  #   else{
  #     MRT$data[,"Subscription Costs"] <-  MRT$data[,"Subscription Costs"]
  #     MRT$data[,"R&D Costs"] <-  MRT$data[,"R&D Costs"]
  #     MRT$data[,"BD Costs"] <-  MRT$data[,"BD Costs"]
  #     MRT$data[,"Discretionary Spending"] <-  MRT$data[,"Discretionary Spending"]
  #   }
  # })
  #####################################Profit Page#####################################################################################
  output$part_profit <- renderGvis({
    gross_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],1], 
                  Gross_Profit = gross_profit_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = net_profit_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisColumnChart(df, xvar = "Month", 
                    yvar = c("Gross_Profit", "Net_Profit"),
                    options=list(bar="{groupWidth:'70%'}", width = "800", height = "400",
                                 isStacked = TRUE, vAxis="{title:'Dollars($)'}",
                                 hAxis="{title:'Months'}", title = "Profit",
                                 series = "[{color:'8497e5'}, {color:'b8e986'}]"))
  })
  #gross/ net margins
  output$part_gross <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    gross_margins_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Gross Margins`))
    net_margins_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Net Margins`))
    df=data.frame(Revenue = revenue_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Margins= gross_margins_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Margins= net_margins_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit Margins'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}]", 
                                   title = "Margins(%)"))
  })
  
  ################################Runway page########################################################################################
  output$part_runway <- renderGvis({
    cash_in_bank_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Cash in Bank`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    funds_raised_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Cumulative Fundraising`))
    df=data.frame(Month= MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Cash_in_Bank= cash_in_bank_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = net_profit_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Funds_Raised = funds_raised_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize=6,width = 800, height = 400,vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", title = "Runway", 
                                   series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]"))
  })
  
  #Profit and Revenue
  output$part_profRev <-renderGvis({
    gross_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    overhead_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Overhead))
    df=data.frame(Revenue= gross_profit_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Overhead= overhead_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profit= gross_profit_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit= net_profit_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]", 
                                   title = "Profit/Revenue"))
  })
  
  ###########################Growth Page########################################################################################
  output$part_num_client <- renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    ent_client_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Enterprise Clients`))
    small_buis_cli_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Small Business Clients`))
    personal_clients_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Personal Clients`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Revenue = revenue_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Enterprise_Clients = ent_client_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Small_Business_Clients = small_buis_cli_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Personal_Clients = personal_clients_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
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
  
  
  output$part_rev_client <- renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    ent_rev_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Enterprise Revenue`))
    small_buis_rev_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Small Business Revenue`))
    personal_rev_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Personal Revenue`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Revenue = revenue_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Enterprise_Clients_Revenue = ent_rev_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Small_Business_Clients_Revenue = small_buis_rev_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Personal_Clients_Revenue = personal_rev_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisComboChart(df, xvar="Month",
                   yvar=c("Personal_Clients_Revenue", "Revenue", "Small_Business_Clients_Revenue", 
                          "Enterprise_Clients_Revenue"),
                   options=list(pointSize = 3, seriesType="bar",
                                series="[{type:'bars', color:'b8e986'}, 
                                {type:'line', color:'8497e5'},
                                {type:'bars', color:'grey'},
                                {type: 'bars', color: 'black'}]",
                                vAxes="[{title:'Revenue'}]",  isStacked = TRUE,
                                width = 800, height = 400, title = "Revenue from Clients", hAxis="{title:'Months'}"))
  })
  ##########################################Churn page##################################################################################
  
  
  output$part_client <-renderGvis({
    churn_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$Churn))
    client_growth_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Client Growth Percentage`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Churn = churn_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Client_Growth_Percentage = client_growth_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Percent'}",
                                   hAxis="{title:'Months'}", title = "Client Growth", 
                                   series = "[{color:'8497e5'}, {color: 'b8e986'}]"))
  })
  
  ############################Workforce Page##########################################################################################
  
  #work Force
  output$part_workForce <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    head_count_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Company Head Count`))
    revperhead_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Revenue per Head`))
    partner_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$Partners))
    df=data.frame(Revenue= revenue_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Company_Head_Count= head_count_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Revenue_per_Head= revperhead_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partners= partner_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
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
  
  output$part_Lcost <- renderGvis({
    Agent_Labor_cost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Agent Labor Cost`))
    operators_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Operators))
    RRR_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$RRRs))
    sentries_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Sentries))
    strategists_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Strategists))
    specialists_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Specialists))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Agent_Labor_Cost = Agent_Labor_cost_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Operator_Labor_cost = operators_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  RRR_Labor_Cost = RRR_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Sentry_Labor_Cost = sentries_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Strategists_Labor_Cost = strategists_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Specialists_Labor_Cost = specialists_numeric[input$part_moSlider[1]:input$part_moSlider[2]]
                  
    )
    gvisComboChart(df, xvar="Month",
                   yvar=c("Operator_Labor_cost", "Agent_Labor_Cost", "RRR_Labor_Cost", 
                          "Sentry_Labor_Cost", "Strategists_Labor_Cost", "Specialists_Labor_Cost"),
                   options=list(pointSize = 3, seriesType="line",
                                series="[{type:'line', 
                                targetAxisIndex:0,
                                color:'b8e986'}, 
                                {type:'bars', 
                                targetAxisIndex:1,
                                color:'8497e5'},
                                {color:'grey'}, {color:'black'}, 
                                {color:'orange'},{color:'blue'}]",
                                vAxes="[{title:'Labor_Costs'}, {title:'Agent_Labor_Cost'}]", 
                                width = 800, height = 400, title = "Labor Costs", hAxis="{title:'Months'}"))
    })
  
  
  ############################Partner pay########################################################################################
  output$part_combo <- renderGvis({
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Pay`))
    gross_profits_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Partner_Pay = partner_pay_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profits = gross_profits_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisComboChart(df, xvar = "Month", 
                   yvar = c("Partner_Pay", "Gross_Profits"),
                   options=list(seriesType="bars",
                                series="[{type:'line', color: '8497e5'},
                                {type:'bars', color: 'b8e986'}]", 
                                width = 800, height = 400, vAxis="{title:'Dollars($)'}", pointSize = 4,
                                hAxis="{title:'Months'}", title = "Partner Pay"))
  })
  
  output$part_linechart <- renderGvis({
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Pay`))
    gross_profits_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Net Profit`))
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    df=data.frame(Revenue = revenue_numeric[input$part_moSlider[1]:input$part_moSlider[2]], 
                  Partner_Pay = partner_pay_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profits = gross_profits_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = net_profit_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Black'}, {color:'Red'}]", title = "Partner Pay Compared to Revenue"))
  })
  
  ########################################Overhead Page##############################################################################
  
  output$part_overhead <-renderGvis({
    overhead_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Overhead))
    RDcost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`R&D Costs`))
    BDcost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`BD Costs`))
    subsCost_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Subscription Costs`))
    Disc_spend_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Discretionary Spending`))
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Pay`))
    partner_bonuses_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Bonuses`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"],
                  Total_Overhead = overhead_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  BD_Costs= BDcost_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Subscription_Costs= subsCost_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Discretionary_Spending= Disc_spend_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partner_Pay= partner_pay_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partner_Bonuses= partner_bonuses_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Red'}]", title = "Overhead"))
  })
  
  #Ecoomies of Scale
  output$part_econScale <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", MRT$data$Revenue))
    overhead_opex_numeric = as.numeric(gsub("[\\%,]", "", MRT$data$`Overhead/Opex %`))
    df=data.frame(Revenue = revenue_numeric[input$part_moSlider[1]:input$part_moSlider[2]],
                  `Overhead/Opex %`= overhead_opex_numeric[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Overhead/Opex %'}",
                                   hAxis="{title:'Revenue'}", series = "[{color: '8497e5'}]", title = "Economies of Scale"))
  })
  
  #######################################other################################################################################
  output$part_viz1 <-renderGvis({
    Gross_Margins = as.numeric(gsub("[\\%,]", "", MRT$data$`Gross Margins`))
    df=data.frame(Month= MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Gross_Margins= Gross_Margins[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Perecent of Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Gross Margins"))
  })
  output$part_viz2 <-renderGvis({
    partner = as.numeric(gsub("[\\%,]", "", MRT$data$Partners))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Partner = partner[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'# of Partners'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Amount of Partners"))
  })
  
  output$part_viz3 <-renderGvis({
    part_bonus = as.numeric(gsub("[\\$,]", "", MRT$data$`Partner Bonuses`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Partner_Bonuses = part_bonus[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Partner Bonues"))
  })
  output$part_viz4 <-renderGvis({
    comissions = as.numeric(gsub("[\\$,]", "", MRT$data$Comissions))
    df=data.frame(Month= MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Comissions = comissions[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Comissions"))
  })
  
  output$part_viz5 <-renderGvis({
    total_fixed_cost = as.numeric(gsub("[\\$,]", "", MRT$data$`Subscription Costs`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Subscription_Costs= total_fixed_cost[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Total Fixed Costs"))
  })
  output$part_viz6 <-renderGvis({
    RDCost = as.numeric(gsub("[\\$,]", "", MRT$data$`R&D Costs`))
    df=data.frame(Month = MRT$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  RD_Costs = RDCost[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "R&D Costs"))
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
    gvisTable(MRT$data[,c(1:26,31:33,35:37,42,44,47:50,61)])
  })

  }
