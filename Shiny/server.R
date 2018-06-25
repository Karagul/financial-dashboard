library(shiny)
library(ggplot2)
library(googleVis)
library(googlesheets)
library(DT)
library(shinythemes)
source('ui.R')
load('gs_MRT3')
# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  ###date slider
  # sliderMonth <- reactiveValues()
  # observe({
  #   full.date <- as.POSIXct(input$moSlider, tz="GMT")
  #   sliderMonth$Month <- as.character(monthStart(full.date))
  # })
  
  ###################################INVESTOR PAGE#############################################
  #####################################Profit Page##############################
  output$profit <- renderGvis({
    gross_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Gross Profit`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Net Profit`))
    df=data.frame(Month = gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  Gross_Profit = gross_profit_numeric[input$moSlider[1]:input$moSlider[2]],
                  Net_Profits = net_profit_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisColumnChart(df, xvar = "Month", 
                    yvar = c("Gross_Profit", "Net_Profits"),
                    options=list(bar="{groupWidth:'70%'}", width = "800", height = "400", 
                                 isStacked = TRUE, vAxis="{title:'Dollars($)'}",
                                 hAxis="{title:'Months'}", title = "Net Profit",
                                 series = "[{color:'8497e5'}, {color:'b8e986'}]"))
  })
  
  ################################Runway page################################
  output$runway <- renderGvis({
    cash_in_bank_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Cash in Bank`))
    net_profit_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Net Profit`))
    funds_raised_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Cumulative Fundraising`))
    df=data.frame(Month= gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  Cash_in_Bank= cash_in_bank_numeric[input$moSlider[1]:input$moSlider[2]],
                  Net_Profit = net_profit_numeric[input$moSlider[1]:input$moSlider[2]],
                  Funds_Raised = funds_raised_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize=6,width = 800, height = 400,vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", title = "Runway", 
                                   series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]"))
  })
  
  ############################Labor Cost Page########################
  
  output$Lcost <- renderGvis({
    Agent_Labor_cost_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Agent Labor Costs`))
    operators_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Operators))
    RRR_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$RRRs))
    sentries_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Sentries))
    strategists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Strategists))
    specialists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Specialists))
    df=data.frame(Month = gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
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
                   options=list(seriesType="line",
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
  
  ###########################Revenue Page############################
  output$gchart <-renderGvis({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Revenue))
    df=data.frame(month= gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  Revenue= revenue_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisBarChart(df, options=list(width = 800, height = 400, vAxis="{title:'Months'}",
                                  hAxis="{title:'Dollars($)'}", title = "Revenue", 
                                  series = "[{color:'8497e5'}]"))
  })
  
  ##########Partner pay/gross revenue##################
  output$combo <- renderGvis({
    partner_pay_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Partner Pay`))
    gross_profits_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Gross Profit`))
    df=data.frame(Month = gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  Partner_Pay = partner_pay_numeric[input$moSlider[1]:input$moSlider[2]],
                  Gross_Profits = gross_profits_numeric[input$moSlider[1]:input$moSlider[2]])
    gvisComboChart(df, xvar = "Month", 
                   yvar = c("Partner_Pay", "Gross_Profits"),
                   options=list(seriesType="bars",
                                series="[{type:'line', color: 'b8e986'},
                                {type:'bars', color: '8497e5'}]", 
                                width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                hAxis="{title:'Months'}", title = "Partner Pay Compared to Revenue"))
  })
  
  #########other####################
  output$viz1 <-renderGvis({
    Gross_Margins = as.numeric(gsub("[\\%,]", "", gs_MRT3$`Gross Margins`))
    df=data.frame(Month= gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  `Gross Margins`= Gross_Margins[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(width = 400, height = 200, vAxis="{title:'Perecent of Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Gross Margins"))
  })
  output$viz2 <-renderGvis({
    partner = as.numeric(gsub("[\\%,]", "", gs_MRT3$Partners))
    df=data.frame(Month = gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  Partner = partner[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(width = 400, height = 200, vAxis="{title:'# of Partners'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Amount of Partners"))
  })
  
  output$viz3 <-renderGvis({
    part_bonus = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Partner Bonuses`))
    df=data.frame(Month = gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  'Partner Bonuses' = part_bonus[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Partner Bonues"))
  })
  output$viz4 <-renderGvis({
    comissions = as.numeric(gsub("[\\$,]", "", gs_MRT3$Comissions))
    df=data.frame(Month= gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  Comissons = comissions[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Comissions"))
  })
  
  output$viz5 <-renderGvis({
    total_fixed_cost = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Total Fixed Costs`))
    df=data.frame(Month = gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  `Total Fixed Costs`= total_fixed_cost[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Total Fixed Costs"))
  })
  output$viz6 <-renderGvis({
    RDCost = as.numeric(gsub("[\\$,]", "", gs_MRT3$`R&D Costs`))
    df=data.frame(Month = gs_MRT3[input$moSlider[1]:input$moSlider[2],1], 
                  `R&D Costs` = RDCost[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "R&D Costs"))
  })
  
  ######################################PARTNER PAGE################################################
  output$Bar_graph3.0 <- renderPlot({
    revenue_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT3$Revenue))
    ggplot(data = gs_MRT3, aes(x = Month, y = revenue_numeric/100)) +
      geom_bar(stat = "identity", color = "yellow")
  })
  
  output$line_graph3.0 <- renderPlot({
    Labor_Cost = as.numeric(gsub("[\\$,]", "", gs_MRT3$`Labor Cost`))
    ggplot(data = gs_MRT3, aes(x = Month, y = Labor_Cost/100)) +
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
