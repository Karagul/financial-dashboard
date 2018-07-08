library(shiny)
library(googleVis)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)
library(DT)


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
  ###################################INVESTOR PAGE#############################################
  #bottoom page table
  output$main_table <-renderGvis({
    gvisTable(live$data)
  })
  
  #Effects the slider for the client growth slider
  
  observeEvent(input$growslide, {
    i <- 1
      while(i <= nrow(MRT$data)){
        if(MRT$data$Month[i] == curr_date){
          row_after_curr_mon<-i+1
          while(row_after_curr_mon <= nrow(MRT$data)){
            if(input$growslide == "Custom"){
              live$data <- MRT$data
            }
            else if(input$growslide == "5%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .05
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
              Total_monthly_ARPA_fn(row_after_curr_mon)
              Client_growth_after_churn_fn(row_after_curr_mon)
              Company_head_count_fn(row_after_curr_mon)
              Revenue_per_head_fn(row_after_curr_mon)
              Actual_labor_costs_fn(row_after_curr_mon)
              Gross_profit_fn(row_after_curr_mon)
              Gross_margins_fn(row_after_curr_mon)
              Total_partner_pay_fn(row_after_curr_mon)
              Commissions_fn(row_after_curr_mon)
            }
            else if(input$growslide == "10%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .10
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
              Total_monthly_ARPA_fn(row_after_curr_mon)
              Client_growth_after_churn_fn(row_after_curr_mon)
              Company_head_count_fn(row_after_curr_mon)
              Revenue_per_head_fn(row_after_curr_mon)
              Actual_labor_costs_fn(row_after_curr_mon)
              Gross_profit_fn(row_after_curr_mon)
              Gross_margins_fn(row_after_curr_mon)
              Total_partner_pay_fn(row_after_curr_mon)
              Commissions_fn(row_after_curr_mon)
            }
            else if(input$growslide == "15%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .15
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
              Total_monthly_ARPA_fn(row_after_curr_mon)
              Client_growth_after_churn_fn(row_after_curr_mon)
              Company_head_count_fn(row_after_curr_mon)
              Revenue_per_head_fn(row_after_curr_mon)
              Actual_labor_costs_fn(row_after_curr_mon)
              Gross_profit_fn(row_after_curr_mon)
              Gross_margins_fn(row_after_curr_mon)
              Total_partner_pay_fn(row_after_curr_mon)
              Commissions_fn(row_after_curr_mon)
            }
            else if(input$growslide == "20%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .20
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "25%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .25
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "30%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .30
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "35%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .35
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "40%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .40
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "45%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .45
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "50%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .50
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "55%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .55
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "60%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .60
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "65%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .65
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "70%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .70
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "75%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .75
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "80%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .80
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "85%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .85
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "90%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .90
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "95%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- .95
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            else if(input$growslide == "100%"){
              live$data$`Client growth percentage`[row_after_curr_mon] <- 1
              Revenue_fn(row_after_curr_mon)
              Revenue_percent_change_fn(row_after_curr_mon)
            }
            row_after_curr_mon = row_after_curr_mon + 1
          }
        }
        i = i + 1
    }
  }, ignoreNULL=FALSE)


  #effects the slider for the cost multiplier slider
  observeEvent(input$mCost, {
    if(input$growslide == "10%"){
      live$data = MRT$data
    }
    else if(input$growslide == "20%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "30%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "40%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "50%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "60%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "70%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "80%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "90%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "100%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "110%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "125%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "150%"){
      live$data <- MRT$data
    }
    else if(input$growslide == "200%"){
      live$data <- MRT$data
    }
  }, ignoreNULL=FALSE)

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
    df=data.frame(Month= MRT$data[input$moSlider[1]:input$moSlider[2],"Month"], 
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
                  Enterprise_Clients_Revenue = live$data$`Enterprise revenue`[input$moSlider[1]:input$moSlider[2]],
                  Small_Business_Clients_Revenue = live$data$`Small business revenue`[input$moSlider[1]:input$moSlider[2]],
                  Personal_Clients_Revenue = live$data$`Personal revenue`[input$moSlider[1]:input$moSlider[2]])
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
  
  
  #######################################other################################################################################
  #gross margins visual
  output$viz1 <-renderGvis({
    df=data.frame(Month= live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Gross_Margins= live$data$`Gross margins`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Perecent of Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Gross Margins"))
  })
  #amount of partners visual
  output$viz2 <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Partner = live$data$`Number of partners`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'# of Partners'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Amount of Partners"))
  })
  #partner bonuses visual
  output$viz3 <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Partner_Bonuses = live$data$`Partner bonuses`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Partner Bonues"))
  })
  #comissons visual
  output$viz4 <-renderGvis({
    df=data.frame(Month= live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Commissions = live$data$Commissions[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Comissions"))
  })
  #total fixed costs visual
  output$viz5 <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Subscription_Costs = live$data$`Subscription costs`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Subscription Costs"))
  })
  #R&D costs Visual
  output$viz6 <-renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  RD_Costs = live$data$`Total R and D costs`[input$moSlider[1]:input$moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "R&D Costs"))
  })
  
  ######################################PARTNER PAGE##################################################################################
  #bottom page table
  output$part_main_table <-renderGvis({
    gvisTable(MRT$data[,c(1:26,31:33,35:37,42,44,47:50,61)])
  })
  
  #Effects the slider for the client growth slider
  
  observeEvent(input$part_growslide, {
    if(input$growslide == "5%"){
      live$data = MRT$data
    }
    else if(input$part_growslide == "10%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "15%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "20%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "25%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "30%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "35%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "40%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "45%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "50%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "55%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "60%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "65%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "70%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "75%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "80%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "85%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "90%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "95%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "100%"){
      live$data <- MRT$data
    }
  }, ignoreNULL=FALSE)
  
  #effects the slider for the cost multiplier slider
  observeEvent(input$part_mCost, {
    if(input$growslide == "10%"){
      live$data = MRT$data
    }
    else if(input$part_growslide == "20%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "30%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "40%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "50%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "60%"){
      live$data <- MRT$data
    }
    else if(input$vgrowslide == "70%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "80%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "90%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "100%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "110%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "125%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "150%"){
      live$data <- MRT$data
    }
    else if(input$part_growslide == "200%"){
      live$data <- MRT$data
    }
  }, ignoreNULL=FALSE)
  
  #####################################Profit Page#####################################################################################
  #profit visual
  output$part_profit <- renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],1], 
                  Gross_Profit = live$data$`Gross Profit`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = live$data$`Net Profit`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisColumnChart(df, xvar = "Month", 
                    yvar = c("Gross_Profit", "Net_Profit"),
                    options=list(bar="{groupWidth:'70%'}", width = "800", height = "400",
                                 isStacked = TRUE, vAxis="{title:'Dollars($)'}",
                                 hAxis="{title:'Months'}", title = "Profit",
                                 series = "[{color:'8497e5'}, {color:'b8e986'}]"))
  })
  #gross/ net margins visual
  output$part_gross <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Margins= live$data$`Gross Margins`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Margins= live$data$`Net Margins`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit Margins'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}]", 
                                   title = "Margins(%)"))
  })
  
  ################################Runway page########################################################################################
  #runway visual
  output$part_runway <- renderGvis({
    df=data.frame(Month= live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Cash_in_Bank= live$data$`Cash in Bank`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = live$data$`Net Profit`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Funds_Raised = live$data$`Cumulative Fundraising`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize=6,width = 800, height = 400,vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", title = "Runway", 
                                   series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]"))
  })
  
  #Profit and Revenue visual
  output$part_profRev <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  Overhead = live$data$Overhead[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profit = live$data$`Gross Profit`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = live$data$`Net Profit`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 6, width = 800, height = 400, vAxis="{title:'Profit'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, {color:'grey'}]", 
                                   title = "Profit/Revenue"))
  })
  
  ###########################Growth Page########################################################################################
  #client growth and revenue
  output$part_num_client <- renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  Enterprise_Clients = live$data$`Enterprise Clients`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Small_Business_Clients = live$data$`Small Business Clients`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Personal_Clients = live$data$`Personal Clients`[input$part_moSlider[1]:input$part_moSlider[2]])
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
  output$part_rev_client <- renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  Enterprise_Clients_Revenue = live$data$`Enterprise Revenue`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Small_Business_Clients_Revenue = live$data$`Small Business Revenue`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Personal_Clients_Revenue = live$data$`Personal Revenue`[input$part_moSlider[1]:input$part_moSlider[2]])
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
  
  #churn vs client growth visual
  output$part_client <-renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Churn = live$data$Churn[input$part_moSlider[1]:input$part_moSlider[2]],
                  Client_Growth = live$data$`Client Growth`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Percent'}",
                                   hAxis="{title:'Months'}", title = "Churn Vs Client Growth", 
                                   series = "[{color:'8497e5'}, {color: 'b8e986'}]"))
  })
  
  ############################Workforce Page##########################################################################################
  
  #Revenue per head visual
  output$part_workForce <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  Company_Head_Count = live$data$`Company Head Count`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Revenue_per_Head= live$data$`Revenue Per Employee`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partners= live$data$Partners[input$part_moSlider[1]:input$part_moSlider[2]])
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
  #Labor Costs visuals
  output$part_Lcost <- renderGvis({
    df=data.frame(Month = live$data[input$moSlider[1]:input$moSlider[2],"Month"], 
                  Actual_Labor_Cost = live$data$`Actual Labor Cost`[input$moSlider[1]:input$moSlider[2]],
                  Expected_Operator_Labor_cost = live$data$`Expected Operator Labor Costs`[input$moSlider[1]:input$moSlider[2]],
                  Expected_RRR_Labor_Cost = live$data$`Expected RRR Labor Costs for Assistants`[input$moSlider[1]:input$moSlider[2]],
                  Expected_Specialist_and_Strategist_Labor_Costs = 
                    live$data$`Expected Specialist and Strategist Labor Costs`[input$moSlider[1]:input$moSlider[2]]
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
                  Partner_Pay = live$data$`Partner Pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profits = live$data$`Gross Profit`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisComboChart(df, xvar = "Month", 
                   yvar = c("Partner_Pay", "Gross_Profits"),
                   options=list(seriesType="bars",
                                series="[{type:'line', color: '8497e5'},
                                {type:'bars', color: 'b8e986'}]", 
                                width = 800, height = 400, vAxis="{title:'Dollars($)'}", pointSize = 4,
                                hAxis="{title:'Months'}", title = "Partner Pay"))
  })
  
  #partner pay compared to Revenue visual
  output$part_linechart <- renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]], 
                  Partner_Pay = live$data$`Partner Pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Gross_Profits = live$data$`Gross Profit`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Net_Profit = live$data$`Net Profit`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Revenue'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Black'}, {color:'Red'}]", title = "Partner Pay Compared to Revenue"))
  })
  
  ########################################Overhead Page##############################################################################
  #overhead visual
  output$part_overhead <-renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"],
                  Total_Overhead = live$data$Overhead[input$part_moSlider[1]:input$part_moSlider[2]],
                  BD_Costs= live$data$`BD Costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  RD_Costs= live$data$`R&D Costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Subscription_Costs= live$data$`Subscription Costs`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Discretionary_Spending= live$data$`Discretionary Spending`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partner_Pay= live$data$`Partner Pay`[input$part_moSlider[1]:input$part_moSlider[2]],
                  Partner_Bonuses= live$data$`Partner Bonuses`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 800, height = 400, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color:'8497e5'}, {color:'b8e986'}, 
                                   {color:'grey'}, {color:'Red'}]", title = "Overhead"))
  })
  
  #Ecoomies of Scale visual
  output$part_econScale <-renderGvis({
    df=data.frame(Revenue = live$data$Revenue[input$part_moSlider[1]:input$part_moSlider[2]],
                  `Overhead/Opex %`= live$data$`Overhead to Opex`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 4, width = 800, height = 400, vAxis="{title:'Overhead/Opex %'}",
                                   hAxis="{title:'Revenue'}", series = "[{color: '8497e5'}]", title = "Economies of Scale"))
  })
  
  #######################################other################################################################################
  #gross margins visual
  output$part_viz1 <-renderGvis({
    df=data.frame(Month= live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Gross_Margins= live$data$`Gross Margins`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Perecent of Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Gross Margins"))
  })
  #amunt of partners visuals
  output$part_viz2 <-renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Partner = live$data$Partners[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'# of Partners'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Amount of Partners"))
  })
  #partner bonuses visual
  output$part_viz3 <-renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Partner_Bonuses = live$data$`Partner Bonuses`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Partner Bonues"))
  })
  #commissions visuals 
  output$part_viz4 <-renderGvis({
    df=data.frame(Month= live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Comissions = live$data$Comissions[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Comissions"))
  })
  #total fixed costs visuals
  output$part_viz5 <-renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  Subscription_Costs= live$data$`Subscription Costs`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "Subscription Costs"))
  })
  #R&D Costs visuals
  output$part_viz6 <-renderGvis({
    df=data.frame(Month = live$data[input$part_moSlider[1]:input$part_moSlider[2],"Month"], 
                  RD_Costs = live$data$`R&D Costs`[input$part_moSlider[1]:input$part_moSlider[2]])
    gvisLineChart(df, options=list(pointSize = 2, width = 400, height = 200, vAxis="{title:'Dollars($)'}",
                                   hAxis="{title:'Months'}", series = "[{color: 'b8e986'}]", title = "R&D Costs"))
  })
  
  #########################################MORE PAGE###############################################
  
  #######################################spreadsheet##############################################
  
  output$table <- renderDataTable({
    datatable(MRT$data, 
              editable = TRUE, options = list(pageLength = nrow(MRT$data)))
    })

  }
