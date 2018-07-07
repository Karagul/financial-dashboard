source('server.R')
#We try not to use "return" in any of the functions

rev1 <- function(dframe,index){
  dframe$Revenue[index] <- dollars
}

#1###################NESTED REVENUE FUCNTIONS#########################################

Revenue_fn <- function(index){
  live$data$Revenue[index] <- Enterprise_revenue_fn(index) + 
    Small_business_revenue(index) + 
    Personal_revenue_fn(index)
}

######Enterprise############
Enterprise_revenue_fn <- function(index){
  live$data$`Enterprise revenue`[index] <- Total_enterprise_monthly_operator_fees_fn(index) + 
    Total_enterprise_monthly_assistant_fees_fn(index) + 
    Total_enterprise_monthly_strategist_specialist_fees_fn(index)
}

#########
Total_enterprise_monthly_operator_fees_fn <- function(index){
  live$data$`Total enterprise monthly operator fees`[index] <- Enterprise_monthly_operator_hrs_fn(index) * live$data$`Operator hrly rate`[index]
}
Enterprise_monthly_operator_hrs_fn <- function(index){
  live$data$`Enterprise monthly operator hrs`[index] <- Enterprise_clients_fn(index) * 
    live$data$`Avg enterprise monthly operator hrs`[index]
}



#######
Total_enterprise_monthly_assistant_fees_fn<- function(index){
  live$data$`Total enterprise monthly assistant fees`[index] <- Enterprise_monthly_assistant_hrs_fn(index) * 
    live$data$`Assistant hrly rate`[index]
}

Enterprise_monthly_assistant_hrs_fn <- function(index){
  live$data$`Enterprise monthly assistant hrs`[index] <- Enterprise_clients_fn(index) * 
    live$data$`Avg enterprise monthly assistant hrs`[index]
}


#####
Total_enterprise_monthly_strategist_specialist_fees_fn <- function(index){
  live$data$`Total enterprise monthly strategist specialist fees`[index] <- 
    Enterprise_monthly_strategist_specialist_hrs_fn(index) * live$data$`Strategist specialist hrly rate`[index]
}

Enterprise_monthly_strategist_specialist_hrs_fn <- function(index){
  live$data$`Enterprise monthly strategist specialist hrs`[index] <- Enterprise_clients_fn(index) * 
    live$data$`Avg enterprise monthly strategist specialist hrs`[index]
}

Enterprise_clients_fn <- function(index){
  live$data$`Enterprise clients`[index] <- live$data$`Percent enterprise clients`[index] * Total_clients_fn(index)
}

Total_clients_fn <- function(index){
  live$data$`Total clients`[index] <- (live$data$`Total clients`[index-1]) / 
    (1 + live$data$`Churn percentage weighted by number of clients`[index] - live$data$`Client growth percentage`[index])
}


######Small Buisness#######

Small_business_revenue <- function(index){
  live$data$`Small business revenue`[index] <- Total_small_business_monthly_operator_fees_fn(index) + 
    Total_small_business_monthly_assistant_fees_fn(index) + 
    Total_small_business_monthly_strategist_specialist_fees_fn(index)
}

#######
Total_small_business_monthly_operator_fees_fn <- function(index){
  live$data$`Total small business monthly operator fees`[index] <- Small_business_monthly_operator_hrs_fn(index) * 
    live$data$`Operator hrly rate`[index]
}

Small_business_monthly_operator_hrs_fn <- function(index){
  live$data$`Small business monthly operator hrs`[index] <- Small_business_clients_fn(index) * 
    live$data$`Avg small business monthly operator hrs`[index]
}

########
Total_small_business_monthly_assistant_fees_fn <- function( index){
  live$data$`Total small business monthly assistant fees`[index] <- Small_business_monthly_assistant_hrs_fn(index) * 
    live$data$`Assistant hrly rate`[index]
}

Small_business_monthly_assistant_hrs_fn <- function(index){
  live$data$`Small business monthly assistant hrs`[index] <- Small_business_clients_fn(index) * 
    live$data$`Avg small business monthly assistant hrs`[index]
}

######
Total_small_business_monthly_strategist_specialist_fees_fn <- function(index){
  live$data$`Total small business monthly strategist specialist fees`[index] <- 
    Small_business_monthly_strategist_specialist_hrs_fn(index) * live$data$`Strategist specialist hrly rate`[index]
}

Small_business_monthly_strategist_specialist_hrs_fn <- function(index){
  live$data$`Small business monthly strategist specialist hrs`[index] <- Small_business_clients_fn(index) * 
    live$data$`Avg small business monthly strategist specialist hrs`[index]
}

Small_business_clients_fn <- function(index){
  live$data$`Small business clients`[index] <- live$data$`Percent small business clients`[index] * Total_clients_fn(index)
}

########Personal Revenue########

Personal_revenue_fn <-function(index){
  live$data$`Personal revenue`[index] <- Total_personal_monthly_operator_fees_fn(index) + 
    Total_personal_monthly_assistant_fees_fn(index) + 
    Total_personal_monthly_strategist_specialist_fees_fn(index)
}


########
Total_personal_monthly_operator_fees_fn <- function(index){
  live$data$`Total personal monthly operator fees`[index] <- Personal_monthly_operator_hrs_fn(index) * 
    live$data$`Operator hrly rate`[index]
}


Personal_monthly_operator_hrs_fn <- function(index){
  live$data$`Personal monthly operator hrs`[index] <- live$data$`Avg personal monthly operator hrs`[index] * 
    Personal_clients_fn(index)
}

########
Total_personal_monthly_assistant_fees_fn <- function(index){
  live$data$`Total personal monthly assistant fees`[index] <- Personal_monthly_assistant_hrs_fn(index) * 
    live$data$`Assistant hrly rate`[index]
}

Personal_monthly_assistant_hrs_fn <- function(index){
  live$data$`Personal monthly assistant hrs`[index] <- live$data$`Avg personal monthly assistant hrs`[index] * 
    Personal_clients_fn(index)
}

#######
Total_personal_monthly_strategist_specialist_fees_fn <- function(index){
  live$data$`Total personal monthly strategist specialist fees`[index] <- Personal_monthly_strategist_specialist_hrs_fn(index) * 
    live$data$`Strategist specialist hrly rate`[index]
}

Personal_monthly_strategist_specialist_hrs_fn <- function(index){
  live$data$`Personal monthly strategist specialist hrs`[index] <- live$data$`Avg personal monthly strategist specialist hrs`[index] * 
    Personal_clients_fn(index)
}

Personal_clients_fn <-function(index){
  live$data$`Personal clients`[index] <- Percent_personal_clients_fn(index) * Total_clients_fn(index)
}

Percent_personal_clients_fn <- function(index){
  live$data$`Percent personal clients`[index] <- 1 - live$data$`Percent enterprise clients`[index] - 
    live$data$`Percent small business clients`[index]
}

#2############################Revenue %#############################################

Revenue_percent_change_fn <- function(index){
  live$data$`Revenue percent change`[index] <- Month_over_month_revenue_fn(index) / Revenue_fn(index-1)
}

Month_over_month_revenue_fn <- function(index){
  live$data$`Month-over-month revenue`[index] <- Revenue_fn(index) - Revenue_fn(index - 1)
}

#3####################





