#We try not to use "return" in any of the functions


Client_growth_fn <- function(percent,dframe,index){
  
}
####################NESTED REVENUE FUCNTIONS#########################################

Revenue_fn <- function(dollars, dframe,index){
  dframe$Revenue[index] <- Enterprise_revenue_fn(dollars, dframe,index) + 
    Small_business_revenue(dollars,dframe,index) + 
    Personal_revenue_fn(dollars,dframe,index)
}

######Enterprise############
Enterprise_revenue_fn <- function(dollars,dframe,index){
  dframe$`Enterprise revenue`[index] <- Total_enterprise_monthly_operator_fees_fn(dollars,dframe,index) + 
    Total_enterprise_monthly_assistant_fees_fn(dollars,dframe,index) + 
    Total_enterprise_monthly_strategist_specialist_fees(dollars,dframe,index)
}

#########
Total_enterprise_monthly_operator_fees_fn <- function(dollars,dframe,index){
  dframe$`Total enterprise monthly operator fees`[index] = 0.8 * dframe$`Operator hrly rate`[index]
}


#######
Total_enterprise_monthly_assistant_fees_fn<- function(dollars,dframe,index){
  dframe$`Total enterprise monthly assistant fees`[index] = Enterprise_monthly_assistant_hrs_fn(dollars,dframe,index) * 
    dframe$`Assistant hrly rate`[index]
  }

Enterprise_monthly_assistant_hrs_fn <- funcion(dollars,dframe,index){
  dframe$`Enterprise monthly assistant hrs` = Enterprise_clients_fn(dollars,dframe,index) * 
    dframe$`Average enterprise monthly operator hrs`[index]
}


#####
Total_enterprise_monthly_strategist_specialist_fees_fn <- function(dollars,dframe,index){
  dframe$`Total enterprise monthly strategist specialist fees`[index] <- 
    Enterprise_monthly_strategist_specialist_hrs_fn(dollars,dframe,index) * dframe$`Strategist specialist hrly rate`[index]
}

Enterprise_monthly_strategist_specialist_hrs_fn <- function(dollars,dframe,index){
  dframe$`Enterprise monthly strategist specialist hrs`[index] <- Enterprise_clients_fn(dollars,dframe,index) * 
    dframe$`Average enterprise monthly strategist specialist hrs`[index]
}

Enterprise_clients_fn <- function(dollars,dframe,index){
  dframe$`Enterprise clients`[index] <- dframe$`% Enterprise client`[index] * Total_clients_fn(dollars,dframe,index)
}

Total_client_fn <- function(dollars,dframe,index){
  dframe$`Total clients`[index] <- (dframe$`Total clients`[index-1]) / 
    (1 + dframe$`Churn percentage weighted by number of clients`[index] - dframe$`Client growth percentage`[index])
}


######Small Buisness#######

Small_business_revenue <- function(dollars,dframe,index){
  dframe$`Small business revenue` = Total_small_business_monthly_operator_fees_fn(dollars,dframe,index) + 
    Total_small_business_monthly_assistant_fees_fn(dollars,dframe,index) + 
    Total_small_business_monthly_strategist_specialist_fees_fn(dollars,dframe,index)
}

#######
Total_small_business_monthly_operator_fees_fn <- fucntion(dollars,dframe,index){
  dframe$`Total small business monthly operator fees`[index] = Small_business_monthly_operator_hrs_fn(dollars,dframe,index) * 
    dframe$`Operator hrly rate`[index]
}

Small_business_monthly_operator_hrs_fn <- function(dollars,dframe,index){
  dframe$`Small business monthly operator hrs`[index] = Small_business_clients_fn(dollars,dframe,index) * 
    dframe$`Average small business monthly operator hrs`[index]
}

########
Total_small_business_monthly_assistant_fees_fn <- function(dollars, dframe, index){
  dframe$`Total small business monthly assistant fees`[index] = Small_business_monthly_assistant_hrs_fn(dollars,dframe,index) * 
    dframe$`Assistant hrly rate`[index]
}

Small_business_monthly_assistant_hrs_fn <- function(dollars,dframe,index){
  dframe$`Small business monthly assistant hrs`[index] = Small_business_clients_fn(dollars,dframe,index) * 
    dframe$`Average small business monthly assistant hrs`[index]
}

######
Total_small_business_monthly_strategist_specialist_fees_fn <- function(dollars,dframe,index){
  dframe$`Total small business monthly strategist specialist fees`[index] = 
    Small_business_monthly_strategist_specialist_hrs_fn(dollars,dframe,index) * dframe$`Strategist specialist hrly rate`[index]
}

Small_business_monthly_strategist_specialist_hrs_fn <- function(dolalrs,dframe,index){
  dframe$`Small business monthly strategist specialist hrs`[index] = Small_business_clients_fn(dollars,dframe,index) * 
    dframe$`Average small business monthly strategist specialist hrs`[index]
}

Small_business_clients_fn <- function(dollars,dframe,index){
  dframe$`Small business clients`[index] = dframe$`% Small business clients`[index] * Total_clients_fn(dollars,dframe,index)
}

########Personal Revenue########

Personal_revenue_fn <-function(dollars,dframe,index){
  dframe$`Personal revenue` <- Total_personal_monthly_operator_fees_fn(dollars,dframe,index) + 
    Total_personal_monthly_assistant_fees_fn(dollars,dframe,index) + 
    Total_personal_monthly_strategist_specialist_fees_fn(dollars,dframe,index)
}


########
Total_personal_monthly_operator_fees_fn <- function(dollars,dframe,index){
  dframe$`Total personal monthly operator fees`[index] <- Personal_monthly_operator_hrs_fn(dollars,dframe,index) * 
    dframe$`Operator hrly rate`[index]
}


Personal_monthly_operator_hrs_fn <- function(dollars,dframe,index){
  dframe$`Personal monthly operator hrs`[index] <- dframe$`Average personal monthly operator hrs`[index] * 
    Personal_clients_fn(dollars,dframe,index)
}

########
Total_personal_monthly_assistant_fees_fn <- function(dollars,dframe,index){
  dframe$`Total personal monthly assistant fees`[index] <- Personal_monthly_assistant_hrs_fn(dollars,dframe,index) * 
    dframe$`Assistant hrly rate`[index]
}

Personal_monthly_assistant_hrs_fn <- function(dollars,dframe,index){
  dframe$`Personal monthly assistant hrs`[index] <- dframe$`Average personal monthly assistant hours`[index] * 
    Personal_clients_fn(dollars,dframe,index)
}

#######
Total_personal_monthly_strategist_specialist_fees_fn <- function(dollars,dframe,index){
  dframe$`Total personal monthly strategist specialist fees`[index] <- Personal_monthly_strategist_specialist_hrs_fn(dollars,dframe,index) * 
    dframe$`Strategist and specialist hrly rate`[index]
}

Personal_monthly_strategist_specialist_hrs_fn <- function(dollars,dframe,index){
  dframe$`Personal_monthly_strategist_specialist_hrs`[index] <- dframe$`Average personal monthly specialist strategist hrs`[index] * 
    Personal_clients_fn(dollars,dframe,index)
}

Personal_clients_fn <-function(dollars,dframe,index){
  dframe$`Personal clients`[index] <- Percent_personal_clients_fn(dollars,dframe,index) * Total_clients_fn(dollars,dframe,index)
}

Percent_personal_clients_fn <- function(dollars,dframe,index){
  dframe$`Percent personal clients`[index] <- 1 - dframe$`Percent enterprise clients`[index] - 
    dframe$`Percent small business clients`[index]
}







