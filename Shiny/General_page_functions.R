source('server.R')
#We try not to use "return" in any of the functions

#1###################NESTED REVENUE FUCNTIONS#########################################called in Client growth slider

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
  live$data$`Total enterprise monthly operator fees`[index] <- Enterprise_monthly_operator_hrs_fn(index) * 
    live$data$`Operator hrly rate`[index]
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

#2############################Revenue %#############################################called in Client growth slider

Revenue_percent_change_fn <- function(index){
  live$data$`Revenue percent change`[index] <- Month_over_month_revenue_fn(index) / live$data$Revenue[index-1]
}

Month_over_month_revenue_fn <- function(index){
  live$data$`Month over month revenue`[index] <- live$data$Revenue[index] - live$data$Revenue[index-1]
}

#3####################Total Clients#################

Total_clients_fn <- function(index){
  live$data$`Total clients`[index] <- (live$data$`Total clients`[index-1]) / 
    (1 + live$data$`Churn percentage weighted by number of clients`[index] - live$data$`Client growth percentage`[index])
}

#4##############Total Monthly ARPA##########################called in Client growth slider


Total_monthly_ARPA_fn <- function(index){
  live$data$`Total monthly ARPA`[index] <- live$data$Revenue[index] / Total_clients_fn(index)
}


#5#####################Churn############# May not be neccessary##########

# Churn_fn <- function(index){
#   live$data$`Churn percentage weighted by number of clients`[index] <- live$data$`Client churn`[index]
# }


#6####################Client Growth After Churn##############called in Client growth slider

Client_growth_after_churn_fn <- function(index){
  live$data$`Client growth after churn`[index] <- live$data$`Client growth percentage`[index] - 
    live$data$`Churn percentage weighted by number of clients`[index]
}

#7#################Avg Customer Lifetime (months)###################
Avg_customer_lifetime_months_fn <- function(index){
  live$data$`Avg customer lifetime in months`[index] <- 1 / live$data$`Churn percentage weighted by number of clients`[index]
}

#8#####################CLTV#############################

CLTV_fn <- function(index){
  live$data$`CLTV`[index] <- (Total_monthly_ARPA_fn(index) * Avg_customer_lifetime_months_fn(index)) - live$data$`CAC`[index]
}

#10###################CLTV/CAC##################

CLTV_to_CAC_ratio_fn <- function(index){
  live$data$`CLTV to CAC ratio`[index] <- CLTV_fn(index) / live$data$CAC[index]
}

#11######################Company Head Count##################called in Client growth slider
Company_head_count_fn <- function(index){
  live$data$`Company head count`[index] <- live$data$`Number of partners`[index] + Number_of_agents_fn(index)
}


#12###################Agents#################

Number_of_agents_fn <- function(index){
  live$data$`Number of agents`[index] <- 1.1 * Total_agents_not_including_set_managers_fn(index)
}

Total_agents_not_including_set_managers_fn <- function(index) {
  live$data$`Total agents not including set managers`[index] <- Number_of_operators_invisible_decides_to_employ_this_month_fn(index) + 
    Number_of_RRRs_invisible_decides_to_employ_this_month_fn(index) + 
    Number_of_specialists_and_strategists_invisible_decides_to_employ_this_month_fn(index)
}

#######################Number of operators invisible decides to employ this month
Number_of_operators_invisible_decides_to_employ_this_month_fn <- function(index){
  live$data$`Number of operators invisible decides to employ this month`[index] <-  
    (Actual_client_hours_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked_fn(index) + 
       Non_billable_operator_RRR_and_sentry_hrs_fn(index)) / (live$data$`Avg hrs per week per operator`[index] * 4.35)
}


Non_billable_operator_RRR_and_sentry_hrs_fn <- function(index){
  live$data$`Non billable operator RRR and sentry hrs`[index] <- Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs_fn(index) * 
    live$data$`Percent of operator sentry and RRR hours that are NOT client billable`[index]
} 

#not needed?
Percent_of_total_agent_workforce_hrs_made_up_by_sentries_fn <- function(index){
  live$data$`Percent of total agent workforce hrs made up by sentries`[index] <- 
    live$data$`Percent of total agent workforce hrs made up by sentries`[index - 1] - .0016
}

Actual_client_hours_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked_fn <- function(index){
  live$data$`Actual client hours operator sentry and ten dollar per hr RRR hrs worked`[index] <- 
    Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs_fn(index) * 
    live$data$`Reduction in agent task completion times relative to price benchmark`[index]
}


Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs_fn <- function(index){
  live$data$`Billable operator sentry and 10 dollar per hr RRR hrs`[index] <- Enterprise_monthly_operator_hrs_fn(index) + 
    Small_business_monthly_operator_hrs_fn(index) + Personal_monthly_operator_hrs_fn(index)
} 

#Not needed?
Personal_monthly_operator_hrs_fn <- function(index){
  live$data$`Personal monthly operator hrs`[index] <- live$data$`Avg personal monthly operator hrs`[index] * Personal_clients_fn(index)
}


###################Number of RRRs invisible decides to employ this month

Number_of_RRRs_invisible_decides_to_employ_this_month_fn <- function(index){
  live$data$`Number of RRRs invisible decides to employ this month`[index] <-  
    (Actual_client_hours_RRRs_worked_fn(index)) /
       (live$data$`Avg hrs per week per RRR`[index] * 4.35)
}


####NOT ACTUALLTY CORRECT##########
##################Number of specialists and strategists invisible decides to employ this month
Number_of_specialists_and_strategists_invisible_decides_to_employ_this_month_fn <- function(index){
  live$data$`Number of specialists and strategists invisible decides to employ this month`[index] <-
     (Actual_client_hrs_specialists_and_strategists_worked_fn(index)) / 
    (live$data$`Avg hrs per week per specialists and strategists`[index] * 4.35)
}

Actual_client_hrs_specialists_and_strategists_worked_fn <- function(index){
  live$data$`Actual client hrs specialists and strategists worked`[index] <- Billable_specialist_and_strategist_hrs_fn(index) * 
    live$data$`Reduction in agent task completion times relative to price benchmark`[index]
}

Billable_specialist_and_strategist_hrs_fn <- function(index){
  live$data$`Billable specialist and strategist hrs`[index] <- Enterprise_monthly_strategist_specialist_hrs_fn(index) + 
    Small_business_monthly_strategist_specialist_hrs_fn(index) + Personal_monthly_strategist_specialist_hrs_fn(index)
}

#13##############Actual Labor Costs###############
Actual_labor_costs_fn <- function(index){
  live$data$`Actual labor costs`[index] <- Expected_operator_labor_costs_fn(index) + 
    Expected_RRR_labor_costs_for_assistants_fn(index) + Expected_specialist_and_strategist_labor_costs_fn(index)
}
###############Expected operator labor costs
Expected_operator_labor_costs_fn <- function(index){
  live$data$`Expected operator labor costs`[index] <- 
    (Actual_client_hours_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked_fn(index) + 
       Non_billable_operator_RRR_and_sentry_hrs_fn(index)) * 
    live$data$`Avg rate for operators sentries and RRRs working for clients at 10 dollars per hr`[index]
}

################Expected RRR labor costs for assistants
Expected_RRR_labor_costs_for_assistants_fn <- function(index){
  live$data$`Expected RRR labor costs for assistants`[index] <- Actual_client_hours_RRRs_worked_fn(index) * 
    live$data$`Avg RRR rate working for clients at 20 dollars per hr`[index]
}

Actual_client_hours_RRRs_worked_fn <- function(index){
  live$data$`Actual client hours RRRs worked`[index] <- Billable_RRR_hrs_fn(index) * 
    live$data$`Reduction in agent task completion times relative to price benchmark`[index]
}

Billable_RRR_hrs_fn <- function(index) {
  live$data$`Billable RRR hrs`[index] <- Enterprise_monthly_assistant_hrs_fn(index) + Small_business_monthly_assistant_hrs_fn(index) + 
    Personal_monthly_assistant_hrs_fn(index)
}

###################Expected specialist and strategist labor costs
Expected_specialist_and_strategist_labor_costs_fn <- function(index){
  live$data$`Expected_specialist_and_strategist_labor_costs` <- ((Actual_client_hrs_specialists_and_strategists_worked_fn(index) *
                                                                   live$data$`Avg sentry rate`[index]))
}



#14###########Revenue per Head####################running in client growth slider#########

Revenue_per_head_fn <- function(index){
  live$data$`Revenue per head`[index] <- live$data$Revenue[index] / (live$data$`Number of agents`[index] + 
                                                                       live$data$`Number of partners`[index])  
}

#15###################### Gross Profit#############running in client growth slider
Gross_profit_fn <- function(index){
  live$data$`Gross profit`[index] <- live$data$Revenue[index] - live$data$`Actual labor costs`[index]
}

#16######################Gross Margins##############running in client growth slider
Gross_margins_fn <- function(index){
  live$data$`Gross margins`[index] <- Gross_profit_fn(index) /  live$data$Revenue[index]
}

#18######################Total Partner pay########## used in client growth slider
Total_partner_pay_fn <- function(index){
  live$data$`Total partner pay`[index] <- live$data$`Number of partners`[index] * Avg_partner_salary_fn(index)
}

Avg_partner_salary_fn <- function(index){
  live$data$`Avg partner salary`[index] <-  live$data$`Avg salary cap per partner`[index] + Avg_dollars_shy_of_partner_salary_cap_fn(index)
}

Avg_dollars_shy_of_partner_salary_cap_fn <- function(index){
  if(Gross_margins_split_pre_partner_pay_fn(index) - live$data$`Avg salary cap per partner`[index] > 0){
    live$data$`Avg dollars shy of partner salary cap`[index] = 0
  }
  else {
  live$data$`Avg dollars shy of partner salary cap`[index] <- Gross_margins_split_pre_partner_pay_fn(index) -
    live$data$`Avg salary cap per partner`[index]
  }
}

Gross_margins_split_pre_partner_pay_fn <- function(index){
  live$data$`Gross margins split pre partner pay`[index] <- Gross_profit_fn(index) / live$data$`Number of partners`[index]
}



#19#######################Partner Bonuses
Partner_bonuses_fn <- function(index){
  live$data$`Partner bonuses`[index] <- live$data$`Partner bonuses`[index]
}



#20#####################Sales and marketing costs
Sales_and_marketing_costs_fn <- function(index){
  live$data$`Sales and marketing costs`[index] <- live$data$Revenue[index] * live$data$`Percent commission`[index]
}

#21####################Subscrption costs
Additional_subscription_costs_fn <- function(index){
  live$data$`Additional subscription costs`[index] <- Engineering_software_fn(index) + Operations_software_fn(index)
}

Engineering_software_fn <- function(index){
  live$data$`Engineering software`[index] <- live$data$`Engineering software`[index-1] + 
    (live$data$`Engineering software`[index-1] * live$data$`Composite costs multiplier`[index])
}


Operations_software_fn <- function(index){
  live$data$`Operations software`[index]<- live$data$`Operations software`[index-1] + 
    (live$data$`Operations software`[index-1] * live$data$`Composite costs multiplier`[index])
}


#22#################RandD costs

Total_R_and_D_costs_fn <- function(index){
  live$data$`Total R and D costs`[index] <- Set_managers_costs_fn(index) + Sales_agents_fn(index) + 
    Trainers_costs_fn(index) + Invisible_Sales_processes_fn(index) + Process_architecture_fn(index)
}

Set_managers_costs_fn <- function(index){
  live$data$`Set managers costs`[index] <- live$data$`Set managers costs`[index-1] + (live$data$`Set managers costs`[index-1] *
                                                                                        live$data$`Composite costs multiplier`[index])
}

Sales_agents_fn <- function(index){
  live$data$`Sales agents`[index] <- live$data$`Sales agents`[index-1] + (live$data$`Sales agents`[index-1] * 
                                                                                        live$data$`Composite costs multiplier`[index])
}

Trainers_costs_fn <- function(index){
  live$data$`Trainers costs`[index] <- live$data$`Trainers costs`[index-1] + (live$data$`Trainers costs`[index-1] * 
                                                                                live$data$`Composite costs multiplier`[index])
}

Invisible_Sales_processes_fn <- function(index){
  live$data$`Invisible sales processes`[index] <- live$data$`Invisible sales processes`[index-1] + 
    (live$data$`Invisible sales processes`[index-1] * live$data$`Composite costs multiplier`[index])
}

Process_architecture_fn <- function(index){
  live$data$`Process architecture`[index] <- live$data$`Process architecture`[index-1] + 
    (live$data$`Process architecture`[index-1] * live$data$`Composite costs multiplier`[index])
}



#23####################BD Costs

BD_costs_fn <- function(index){
  live$data$`BD costs`[index]<- live$data$`BD costs`[index-1] + 
    (live$data$`BD costs`[index-1] * live$data$`Composite costs multiplier`[index])
}


#24####################Discretionary Spending 
Discretionary_spending_fn <- function(index){
  live$data$`Discretionary spending`[index]<- live$data$`Discretionary spending`[index-1] + 
    (live$data$`Discretionary spending`[index-1] * live$data$`Composite costs multiplier`[index])
}

#25#####################overhead

Overhead_fn <- function(index){
  live$data$Overhead[index] <- Total_partner_pay_fn(index) + Partner_bonuses_fn(index) + Additional_subscription_costs_fn(index) + 
    BD_costs_fn(index) + Total_R_and_D_costs_fn(index) + Discretionary_spending_fn(index)
}

#26####################Overhead to Opex
Overhead_to_opex_fn <- function(index){
  live$data$`Overhead to opex`[index] <- live$data$Overhead[index] / (live$data$`Sales and marketing costs`[index] + 
                                                                        live$data$`Actual labor costs`[index])
}

#27####################Net profit

Net_profit_fn <- function(index) {
  live$data$`Net profit`[index] <- live$data$`Gross profit`[index] - live$data$`Total partner pay`[index] - Burn_fn(index)
}

#28####################Net Margins

Net_margins_fn <- function(index) {
  live$data$`Net margins`[index] <- live$data$`Net profit`[index] / live$data$Revenue[index]
}

#29#####################


#30#####################
Cash_in_bank_fn <- function(index){
  live$data$`Cash in bank`[index] <- live$data$`Cash in bank`[index - 1] + live$data$`Net profit`[index] + live$data$`Funds raised`[index]
}


#31######################Burn

Burn_fn <- function(index){
  live$data$Burn[index] <- Partner_bonuses_fn(index) + Sales_and_marketing_costs_fn(index) + Additional_subscription_costs_fn(index) + 
    Total_R_and_D_costs_fn(index) + BD_costs_fn(index) + Discretionary_spending_fn(index)
}



###########################################################################################################################################

#Costs Multiplier Slider

Composite_costs_multiplier_fn <- function(index, percent){
  live$data$`Overhead to revenue ratio`[index] <- percent
  live$data$`Composite costs multiplier`[index] <- Revenue_percent_change_fn(index) * percent
}




