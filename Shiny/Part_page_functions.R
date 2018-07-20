source('server.R')
#We try not to use "return" in any of the functions

#1###################NESTED REVENUE FUCNTIONS#########################################called in Client growth slider

Revenue_partfn <- function(index){
  part_live$data$Revenue[index] <- Enterprise_revenue_partfn(index) + 
    Small_business_revenue_partfn(index) + 
    Personal_revenue_partfn(index)
}

######Enterprise############
Enterprise_revenue_partfn <- function(index){
  part_live$data$`Enterprise revenue`[index] <- Total_enterprise_monthly_operator_fees_partfn(index) + 
    Total_enterprise_monthly_assistant_fees_partfn(index) + 
    Total_enterprise_monthly_specialist_fees_partfn(index) +
    Total_enterprise_monthly_strategist_fees_partfn(index)
}

#########
Total_enterprise_monthly_operator_fees_partfn <- function(index){
  part_live$data$`Total enterprise monthly operator fees`[index] <- Enterprise_monthly_operator_hrs_partfn(index) * 
    part_live$data$`Operator hrly rate`[index]
}
Enterprise_monthly_operator_hrs_partfn <- function(index){
  part_live$data$`Enterprise monthly operator hrs`[index] <- Enterprise_clients_partfn(index) * 
    part_live$data$`Avg enterprise monthly operator hrs`[index]
}



#######
Total_enterprise_monthly_assistant_fees_partfn<- function(index){
  part_live$data$`Total enterprise monthly assistant fees`[index] <- Enterprise_monthly_assistant_hrs_partfn(index) * 
    part_live$data$`Assistant hrly rate`[index]
}

Enterprise_monthly_assistant_hrs_partfn <- function(index){
  part_live$data$`Enterprise monthly assistant hrs`[index] <- Enterprise_clients_partfn(index) * 
    part_live$data$`Avg enterprise monthly assistant hrs`[index]
}

#####
Total_enterprise_monthly_specialist_fees_partfn <- function(index){
  part_live$data$`Total enterprise monthly specialist fees`[index] <- 
    Enterprise_monthly_specialist_hrs_partfn(index) * part_live$data$`Specialist hrly rate`[index]
}

Enterprise_monthly_specialist_hrs_partfn <- function(index){
  part_live$data$`Enterprise monthly specialist hrs`[index] <- Enterprise_clients_partfn(index) * 
    part_live$data$`Avg enterprise monthly specialist hrs`[index]
}

#####
Total_enterprise_monthly_strategist_fees_partfn <- function(index){
  part_live$data$`Total enterprise monthly strategist fees`[index] <- 
    Enterprise_monthly_strategist_hrs_partfn(index) * part_live$data$`Strategist hrly rate`[index]
}

Enterprise_monthly_strategist_hrs_partfn <- function(index){
  part_live$data$`Enterprise monthly strategist hrs`[index] <- Enterprise_clients_partfn(index) * 
    part_live$data$`Avg enterprise monthly strategist hrs`[index]
}

Enterprise_clients_partfn <- function(index){
  part_live$data$`Enterprise clients`[index] <- part_live$data$`Percent enterprise clients`[index] * Total_clients_partfn(index)
}


######Small Buisness#######

Small_business_revenue_partfn <- function(index){
  part_live$data$`Small business revenue`[index] <- Total_small_business_monthly_operator_fees_partfn(index) + 
    Total_small_business_monthly_assistant_fees_partfn(index) + 
    Total_small_business_monthly_specialist_fees_partfn(index) +
    Total_small_business_monthly_strategist_fees_partfn(index)
}

#######
Total_small_business_monthly_operator_fees_partfn <- function(index){
  part_live$data$`Total small business monthly operator fees`[index] <- Small_business_monthly_operator_hrs_partfn(index) * 
    part_live$data$`Operator hrly rate`[index]
}

Small_business_monthly_operator_hrs_partfn <- function(index){
  part_live$data$`Small business monthly operator hrs`[index] <- Small_business_clients_partfn(index) * 
    part_live$data$`Avg small business monthly operator hrs`[index]
}

########
Total_small_business_monthly_assistant_fees_partfn <- function( index){
  part_live$data$`Total small business monthly assistant fees`[index] <- Small_business_monthly_assistant_hrs_partfn(index) * 
    part_live$data$`Assistant hrly rate`[index]
}

Small_business_monthly_assistant_hrs_partfn <- function(index){
  part_live$data$`Small business monthly assistant hrs`[index] <- Small_business_clients_partfn(index) * 
    part_live$data$`Avg small business monthly assistant hrs`[index]
}

######
Total_small_business_monthly_specialist_fees_partfn <- function(index){
  part_live$data$`Total small business monthly specialist fees`[index] <- 
    Small_business_monthly_specialist_hrs_partfn(index) * part_live$data$`Specialist hrly rate`[index]
}

Small_business_monthly_specialist_hrs_partfn <- function(index){
  part_live$data$`Small business monthly specialist hrs`[index] <- Small_business_clients_partfn(index) * 
    part_live$data$`Avg small business monthly specialist hrs`[index]
}

######
Total_small_business_monthly_strategist_fees_partfn <- function(index){
  part_live$data$`Total small business monthly strategist fees`[index] <- 
    Small_business_monthly_strategist_hrs_partfn(index) * part_live$data$`Strategist hrly rate`[index]
}

Small_business_monthly_strategist_hrs_partfn <- function(index){
  part_live$data$`Small business monthly strategist hrs`[index] <- Small_business_clients_partfn(index) * 
    part_live$data$`Avg small business monthly strategist hrs`[index]
}

Small_business_clients_partfn <- function(index){
  part_live$data$`Small business clients`[index] <- part_live$data$`Percent small business clients`[index] * Total_clients_partfn(index)
}

########Personal Revenue########

Personal_revenue_partfn <-function(index){
  part_live$data$`Personal revenue`[index] <- Total_personal_monthly_operator_fees_partfn(index) + 
    Total_personal_monthly_assistant_fees_partfn(index) + 
    Total_personal_monthly_specialist_fees_partfn(index) + 
    Total_personal_monthly_strategist_fees_partfn(index)
}


########
Total_personal_monthly_operator_fees_partfn <- function(index){
  part_live$data$`Total personal monthly operator fees`[index] <- Personal_monthly_operator_hrs_partfn(index) * 
    part_live$data$`Operator hrly rate`[index]
}


Personal_monthly_operator_hrs_partfn <- function(index){
  part_live$data$`Personal monthly operator hrs`[index] <- part_live$data$`Avg personal monthly operator hrs`[index] * 
    Personal_clients_partfn(index)
}

########
Total_personal_monthly_assistant_fees_partfn <- function(index){
  part_live$data$`Total personal monthly assistant fees`[index] <- Personal_monthly_assistant_hrs_partfn(index) * 
    part_live$data$`Assistant hrly rate`[index]
}

Personal_monthly_assistant_hrs_partfn <- function(index){
  part_live$data$`Personal monthly assistant hrs`[index] <- part_live$data$`Avg personal monthly assistant hrs`[index] * 
    Personal_clients_partfn(index)
}

#######
Total_personal_monthly_specialist_fees_partfn <- function(index){
  part_live$data$`Total personal monthly specialist fees`[index] <- Personal_monthly_specialist_hrs_partfn(index) * 
    part_live$data$`Specialist hrly rate`[index]
}

Personal_monthly_specialist_hrs_partfn <- function(index){
  part_live$data$`Personal monthly specialist hrs`[index] <- part_live$data$`Avg personal monthly specialist hrs`[index] * 
    Personal_clients_partfn(index)
}

#######
Total_personal_monthly_strategist_fees_partfn <- function(index){
  part_live$data$`Total personal monthly strategist fees`[index] <- Personal_monthly_strategist_hrs_partfn(index) * 
    part_live$data$`Strategist hrly rate`[index]
}

Personal_monthly_strategist_hrs_partfn <- function(index){
  part_live$data$`Personal monthly strategist hrs`[index] <- part_live$data$`Avg personal monthly strategist hrs`[index] * 
    Personal_clients_partfn(index)
}

Personal_clients_partfn <-function(index){
  part_live$data$`Personal clients`[index] <- Percent_personal_clients_partfn(index) * Total_clients_partfn(index)
}

Percent_personal_clients_partfn <- function(index){
  part_live$data$`Percent personal clients`[index] <- 1 - part_live$data$`Percent enterprise clients`[index] - 
    part_live$data$`Percent small business clients`[index]
}

#2############################Revenue %#############################################called in Client growth slider

Revenue_percent_change_partfn <- function(index){
  part_live$data$`Revenue percent change`[index] <- Month_over_month_revenue_partfn(index) / part_live$data$Revenue[index-1]
}

Month_over_month_revenue_partfn <- function(index){
  part_live$data$`Month over month revenue`[index] <- part_live$data$Revenue[index] - part_live$data$Revenue[index-1]
}

#3####################Total Clients#################

Total_clients_partfn <- function(index){
  part_live$data$`Total clients`[index] <- (part_live$data$`Total clients`[index-1]) / 
    (1 + part_live$data$`Churn percentage weighted by number of clients`[index] - part_live$data$`Client growth percentage`[index])
}

#4##############Total Monthly ARPA##########################called in Client growth slider


Total_monthly_ARPA_partfn <- function(index){
  part_live$data$`Total monthly ARPA`[index] <- part_live$data$Revenue[index] / Total_clients_partfn(index)
}


#6####################Client Growth After Churn##############called in Client growth slider

Client_growth_after_churn_partfn <- function(index){
  part_live$data$`Client growth after churn`[index] <- part_live$data$`Client growth percentage`[index] - 
    part_live$data$`Churn percentage weighted by number of clients`[index]
}

#7#################Avg Customer Lifetime (months)###################
Avg_customer_lifetime_months_partfn <- function(index){
  part_live$data$`Avg customer lifetime in months`[index] <- 1 / part_live$data$`Churn percentage weighted by number of clients`[index]
}

#8#####################CLTV#############################

CLTV_partfn <- function(index){
  part_live$data$`CLTV`[index] <- (Total_monthly_ARPA_partfn(index) * Avg_customer_lifetime_months_partfn(index)) - part_live$data$`CAC`[index]
}

#10###################CLTV/CAC##################

CLTV_to_CAC_ratio_partfn <- function(index){
  part_live$data$`CLTV to CAC ratio`[index] <- CLTV_partfn(index) / part_live$data$CAC[index]
}

#11######################Company Head Count##################called in Client growth slider
Company_head_count_partfn <- function(index){
  part_live$data$`Company head count`[index] <- part_live$data$`Number of partners`[index] + Number_of_agents_partfn(index)
}


#12###################Agents#################

Number_of_agents_partfn <- function(index){
  part_live$data$`Number of agents`[index] <- 1.1 * Total_agents_not_including_set_managers_partfn(index)
}

Total_agents_not_including_set_managers_partfn <- function(index) {
  part_live$data$`Total agents not including set managers`[index] <- Number_of_operators_invisible_decides_to_employ_this_month_partfn(index) + 
    Number_of_RRRs_invisible_decides_to_employ_this_month_partfn(index) + 
    Number_of_specialists_invisible_decides_to_employ_this_month_partfn(index) +
    Number_of_strategists_invisible_decides_to_employ_this_month_partfn(index)
}

#######################Number of operators invisible decides to employ this month
Number_of_operators_invisible_decides_to_employ_this_month_partfn <- function(index){
  part_live$data$`Number of operators invisible decides to employ this month`[index] <-  
    (Actual_client_hrs_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked_partfn(index) + 
       Non_billable_operator_RRR_and_sentry_hrs_partfn(index)) / (part_live$data$`Avg hrs per week per operator`[index] * 4.35)
}


Non_billable_operator_RRR_and_sentry_hrs_partfn <- function(index){
  part_live$data$`Non billable operator RRR and sentry hrs`[index] <- Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs_partfn(index) * 
    part_live$data$`Percent of operator sentry and RRR hrs that are NOT client billable`[index]
} 

Actual_client_hrs_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked_partfn <- function(index){
  part_live$data$`Actual client hrs operator sentry and ten dollar per hr RRR hrs worked`[index] <- 
    Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs_partfn(index) * 
    part_live$data$`Reduction in agent task completion times relative to price benchmark`[index]
}


Billable_operator_sentry_and_10_dollar_per_hr_RRR_hrs_partfn <- function(index){
  part_live$data$`Billable operator sentry and 10 dollar per hr RRR hrs`[index] <- Enterprise_monthly_operator_hrs_partfn(index) + 
    Small_business_monthly_operator_hrs_partfn(index) + Personal_monthly_operator_hrs_partfn(index)
} 

##############Not needed?
# Personal_monthly_operator_hrs_partfn <- function(index){
#   part_live$data$`Personal monthly operator hrs`[index] <- part_live$data$`Avg personal monthly operator hrs`[index] * Personal_clients_partfn(index)
# }


###################Number of RRRs invisible decides to employ this month

Number_of_RRRs_invisible_decides_to_employ_this_month_partfn <- function(index){
  part_live$data$`Number of RRRs invisible decides to employ this month`[index] <-  
    (Actual_client_hrs_RRRs_worked_partfn(index)) /
    (part_live$data$`Avg hrs per week per RRR`[index] * 4.35)
}

Actual_client_hrs_RRRs_worked_partfn <- function(index){
  part_live$data$`Actual client hrs RRRs worked`[index] <- Billable_RRR_hrs_partfn(index) * 
    part_live$data$`Reduction in agent task completion times relative to price benchmark`[index]
}

Billable_RRR_hrs_partfn <- function(index) {
  part_live$data$`Billable RRR hrs`[index] <- Enterprise_monthly_assistant_hrs_partfn(index) + Small_business_monthly_assistant_hrs_partfn(index) + 
    Personal_monthly_assistant_hrs_partfn(index)
}

############Number of specialists invisible decides to employ this month
Number_of_specialists_invisible_decides_to_employ_this_month_partfn <- function(index){
  part_live$data$`Number of specialists invisible decides to employ this month`[index] <-
    (Actual_client_hrs_specialists_worked_partfn(index)) / 
    (part_live$data$`Avg hrs per week per specialists`[index] * 4.35)
}

Actual_client_hrs_specialists_worked_partfn <- function(index){
  part_live$data$`Actual client hrs specialists worked`[index] <- Billable_specialist_hrs_partfn(index) * 
    part_live$data$`Reduction in agent task completion times relative to price benchmark`[index]
}

Billable_specialist_hrs_partfn <- function(index){
  part_live$data$`Billable specialist hrs`[index] <- Enterprise_monthly_specialist_hrs_partfn(index) + 
    Small_business_monthly_specialist_hrs_partfn(index) + Personal_monthly_specialist_hrs_partfn(index)
}

##################Number of strategists invisible decides to employ this month
Number_of_strategists_invisible_decides_to_employ_this_month_partfn <- function(index){
  part_live$data$`Number of strategists invisible decides to employ this month`[index] <-
    (Actual_client_hrs_strategists_worked_partfn(index)) / 
    (part_live$data$`Avg hrs per week per strategists`[index] * 4.35)
}

Actual_client_hrs_strategists_worked_partfn <- function(index){
  part_live$data$`Actual client hrs strategists worked`[index] <- Billable_strategist_hrs_partfn(index) * 
    part_live$data$`Reduction in agent task completion times relative to price benchmark`[index]
}

Billable_strategist_hrs_partfn <- function(index){
  part_live$data$`Billable strategist hrs`[index] <- Enterprise_monthly_strategist_hrs_partfn(index) + 
    Small_business_monthly_strategist_hrs_partfn(index) + Personal_monthly_strategist_hrs_partfn(index)
}

#13##############Actual Labor Costs###############
Actual_labor_costs_partfn <- function(index){
  part_live$data$`Actual labor costs`[index] <- Expected_operator_labor_costs_partfn(index) + 
    Expected_RRR_labor_costs_for_assistants_partfn(index) + Expected_specialist_labor_costs_partfn(index) + 
    Expected_strategist_labor_costs_partfn(index)
}
###############Expected operator labor costs
Expected_operator_labor_costs_partfn <- function(index){
  part_live$data$`Expected operator labor costs`[index] <- 
    (Actual_client_hrs_operator_sentry_and_ten_dollar_per_hr_RRR_hrs_worked_partfn(index) + 
       Non_billable_operator_RRR_and_sentry_hrs_partfn(index)) * 
    part_live$data$`Avg rate for operators sentries and RRRs working for clients at 10 dollars per hr`[index]
}

################Expected RRR labor costs for assistants
Expected_RRR_labor_costs_for_assistants_partfn <- function(index){
  part_live$data$`Expected RRR labor costs for assistants`[index] <- Actual_client_hrs_RRRs_worked_partfn(index) * 
    part_live$data$`Avg RRR rate working for clients at 20 dollars per hr`[index]
}

###################Expected specialist labor costs
Expected_specialist_labor_costs_partfn <- function(index){
  part_live$data$`Expected specialist labor costs`[index] <- ((Actual_client_hrs_specialists_worked_partfn(index) *
                                                            part_live$data$`Avg specialist rate`[index]))
}

###################Expected strategist labor costs
Expected_strategist_labor_costs_partfn <- function(index){
  part_live$data$`Expected strategist labor costs`[index] <- ((Actual_client_hrs_strategists_worked_partfn(index) *
                                                            part_live$data$`Avg strategist rate`[index]))
}


#14###########Revenue per Head####################running in client growth slider#########

Revenue_per_head_partfn <- function(index){
  part_live$data$`Revenue per head`[index] <- part_live$data$Revenue[index] / (part_live$data$`Number of agents`[index] + 
                                                                       part_live$data$`Number of partners`[index])  
}

#15###################### Gross Profit#############running in client growth slider
Gross_profit_partfn <- function(index){
  part_live$data$`Gross profit`[index] <- part_live$data$Revenue[index] - part_live$data$`Actual labor costs`[index]
}

#16######################Gross Margins##############running in client growth slider
Gross_margins_partfn <- function(index){
  part_live$data$`Gross margins`[index] <- Gross_profit_partfn(index) /  part_live$data$Revenue[index]
}

#18######################Total Partner pay########## used in client growth slider
# Total_partner_pay_partfn <- function(index){
#   part_live$data$`Total partner pay`[index] <- part_live$data$`Number of partners`[index] * Avg_partner_salary_partfn(index)
# }
# 
# Avg_partner_salary_partfn <- function(index){
#   part_live$data$`Avg partner salary`[index] <-  part_live$data$`Avg salary cap per partner`[index] + Avg_dollars_shy_of_partner_salary_cap_partfn(index)
# }
# 
# Avg_dollars_shy_of_partner_salary_cap_partfn <- function(index){
#   if((Gross_margins_split_pre_partner_pay_partfn(index) - part_live$data$`Avg salary cap per partner`[index]) > 0){
#     part_live$data$`Avg dollars shy of partner salary cap`[index] = 0
#   }
#   else {
#   part_live$data$`Avg dollars shy of partner salary cap`[index] <- Gross_margins_split_pre_partner_pay_partfn(index) -
#     part_live$data$`Avg salary cap per partner`[index]
#   }
# }
# 
# Gross_margins_split_pre_partner_pay_partfn <- function(index){
#   part_live$data$`Gross margins split pre partner pay`[index] <- Gross_profit_partfn(index) / part_live$data$`Number of partners`[index]
# }

###############Question???????????????????
Total_partner_pay_partfn <- function(index){
  part_live$data$`Total partner pay`[index] <- part_live$data$`Number of partners`[index] * Avg_partner_salary_partfn(index)
}

Avg_partner_salary_partfn <- function(index){
  part_live$data$`Avg partner salary`[index] <-  part_live$data$`Avg salary cap per partner`[index] + 
    part_live$data$`Avg dollars shy of partner salary cap`[index]
}



#20#####################Sales and marketing costs
Sales_and_marketing_costs_partfn <- function(index){
  part_live$data$`Sales and marketing costs`[index] <- Commissions_partfn(index) + Growth_software_costs_partfn(index) + Advertising_costs_partfn(index) + 
    Sales_agents_partfn(index) + Invisible_sales_processes_partfn(index)
}

Commissions_partfn <- function(index){
  part_live$data$`Commissions`[index] <- part_live$data$Revenue[index] * part_live$data$`Percent commission`[index]
}

Growth_software_costs_partfn <- function(index){
  part_live$data$`Growth software`[index] <-part_live$data$`Growth software`[index - 1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}

Advertising_costs_partfn <- function(index){
  part_live$data$`Advertising costs`[index] <-part_live$data$`Advertising costs`[index - 1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}
Sales_agents_partfn <- function(index){
  part_live$data$`Sales agents`[index] <-part_live$data$`Sales agents`[index - 1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}
Invisible_sales_processes_partfn <- function(index){
  part_live$data$`Invisible sales processes`[index] <-part_live$data$`Invisible sales processes`[index - 1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}



#21####################Subscrption costs
Additional_subscription_costs_partfn <- function(index){
  part_live$data$`Additional subscription costs`[index] <- part_live$data$`Additional subscription costs`[index - 1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}


#22#################RandD costs

Total_R_and_D_costs_partfn <- function(index){
  part_live$data$`Total R and D costs`[index] <- part_live$data$`Total R and D costs`[index - 1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}


#23####################BD Costs

BD_costs_partfn <- function(index){
  part_live$data$`BD costs`[index]<- part_live$data$`BD costs`[index-1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}


#24####################Discretionary Spending 
Discretionary_spending_partfn <- function(index){
  part_live$data$`Discretionary spending`[index]<- part_live$data$`Discretionary spending`[index-1] * 
    (1 + part_live$data$`Composite costs multiplier`[index])
}

#25#####################overhead

Overhead_partfn <- function(index){
  part_live$data$Overhead[index] <- Sales_and_marketing_costs_partfn(index) + Additional_subscription_costs_partfn(index) + 
    BD_costs_partfn(index) + Total_R_and_D_costs_partfn(index) + Discretionary_spending_partfn(index)
}

#26####################Overhead to Opex
Overhead_to_opex_partfn <- function(index){
  part_live$data$`Overhead to opex`[index] <- part_live$data$Overhead[index] / (Opex_partfn(index))
}

Opex_partfn <- function(index) {
  part_live$data$Opex[index] <- part_live$data$Overhead[index] + part_live$data$`Actual labor costs`[index]
}
#27####################Net profit

Net_profit_partfn <- function(index) {
  part_live$data$`Net profit`[index] <- part_live$data$`Gross profit`[index] - part_live$data$`Total partner pay`[index] - Burn_partfn(index)
}

#28####################Net Margins

Net_margins_partfn <- function(index) {
  part_live$data$`Net margins`[index] <- part_live$data$`Net profit`[index] / part_live$data$Revenue[index]
}

#29#####################


#30#####################
Cash_in_bank_partfn <- function(index){
  part_live$data$`Cash in bank`[index] <- part_live$data$`Cash in bank`[index - 1] + part_live$data$`Net profit`[index] + part_live$data$`Funds raised`[index]
}


#31######################Burn
###############PArtner Bonuses?????????????????

Burn_partfn <- function(index){
  part_live$data$Burn[index] <- part_live$data$`Partner bonuses`[index] + Sales_and_marketing_costs_partfn(index) + Additional_subscription_costs_partfn(index) + 
    Total_R_and_D_costs_partfn(index) + BD_costs_partfn(index) + Discretionary_spending_partfn(index)
}



###########################################################################################################################################

#Costs Multiplier Slider

Composite_costs_multiplier_partfn <- function(index, percent){
  part_live$data$`Overhead to revenue ratio`[index] <- percent
  part_live$data$`Composite costs multiplier`[index] <- Revenue_percent_change_partfn(index) * percent
}




