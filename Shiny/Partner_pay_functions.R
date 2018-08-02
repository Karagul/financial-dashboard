source('server.R')
#We try not to use "return" in any of the functions
Current_date = format(Sys.Date(), '%Y-%m-%d')


Salary_before_cap_ppfn <- function(index){
  pp_live$data$`Salary before cap`[index] <- Total_points_before_cap_col_ppfn(index) * Point_value_before_cap_ppfn()
}

############
Point_value_before_cap_ppfn <- function(){
  pp_live$data$Number[10] <- Gross_profits_ppfn() / Total_points_before_cap_ppfn()
    
}

Gross_profits_ppfn <- function(){
  pp_live$data$Number[3] <- as.numeric(pp_live$data$Number[1]) * as.numeric(pp_live$data$Number[2]) #rev * gross margins
}

Total_points_before_cap_ppfn <- function(){
  tot_point <- 0
  i <- 1
  while(i <= 27){
    tot_point <- tot_point + Total_points_before_cap_col_ppfn(i)
    i <- i + 1
  }
  pp_live$data$Number[9] <- tot_point
}


Total_points_before_cap_col_ppfn <- function(index){
  pp_live$data$`Total points before cap`[index] <- Experience_points_ppfn(index) + pp_live$data$`New merit`[index] +
    pp_live$data$`Historical merit`[index]
}


Experience_points_ppfn <- function(index){
  pp_live$data$`Experience points`[index] <- pp_live$data$`Today minus start date`[index] * (12/365) + 
    Tier_points_ppfn(index)
}

Tier_points_ppfn <- function(index){
  if(pp_live$data$Tier[index] == "Tier 1"){
    pp_live$data$`Tier points`[index] <- 8
  }
  else if(pp_live$data$Tier[index] == "Tier 2"){
    pp_live$data$`Tier points`[index] <- 6
  }
  else if(pp_live$data$Tier[index] == "Tier 3"){
    pp_live$data$`Tier points`[index] <- 4
  }
  else if(pp_live$data$Tier[index] == "Tier 4"){
    pp_live$data$`Tier points`[index] <- 2
  }
}


#########################################################################################################################################

Salary_after_cap_ppfn <- function(){
  i = 1
  while(i <= 27){
    pp_live$data$`Salary after cap`[i] <- Salary_before_cap_ppfn(i)
    i <- i + 1
  }
  handout_ppfn()
}

handout_ppfn <- function(){
  additional_sal <- 0
  Tot_points_minus_those_who_hit_cap <- 0
  #Salary Cap = pp_live$data$Number[5]
  i = 1
  while(i <= 27){
    if(pp_live$data$`Salary after cap`[i] > as.numeric(pp_live$data$Number[5])){
     additional_sal <- additional_sal + pp_live$data$`Salary after cap`[i]  - as.numeric(pp_live$data$Number[5])
     pp_live$data$`Salary after cap`[i] <- as.numeric(pp_live$data$Number[5])
     Tot_points_minus_those_who_hit_cap <- Tot_points_minus_those_who_hit_cap + Total_points_before_cap_col_ppfn(i)
    }
    i <- i + 1
  }

  Tot_points_minus_those_who_hit_cap <- as.numeric(pp_live$data$Number[9]) - Tot_points_minus_those_who_hit_cap
  n <- 1
  while(n <= 27){
    if(pp_live$data$`Salary after cap`[n] < as.numeric(pp_live$data$Number[5])){
      pp_live$data$`Salary after cap`[n] <- pp_live$data$`Salary after cap`[n] +
        (additional_sal * (Total_points_before_cap_col_ppfn(n) / Tot_points_minus_those_who_hit_cap))
    }
    n <- n + 1
  }

  #Recursive part(will only be meaningful if someone goes over cap when given handout)
  tfbool = 0;
  z <- 1
  while(z <= 27){
    if(pp_live$data$`Salary after cap`[z] > as.numeric(pp_live$data$Number[5])){
       tfbool = 1;
    }
    z <- z + 1
  }
  if(tfbool == 1){
    handout_ppfn();
  }
}


############fucntion neccessary to partner pay data table but dont effect calucllations##########
Revenue_total_maxing_out_all_partners_caps_ppfn <- function(){
  pp_live$data$Number[6] <- (as.numeric(pp_live$data$Number[5]) * as.numeric(pp_live$data$Number[8])) / as.numeric(pp_live$data$Number[2])
}

Partners_hitting_cap_ppfn <- function(){
  part_val <- 0
  i <- 1
  while(i <= 27){
    if(pp_live$data$`Salary after cap`[i] >= as.numeric(pp_live$data$Number[5])){
      part_val <- part_val + 1 
    }
    i <- i + 1
  }
  pp_live$data$Number[13] <- part_val
}





