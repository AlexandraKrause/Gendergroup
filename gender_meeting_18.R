library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)

devtools::install_github("eikeluedeling/decisionSupport")
library(decisionSupport)


####first step:get data####

input_table_gender <-read.csv2("./input_table_gender_final_trial_years.csv", dec = ",")
#input_table_gender <-read.csv2("./input_table_gender_final_trial.csv", dec = ",")

input_table_gender <- input_table_gender %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))

str(input_table_gender)
pension_years <- 17
working_years <- 40
var_slight <- 1
# #inputestimates
# #Reminder about the make_variables function
# make_variables <- function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                               as.numeric(x[1,i]),envir=.GlobalEnv)
#  }#Then call:
#    make_variables(as.estimate(input_table_gender))



####function####

# Branch 1 = Default vs. Own branch (way 1, 2, 3)
# Branch 2 = Default vs. Off-Farm Job (Way 4, 5, 6, 7)
# Branch 3 = Default vs. On farm job = Payment of wife (Way 8, 9, 10, 11)
# Branch 4 = Default vs. Family money (way 12, 13, 14) 

decision_function <- function(x, varnames){

  #Agricultural insurance
  Agri_insurance <- c(rep (0,working_years),vv(var_mean = Agri_insurance, 
                                     var_CV = var_slight, 
                                     n = pension_years))
  
  Agri_insurance_inv <- c(vv(var_mean =Agri_insurance_inv, 
                       var_CV = var_slight, 
                       n = working_years), rep(0,pension_years))
  #Private insurance
  Private_insurance_off_farm <- c(rep (0,working_years), vv(var_mean =Private_insurance_off_farm, 
                       var_CV = var_slight, 
                       n = pension_years))
  
  Private_insurance_on_farm <- c(rep (0,working_years), vv(var_mean =Private_insurance_on_farm, 
                                   var_CV = var_slight, 
                                   n = pension_years))
  
  Private_insurance_own_branch <- c(rep (0,working_years),vv(var_mean =Private_insurance_own_branch, 
                                   var_CV = var_slight, 
                                   n = pension_years))
  
  Private_insurance_family_money <- c(rep (0,working_years), vv(var_mean =Private_insurance_family_money, 
                                   var_CV = var_slight, 
                                   n = pension_years))
  
  Private_insurance_inv_off_farm <- c(vv(var_mean =Private_insurance_inv_off_farm, 
                                   var_CV = var_slight, 
                                   n = working_years), rep(0,pension_years))
  
  Private_insurance_inv_on_farm <- c( vv(var_mean =Private_insurance_inv_on_farm, 
                                       var_CV = var_slight, 
                                       n = working_years), rep(0,pension_years))
  
  Private_insurance_inv_own_branch <- c( vv(var_mean =Private_insurance_inv_own_branch, 
                                       var_CV = var_slight, 
                                       n = working_years), rep(0,pension_years))
  
  Private_insurance_inv_family_money <- c( vv(var_mean =Private_insurance_inv_family_money, 
                                       var_CV = var_slight, 
                                       n = working_years), rep(0,pension_years))
  #State insurance
  
  State_insurance_off_farm <- c(rep (0,working_years), vv(var_mean =State_insurance_off_farm, 
                                  var_CV = var_slight, 
                                  n = pension_years))
  
  State_insurance_on_farm <- c(rep (0,working_years), vv(var_mean =State_insurance_on_farm, 
                                 var_CV = var_slight, 
                                 n = pension_years))
  
  State_insurance_inv_off_farm <-c(vv(var_mean =State_insurance_inv_off_farm, 
                                 var_CV = var_slight, 
                                 n = working_years),  rep(0,pension_years))
  
  State_insurance_inv_on_farm <- c(vv(var_mean =State_insurance_inv_on_farm, 
                                     var_CV = var_slight, 
                                     n = working_years), rep(0,pension_years))
  #ETF
  
  
  ETF_off_farm <- c(rep (0,working_years),vv(var_mean =ETF_off_farm, 
                                   var_CV = var_slight, 
                                   n = pension_years))
  
  ETF_on_farm <- c(rep (0,working_years), vv(var_mean =ETF_on_farm, 
                     var_CV = var_slight, 
                     n = pension_years))
  
  ETF_own_branch <- c(rep(0,working_years),vv(var_mean = ETF_own_branch, 
                     var_CV = var_slight, 
                     n = pension_years))
  
  ETF_family_money <- c(rep (0,working_years), vv(var_mean = ETF_family_money, 
                     var_CV = var_slight, 
                     n = pension_years))
  
  
  ETF_inv_off_farm <- c(vv(var_mean =ETF_inv_off_farm, 
                     var_CV = var_slight, 
                     n = working_years), rep(0,pension_years))
  
  ETF_inv_on_farm <- c(vv(var_mean =ETF_inv_on_farm, 
                    var_CV = var_slight, 
                    n = working_years), rep(0,pension_years))
  
  ETF_inv_own_branch <- c(vv(var_mean =ETF_inv_own_branch, 
                       var_CV = var_slight, 
                       n = working_years), rep(0,pension_years))
  
  ETF_inv_family_money <- c( vv(var_mean =ETF_inv_family_money, 
                         var_CV = var_slight, 
                         n = working_years), rep(0,pension_years))
  
  # Mix
  
  Mix_off_farm <- c(rep (0,working_years), vv(var_mean = Mix_off_farm, 
                         var_CV = var_slight, 
                         n = pension_years))
  
  
  Mix_on_farm <- c(rep (0,working_years), vv(var_mean =Mix_on_farm, 
                    var_CV = var_slight, 
                    n = pension_years))
  
  Mix_own_branch <- c(rep (0,working_years), vv(var_mean =Mix_own_branch, 
                       var_CV = var_slight, 
                       n = pension_years))
  
  Mix_family_money <- c(rep (0,working_years), vv(var_mean =Mix_family_money, 
                         var_CV = var_slight, 
                         n = pension_years))
  
  Mix_inv_off_farm <- c(vv(var_mean =Mix_inv_off_farm, 
                         var_CV = var_slight, 
                         n = working_years), rep(0,pension_years))
  
  Mix_inv_on_farm <- c(vv(var_mean =Mix_inv_on_farm, 
                        var_CV = var_slight, 
                        n = working_years), rep(0,pension_years))
  
  Mix_inv_own_branch <- c(vv(var_mean =Mix_inv_own_branch, 
                           var_CV = var_slight, 
                           n = working_years), rep(0,pension_years))
  
  Mix_inv_family_money <- c(vv(var_mean =Mix_inv_family_money, 
                             var_CV = var_slight, 
                             n = working_years), rep(0,pension_years))
  #default option
  
  Default_option <- c(vv(var_mean = Default_option,
                       var_CV = var_slight, 
                       n = working_years), rep(0,pension_years))
  
# Vector with 480 zeros to put in Front of VV-Vectors with length of 2to create time horizons.

#}
  
  #### calculate ex-ante risks ####
  # TO DO: Change variable Name in input table and change the variable here accordingly
  Husband_risk <-
    chance_event(Husband_risk_input, 1, 0, n = 1)
  
  Divorce_risk <-
    chance_event(Divorce_risk_input, 1, 0, n = 1)
  
  Man_Death_risk <-
    chance_event(Man_Death_risk_input, 1, 0, n = 1)
  
  Bancruptcy_risk <-
    chance_event(Bancruptcy_risk_input, 1, 0, n = 1)
  
  
  # Default option: Our decision maker is a farm wife and does nothing special. 
  # Apart from the mandatory agricultural insurance, she invests nothing in her retirement. 
  # Default always same for all 14 options; We use it to compare all other options with
  
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE) 
   

# Branch 1 = Default vs. Own branch (way 1, 2, 3)
  
    # Way 1: She sets up her own business branch. 
    # Here, she continues to be part of the agricultural insurance, 
    # but also invests about 10 % of her income in private insurance. 

    
    profit_with_Own_business_branch_1 <- (Private_insurance_own_branch - Private_insurance_inv_own_branch + Agri_insurance - Agri_insurance_inv) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
 
    
    NPV_profit_with_Own_business_branch_1 <- discount(profit_with_Own_business_branch_1,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_Own_business_branch_1 <- NPV_profit_with_Own_business_branch_1 - NPV_no_branch
    
    
    # Way 2: She sets up her own business branch. 
    # Here, she continues to be part of the agricultural insurance, 
    # but also invests about 10 % of her income in ETF. 
    
    
    profit_with_Own_business_branch_2 <- (ETF_own_branch - ETF_inv_own_branch + Agri_insurance - Agri_insurance_inv) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
    
    
    NPV_profit_with_Own_business_branch_2 <- discount(profit_with_Own_business_branch_2,
                                                      discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_Own_business_branch_2 <- NPV_profit_with_Own_business_branch_2 - NPV_no_branch
    
    # Way 3: She sets up her own business branch. 
    # Here, she continues to be part of the agricultural insurance, 
    # but also invests about 10 % of her income in a Investment-Mix.
    

    profit_with_Own_business_branch_3 <- (Mix_own_branch - Mix_inv_own_branch + Agri_insurance - Agri_insurance_inv) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
    
    
    NPV_profit_with_Own_business_branch_3 <- discount(profit_with_Own_business_branch_3,
                                                      discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_Own_business_branch_3 <- NPV_profit_with_Own_business_branch_3 - NPV_no_branch
    

    
# Branch 2 = Default vs. Off-Farm Job (Way 4, 5, 6, 7)
     
    # Way 4: She gets an off-farm job. 
    # Here, she stops to be part of the agricultural insurance, 
    # Instead, she is part of the mandatory state insurance.
    # She invests no income in pension. 
    
    
    
    profit_with_off_farm_job_4 <- (State_insurance_off_farm - State_insurance_inv_off_farm) * (1- Husband_risk)
    
    
    NPV_profit_with_off_farm_job_4 <- discount(profit_with_off_farm_job_4,
                                                      discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_off_farm_job_4 <- NPV_profit_with_off_farm_job_4 - NPV_no_branch
    
    # Way 5: She gets an off-farm job. 
    # Here, she stops to be part of the agricultural insurance, 
    # Instead, she is part of the mandatory state insurance.
    # She invests a part of her income in private insurance. 
    
  
    
    profit_with_off_farm_job_5 <- (State_insurance_off_farm - State_insurance_inv_off_farm + Private_insurance_off_farm - Private_insurance_inv_off_farm) * (1- Husband_risk)
    
    
    NPV_profit_with_off_farm_job_5 <- discount(profit_with_off_farm_job_5,
                                                      discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_off_farm_job_5 <- NPV_profit_with_off_farm_job_5 - NPV_no_branch
    

    # Way 6: She gets an off-farm job. 
    # Here, she stops to be part of the agricultural insurance, 
    # Instead, she is part of the mandatory state insurance.
    # She invests a part of her income in ETF. 
    
 
    
    profit_with_off_farm_job_6 <- (ETF_off_farm - ETF_inv_off_farm + State_insurance_off_farm - State_insurance_inv_off_farm) * (1- Husband_risk)
    
    
    NPV_profit_with_off_farm_job_6 <- discount(profit_with_off_farm_job_6,
                                               discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_off_farm_job_6 <- NPV_profit_with_off_farm_job_6 - NPV_no_branch
    
    # Way 7: She gets an off-farm job. 
    # Here, she stops to be part of the agricultural insurance, 
    # Instead, she is part of the mandatory state insurance.
    # She invests a part of her income in a mixed investment. 

    
  
    profit_with_off_farm_job_7 <- (Mix_off_farm - Mix_inv_off_farm + State_insurance_off_farm - State_insurance_inv_off_farm) * (1- Husband_risk)
    
    
    NPV_profit_with_off_farm_job_7 <- discount(profit_with_off_farm_job_7,
                                               discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_off_farm_job_7 <- NPV_profit_with_off_farm_job_7 - NPV_no_branch
    

# Branch 3 = Default vs. On farm job = Payment of wife (Way 8)
  
    # Way 8: She gets an official working contract and gets officially paid on farm by her husband.  
    # Here, she stops to be part of the agricultural insurance. 
    # Instead, she is part of the mandatory state insurance.
    # She invests no additional money in her pension.
 
    
    profit_with_on_farm_job_8 <- (State_insurance_on_farm - State_insurance_inv_on_farm) * (1- Husband_risk + Bancruptcy_risk + Divorce_risk + Man_Death_risk)
    
    
    NPV_profit_with_on_farm_job_8 <- discount(profit_with_on_farm_job_8,
                                               discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_on_farm_job_8 <- NPV_profit_with_on_farm_job_8 - NPV_no_branch
    
 
    # Way 9: She gets an official working contract and gets officially paid on farm by her husband.  
    # Here, she stops to be part of the agricultural insurance. 
    # Instead, she is part of the mandatory state insurance.
    # She invests a part of her income in private insurance. 

    
    profit_with_on_farm_job_9 <- (Private_insurance_on_farm - Private_insurance_inv_on_farm + State_insurance_on_farm - State_insurance_inv_on_farm) * (1- Husband_risk + Bancruptcy_risk + Divorce_risk +  Man_Death_risk)
    
    
    NPV_profit_with_on_farm_job_9 <- discount(profit_with_on_farm_job_9,
                                              discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_on_farm_job_9 <- NPV_profit_with_on_farm_job_9 - NPV_no_branch
    
  
    # Way 10: She gets an official working contract and gets officially paid on farm by her husband.  
    # Here, she stops to be part of the agricultural insurance, 
    # Instead, she is part of the mandatory state insurance.
    # She invests a part of her income in ETF. 

    
    profit_with_on_farm_job_10 <- (ETF_on_farm - ETF_inv_on_farm + State_insurance_on_farm - State_insurance_inv_on_farm) * (1- Husband_risk + Bancruptcy_risk + Divorce_risk +  Man_Death_risk)
    
    
    NPV_profit_with_on_farm_job_10 <- discount(profit_with_on_farm_job_10,
                                              discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_on_farm_job_10 <- NPV_profit_with_on_farm_job_10 - NPV_no_branch
    

    # Way 11: She gets an official working contract and gets officially paid on farm by her husband.  
    # Here, she stops to be part of the agricultural insurance, 
    # Instead, she is part of the mandatory state insurance.
    # She invests a part of her income in a mixed investment. 

    
    profit_with_on_farm_job_11 <- (Mix_on_farm - Mix_inv_on_farm + State_insurance_on_farm - State_insurance_inv_on_farm) * (1- Husband_risk + Bancruptcy_risk + Divorce_risk +  Man_Death_risk)
    
    
    NPV_profit_with_on_farm_job_11 <- discount(profit_with_on_farm_job_11,
                                               discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_on_farm_job_11 <- NPV_profit_with_on_farm_job_11 - NPV_no_branch
    
    
# Branch 4 = Default vs. Family money (way 12, 13, 14) 
    
    # Way 12: She convinces her husband to invest in her pension.
    # Here, she continues to be part of the agricultural insurance, 
    # She uses the money from her husband to invest in a private insurance. 
    
    profit_with_family_money_12 <- (Private_insurance_family_money - Private_insurance_inv_family_money + Agri_insurance - Agri_insurance_inv) * (1- Husband_risk + Bancruptcy_risk + Divorce_risk +  Man_Death_risk)
    
    
    NPV_profit_with_family_money_12 <- discount(profit_with_family_money_12,
                                               discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_family_money_12 <- NPV_profit_with_family_money_12 - NPV_no_branch
    
    # Way 13: She convinces her husband to invest in her pension.
    # Here, she continues to be part of the agricultural insurance, 
    # She uses the money from her husband to invest in ETF. 
   
    profit_with_family_money_13 <- (ETF_family_money - ETF_inv_family_money + Agri_insurance - Agri_insurance_inv) * (1- Husband_risk + Bancruptcy_risk + Divorce_risk +  Man_Death_risk)
    
    
    NPV_profit_with_family_money_13 <- discount(profit_with_family_money_13,
                                                discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_family_money_13 <- NPV_profit_with_family_money_13 - NPV_no_branch
    
    # Way 14: She convinces her husband to invest in her pension.
    # Here, she continues to be part of the agricultural insurance, 
    # She uses the money from her husband to invest in a mixed investment. 
    
    profit_with_family_money_14 <- (Mix_family_money - Mix_inv_family_money + Agri_insurance - Agri_insurance_inv) * (1- Husband_risk + Bancruptcy_risk + Divorce_risk +  Man_Death_risk)
    
    
    NPV_profit_with_family_money_14 <- discount(profit_with_family_money_14,
                                                discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision_profit_with_family_money_14 <- NPV_profit_with_family_money_14 - NPV_no_branch
    
    
    
    ####return list####
    
    return(list(NPV_no_branch =  NPV_no_branch,
                
                #way 1
                NPV_profit_with_Own_business_branch_1 =  NPV_profit_with_Own_business_branch_1, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_Own_business_branch_1 = NPV_decision_profit_with_Own_business_branch_1,
                Cashflow_decision_gender_way_A =  profit_with_Own_business_branch_1  - profit_Default,
                
                #way2
                NPV_profit_with_Own_business_branch_2 =  NPV_profit_with_Own_business_branch_2, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_Own_business_branch_2 = NPV_decision_profit_with_Own_business_branch_2,
                Cashflow_decision_gender_way_B = profit_with_Own_business_branch_2  - profit_Default,
                
                #way3
                NPV_profit_with_Own_business_branch_3 =  NPV_profit_with_Own_business_branch_3, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_Own_business_branch_3 = NPV_decision_profit_with_Own_business_branch_3,
                Cashflow_decision_gender_way_C =  profit_with_Own_business_branch_3  - profit_Default,
                
                #way4
                NPV_profit_with_off_farm_job_4 =  NPV_profit_with_off_farm_job_4, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_off_farm_job_4 = NPV_decision_profit_with_off_farm_job_4,
                Cashflow_decision_gender_way_D =  profit_with_off_farm_job_4  - profit_Default,
                
                #way5
                NPV_profit_with_off_farm_job_5 =  NPV_profit_with_off_farm_job_5, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_off_farm_job_5 = NPV_decision_profit_with_off_farm_job_5,
                Cashflow_decision_gender_way_E =  profit_with_off_farm_job_5  - profit_Default,
                
                #way6
                NPV_profit_with_off_farm_job_6 =  NPV_profit_with_off_farm_job_6, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_off_farm_job_6 = NPV_decision_profit_with_off_farm_job_6,
                Cashflow_decision_gender_way_F =  profit_with_off_farm_job_6  - profit_Default,
                
                #way7
                NPV_profit_with_off_farm_job_7 =  NPV_profit_with_off_farm_job_7, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_off_farm_job_7 = NPV_decision_profit_with_off_farm_job_7,
                Cashflow_decision_gender_way_G =  profit_with_off_farm_job_7  - profit_Default,
                
                #way8
                NPV_profit_with_on_farm_job_8 =  NPV_profit_with_on_farm_job_8, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_on_farm_job_8 = NPV_decision_profit_with_on_farm_job_8,
                Cashflow_decision_gender_way_H =  profit_with_on_farm_job_8  - profit_Default,
              
                #way9
                NPV_profit_with_on_farm_job_9 =  NPV_profit_with_on_farm_job_9, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_on_farm_job_9 = NPV_decision_profit_with_on_farm_job_9,
                Cashflow_decision_gender_way_I =  profit_with_on_farm_job_9  - profit_Default,
                
                #way10
                NPV_profit_with_on_farm_job_10 =  NPV_profit_with_on_farm_job_10, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_on_farm_job_10 = NPV_decision_profit_with_on_farm_job_10,
                Cashflow_decision_gender_way_J =  profit_with_on_farm_job_10  - profit_Default,
                
                #way11
                NPV_profit_with_on_farm_job_11 =  NPV_profit_with_on_farm_job_11, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_on_farm_job_11 = NPV_decision_profit_with_on_farm_job_11,
                Cashflow_decision_gender_way_K =  profit_with_on_farm_job_11  - profit_Default,
                
                #way12
                NPV_profit_with_family_money_12 =  NPV_profit_with_family_money_12, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_family_money_12 = NPV_decision_profit_with_family_money_12,
                Cashflow_decision_gender_way_L =  profit_with_family_money_12  - profit_Default,
                
                #way13
                NPV_profit_with_family_money_13 =  NPV_profit_with_family_money_13, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_family_money_13 = NPV_decision_profit_with_family_money_13,
                Cashflow_decision_gender_way_M =  profit_with_family_money_13  - profit_Default,
                
                #way14
                NPV_profit_with_family_money_14 =  NPV_profit_with_family_money_14, 
                NPV_no_branch = NPV_no_branch,
                NPV_decision_profit_with_family_money_14 = NPV_decision_profit_with_family_money_14,
                Cashflow_decision_gender_way_N =  profit_with_family_money_14  - profit_Default

    )) 
    
}
  



mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_profit_with_Own_business_branch_1",
                                             "NPV_profit_with_Own_business_branch_2"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_off_farm_job_4",
                                             "NPV_decision_profit_with_off_farm_job_5",
                                             "NPV_decision_profit_with_off_farm_job_6",
                                             "NPV_decision_profit_with_off_farm_job_7",
                                             "NPV_decision_profit_with_on_farm_job_8",
                                             "NPV_decision_profit_with_on_farm_job_9",
                                             "NPV_decision_profit_with_on_farm_job_10",
                                             "NPV_decision_profit_with_on_farm_job_11"
                                             ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Own_business_branch_1",
                                             "NPV_decision_profit_with_Own_business_branch_2",
                                             "NPV_decision_profit_with_Own_business_branch_3"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_family_money_12",
                                             "NPV_decision_profit_with_family_money_13",
                                             "NPV_decision_profit_with_family_money_14"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


################################################################################
### Our Model does not run yet, therefore we haven't cleaned up here

#Plot Net Present Value (NPV) distributions

#We can use the plot_distributions() function to produce 
#one of the several plotting options for distribution outputs.
#This shows us an overlay of the full results of the 
#Monte Carlo model of the decision options, 
#i.e. the expected NPV if we choose to do the
#intervention Interv_NPV or not do the intervention NO_Interv_NPV.

#Here we show the results of a Monte Carlo simulation 
#(200 model runs) for 
#estimating the comparative profits with and without hail nets.

#Here we show the results of a Monte Carlo simulation (200 model runs) for
#estimating the comparative profits with and without hail nets.
#plot_distributions(mcSimulation_object = mcSimulation_results, #without _wayx , this code wil not work.
#                   vars = c("NPV_no_branch", "NPV_branch"),
#                   method = 'smooth_simple_overlay', 
#                   base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Own_business_branch_1",
                                             "NPV_decision_profit_with_Own_business_branch_2",
                                             "NPV_decision_profit_with_Own_business_branch_3"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_off_farm_job_4",
                                             "NPV_decision_profit_with_off_farm_job_5",
                                             "NPV_decision_profit_with_off_farm_job_6",
                                             "NPV_decision_profit_with_off_farm_job_7"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_on_farm_job_8",
                                             "NPV_decision_profit_with_on_farm_job_9",
                                             "NPV_decision_profit_with_on_farm_job_10",
                                             "NPV_decision_profit_with_on_farm_job_11"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_family_money_12",
                                             "NPV_decision_profit_with_family_money_13",
                                             "NPV_decision_profit_with_family_money_14"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

 
# plot_distributions(mcSimulation_object = mcSimulation_results_way10, #without _wayx , this code wil not work.
#                    vars = c("NPV_no_branch", "NPV_branch"),
#                    method = 'smooth_simple_overlay', 
#                    base_size = 7,
#                    theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#D9D9D9", size = 1),
#       panel.grid = element_line(colour = "#D9D9D9"),
#       plot.title = element_text(vjust = 2, hjust = 0.5),
#       axis.title.y = element_text(vjust = 2),
#       axis.title.x = element_text(vjust = -1),
#       axis.text.x = element_text(size = 10),
#       strip.text = element_text(size = 12)))
#   
# 
# 
# plot_distributions(mcSimulation_object = mcSimulation_results_way10, #without _wayx , this code wil not work.
#                    vars = c("NPV_no_branch", "NPV_branch"),
#                    method = 'smooth_simple_overlay', 
#                    base_size = 15)
#       
# plot_distributions(mcSimulation_object = mcSimulation_results_way2, #without _wayx , this code wil not work.
#                    vars = c("NPV_no_branch", "NPV_branch"),
#                    method = 'smooth_simple_overlay', 
#                    base_size = 15)
# 
# plot_distributions(mcSimulation_object = mcSimulation_results_way6, #without _wayx , this code wil not work.
#                    vars = c("NPV_no_branch", "NPV_branch"),
#                    method = 'smooth_simple_overlay', 
#                    base_size = 15)


# boxplots

#We can use the same function to show the distributions of the
#‘do’ Interv_NPV and ‘do not do’ NO_Interv_NPV decision scenarios
#as boxplots. This can be useful when comparing multiple outputs
#by illustrating the spread of the data resulting from the 
#decision model. Boxplots show the median (central line), 
#the 25th and 75th percentiles (sides of boxes) and any outliers 
#(light circles outside of boxes).

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, #without _wayx , this code wil not work.
                                    vars = c("NPV_no_branch",
                                             "NPV_profit_with_Own_business_branch_1"),
                                    method = 'boxplot')

# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way6, #without _wayx , this code wil not work.
#                                     vars = c("NPV_no_branch",
#                                              "NPV_branch"),
#                                     method = 'boxplot')
# 
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way2, #without _wayx , this code wil not work.
#                                     vars = c("NPV_no_branch",
#                                              "NPV_branch"),
#                                     method = 'boxplot')
# 
# 
# 
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way10, #without _wayx , this code wil not work.
#                                     vars = c("NPV_no_branch",
#                                              "NPV_branch"),
#                                     method = 'boxplot')
#distribution

#We can use the same function for the value of the decision 
#(difference in NPV between do and do not do). 
#This can be quite helpful for us since it shows us the outcome 
#distribution of the decision itself.
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, #without _wayx , this code wil not work.
                                    vars = "NPV_profit_with_Own_business_branch_1",
                                    method = 'boxplot_density')



##histogram


#ggplot(stacked_test,aes(x=values))+ 
#  geom_histogram(data=subset(stacked_test,ind =='practice'),
#                 aes(fill = ind), alpha = 0.5, bins = 150) + 
#  geom_histogram(data=subset(stacked_test,ind == 'practice.2'),
#                 aes(fill = ind), alpha = 0.5, bins = 150) +
#  geom_histogram(data=subset(stacked_test,ind == 'practice.3'),
#                 aes(fill = ind), alpha = 0.5, bins = 150) 

####Cashflow analysis####

#Here we plot the distribution of annual cashflow over the 
#entire simulated period for the intervention (n_years). 
#For this we use the plot_cashflow() function which uses the 
#specified cashflow outputs from the mcSimulation() function 
#(in our case Cashflow_decision_do) to show cashflow over time.


#Fehler: cashflow_var_name contains numbers. Consider renaming your cashflow variables in the model function!

CashflowA <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_A" ) #without the correct variable name this code will not work.

CashflowB <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_B" ) 

CashflowC <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_C" ) 

CashflowD <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_D" ) 

CashflowE <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_E" )

CashflowF <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_F" )

CashflowG <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_G" )

CashflowH <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_H" )

CashflowI <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_I" )

CashflowJ <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_J" )

CashflowK <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_K" )

CashflowL <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_L" )

CashflowM <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_M" )

CashflowN <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_N" )
CashflowA
CashflowB
CashflowC
CashflowD
CashflowE
CashflowF
CashflowG
CashflowH
CashflowI
CashflowJ
CashflowK
CashflowL
CashflowM
####Projection to Latent Structures (PLS) analysis####

#We apply a post-hoc analysis to the mcSimulation() outputs

#with plsr.mcSimulation() to determine the Variable 
#Importance in the Projection (VIP) score and coefficients of 
#a Projection to Latent Structures (PLS) regression model. 
#This function uses the outputs of the mcSimulation() selecting
#all the input variables from the decision analysis function 
#in the parameter object and then runs a PLS regression with an 
#outcome variable defined in the parameter resultName. 
#We use the code names(mcSimulation_results$y)[3] to select the
#outcome variable NPV_decision_do, which is the third element of
#the list y in our mcSimulation_results outputs
#(this must be a character element).


pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)





#We run the plot_pls() on the results from plsr.mcSimulation() with a number of standard settings.
#The length of the bars is equal to VIP with a vertical line at ‘1’
#on the x-axis indicating a standard cut-off for VIP used for variable selection. 
#The overall plot only shows those variables with a VIP > 0.8, which is the 
#common threshold for variable selection. The colors of the bars represent the 
#positive or negative coefficient of the given input variable with the output variable.
#Here we import the input table again to replace the labels
# for the variables on the y-axis. The input table can 
#include a label and variable column. The standard 
#(from the variable column) are usually computer readable and 
#not very nice for a plot. The plot_pls() function uses the text
#in the label column as replacement for the default text in the 
#variable column.
plot_pls(pls_result, threshold = 0) # I corrected it alread several times, but the input table here needs the name of our input table in our environment.
# input_table = input_table_gender, 
#showing strange results

# We calculate Value of Information (VoI) analysis with the Expected Value of Perfect Information (EVPI). 
#As we learned in Lecture 8 on forecasts, EVPI measures 
# the expected opportunity loss that is incurred when the decision-maker 
# does not have perfect information about a particular variable. 
# EVPI is determined by examining the influence of that variable on the output value of a decision model.
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_Own_business_branch_1")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_Own_business_branch_1")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_Own_business_branch_2")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_Own_business_branch_2")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_Own_business_branch_3")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_Own_business_branch_3")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_4 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_4")


mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_5 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_5")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_6 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_6")


mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_7 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_7")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_8 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_8")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_9 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_9")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_10 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_10")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_11 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_11")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_with_family_money_12 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_with_family_money_12")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_with_family_money_13 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_with_family_money_13")

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_with_family_money_14 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_with_family_money_14")


## in the compound figute, we are forced to use the wrong input table as an input, therefore we get bad results for some plots.
compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = input_table_gender, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_profit_with_Own_business_branch_1", 
                cashflow_var_name = "Cashflow_decision_gender", 
                base_size = 7)


#way 2
# plot_cashflow(mcSimulation_object = mcSimulation_results_way2, cashflow_var_name = "Cashflow_decision_gender")
# pls_result <- plsr.mcSimulation(object = mcSimulation_results_way2,
#                                 resultName = names(mcSimulation_results_way2$y)[3], ncomp = 1)
# plot_pls(pls_result, threshold = 0, input_table = input_table_gender)
# mcSimulation_table <- data.frame(mcSimulation_results_way2$x, mcSimulation_results_way2$y[1:3])
# evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
# plot_evpi(evpi, decision_vars = "NPV_decision")
# compound_figure(mcSimulation_object = mcSimulation_results_way2, 
#                 input_table = input_table_gender, plsrResults = pls_result, 
#                 EVPIresults = evpi, decision_var_name = "NPV_decision", 
#                 cashflow_var_name = "Cashflow_decision_gender", 
#                 base_size = 7)


