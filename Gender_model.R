library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)

####TO DO List ####
#devtools::install_github("eikeluedeling/decisionSupport", 
#ref = "fix_plot_evpi")
#install.packages("Rtools 4.0")
#library("Rtools 4.0")

####Get data####

input_table_gender <-read.csv2("./input_table_gender_final_trial_years_woRisk_Alina.csv", dec = ",")

input_table_gender <- input_table_gender %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))

#inputestimates
#Reminder about the make_variables function
# make_variables <- function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                               as.numeric(x[1,i]),envir=.GlobalEnv)
# }#Then call:
#   make_variables(as.estimate(input_table_gender))


####Decision function####

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
  Private_insurance_off_farm <- c(rep (0,working_years), 
                                  vv(var_mean =Private_insurance_off_farm, 
                                                            var_CV = var_slight, 
                                                            n = pension_years))
  
  Private_insurance_on_farm <- c(rep (0,working_years), 
                                 vv(var_mean =Private_insurance_on_farm, 
                                                           var_CV = var_slight, 
                                                           n = pension_years))
  
  Private_insurance_own_branch <- c(rep (0,working_years),
                                    vv(var_mean =Private_insurance_own_branch, 
                                                            var_CV = var_slight, 
                                                             n = pension_years))
  
  Private_insurance_family_money <- c(rep (0,working_years),
                                    vv(var_mean =Private_insurance_family_money, 
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
  
  State_insurance_off_farm <- c(rep (0,working_years), 
                                vv(var_mean =State_insurance_off_farm, 
                                                          var_CV = var_slight, 
                                                          n = pension_years))
  
  State_insurance_on_farm <- c(rep (0,working_years),
                               vv(var_mean =State_insurance_on_farm, 
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


  
  # Default option: Our decision maker is a farm wife and does nothing special. 
  # Apart from the mandatory agricultural insurance, she invests nothing 
  #in her retirement.
  # The agricultural Insurance is not payed by her, but from her family.
  # Default always same for all 14 options; We use it to compare all other
  # options with
  
  PartA <- Agri_insurance + Agri_insurance_inv
  # this is payed by the husband /in-laws
  PartB <- Agri_insurance_inv
  profit_Default <- (PartA - PartB)
  
  NPV_no_branch <- discount(profit_Default,
                            discount_rate = discount_rate, calculate_NPV = TRUE) 
  
  
  # Branch 1 = Default vs. Own branch (way 1, 2, 3)
  
  # Way 1: She sets up her own business branch. 
  # Here, she continues to be part of the agricultural insurance, 
  # but also invests about 10 % of her income in private insurance. 
  
  
  
  PartA <- Private_insurance_own_branch + Agri_insurance
  PartB <- Agri_insurance_inv + Private_insurance_inv_own_branch
  profit_with_Own_business_branch_1 <- (PartA - PartB)
  
  NPV_profit_with_Own_business_branch_1 <- discount(profit_with_Own_business_branch_1,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_Own_business_branch_1 <- NPV_profit_with_Own_business_branch_1 - NPV_no_branch
  
  
  
  
  # Way 2: She sets up her own business branch. 
  # Here, she continues to be part of the agricultural insurance, 
  # but also invests about 10 % of her income in ETF. 
  
  PartA <- ETF_own_branch + Agri_insurance
  PartB <- Agri_insurance_inv + ETF_inv_own_branch
  
  profit_with_Own_business_branch_2 <- (PartA - PartB)
  
  
  NPV_profit_with_Own_business_branch_2 <- discount(profit_with_Own_business_branch_2,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_Own_business_branch_2 <- NPV_profit_with_Own_business_branch_2 - NPV_no_branch
  
  # Way 3: She sets up her own business branch. 
  # Here, she continues to be part of the agricultural insurance, 
  # but also invests about 10 % of her income in a Investment-Mix.
  
  
  PartA <- Mix_own_branch + Agri_insurance 
  PartB <- Agri_insurance_inv + Mix_inv_own_branch
  
  
  profit_with_Own_business_branch_3 <- (PartA - PartB)
  
  
  NPV_profit_with_Own_business_branch_3 <- discount(profit_with_Own_business_branch_3,
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_Own_business_branch_3 <- NPV_profit_with_Own_business_branch_3 - NPV_no_branch
  
  
  
  # Branch 2 = Default vs. Off-Farm Job (Way 4, 5, 6, 7)
  
  # Way 4: She gets an off-farm job. 
  # Here, she stops to be part of the agricultural insurance, 
  # Instead, she is part of the mandatory state insurance.
  # She invests no income in pension. 
  
  
  
  profit_with_off_farm_job_4 <- (State_insurance_off_farm - State_insurance_inv_off_farm)
  
  
  NPV_profit_with_off_farm_job_4 <- discount(profit_with_off_farm_job_4,
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_off_farm_job_4 <- NPV_profit_with_off_farm_job_4 - NPV_no_branch
  
  # Way 5: She gets an off-farm job. 
  # Here, she stops to be part of the agricultural insurance, 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in private insurance. 
  
  
  PartA <- Private_insurance_off_farm + State_insurance_off_farm 
  PartB <- Private_insurance_inv_off_farm + State_insurance_inv_off_farm 
  
  profit_with_off_farm_job_5 <- (PartA - PartB)
  
  
  NPV_profit_with_off_farm_job_5 <- discount(profit_with_off_farm_job_5,
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_off_farm_job_5 <- NPV_profit_with_off_farm_job_5 - NPV_no_branch
  
  
  # Way 6: She gets an off-farm job. 
  # Here, she stops to be part of the agricultural insurance, 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in ETF. 
  
  PartA <- ETF_off_farm + State_insurance_off_farm 
  PartB <- State_insurance_inv_off_farm + ETF_inv_off_farm
  
  profit_with_off_farm_job_6 <- (PartA - PartB)
  
  
  NPV_profit_with_off_farm_job_6 <- discount(profit_with_off_farm_job_6,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_off_farm_job_6 <- NPV_profit_with_off_farm_job_6 - NPV_no_branch
  
  # Way 7: She gets an off-farm job. 
  # Here, she stops to be part of the agricultural insurance, 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in a mixed investment. 
  
  PartA <- Mix_off_farm + State_insurance_off_farm
  PartB <- Mix_inv_off_farm + State_insurance_inv_off_farm
  
  
  profit_with_off_farm_job_7 <- (PartA - PartB)
  
  
  NPV_profit_with_off_farm_job_7 <- discount(profit_with_off_farm_job_7,
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_off_farm_job_7 <- NPV_profit_with_off_farm_job_7 - NPV_no_branch
  
  
  # Branch 3 = Default vs. On farm job = Payment of wife (Way 8)
  
  # Way 8: She gets an official working contract and gets officially paid
  #on farm by her husband.  
  # Here, she stops to be part of the agricultural insurance. 
  # Instead, she is part of the mandatory state insurance.
  # She invests no additional money in her pension.
  
  
  profit_with_on_farm_job_8 <- (State_insurance_on_farm - State_insurance_inv_on_farm)
  
  
  NPV_profit_with_on_farm_job_8 <- discount(profit_with_on_farm_job_8,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_on_farm_job_8 <- NPV_profit_with_on_farm_job_8 - NPV_no_branch
  
  
  # Way 9: She gets an official working contract and gets officially paid
  #on farm by her husband.  
  # Here, she stops to be part of the agricultural insurance. 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in private insurance. 
  
  PartA <- Private_insurance_on_farm + State_insurance_on_farm 
  PartB <- State_insurance_inv_on_farm + Private_insurance_inv_on_farm
  
  profit_with_on_farm_job_9 <- (PartA - PartB)
  
  
  NPV_profit_with_on_farm_job_9 <- discount(profit_with_on_farm_job_9,
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_on_farm_job_9 <- NPV_profit_with_on_farm_job_9 - NPV_no_branch
  
  
  # Way 10: She gets an official working contract and gets officially paid
  #on farm by her husband.  
  # Here, she stops to be part of the agricultural insurance, 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in ETF. 
  
  PartA <- ETF_on_farm + State_insurance_on_farm 
  PartB <- State_insurance_inv_on_farm + ETF_inv_on_farm
  
  profit_with_on_farm_job_10 <- (PartA - PartB)
  
  
  NPV_profit_with_on_farm_job_10 <- discount(profit_with_on_farm_job_10,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_on_farm_job_10 <- NPV_profit_with_on_farm_job_10 - NPV_no_branch
  
  
  # Way 11: She gets an official working contract and gets officially paid
  #on farm by her husband.  
  # Here, she stops to be part of the agricultural insurance, 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in a mixed investment. 
  
  PartA <- Mix_on_farm + State_insurance_on_farm
  PartB <- State_insurance_inv_on_farm + Mix_inv_on_farm
  
  profit_with_on_farm_job_11 <- (PartA - PartB)
  
  
  NPV_profit_with_on_farm_job_11 <- discount(profit_with_on_farm_job_11,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_on_farm_job_11 <- NPV_profit_with_on_farm_job_11 - NPV_no_branch
  
  
  # Branch 4 = Default vs. Family money (way 12, 13, 14) 
  
  # Way 12: She convinces her husband to invest in her pension.
  # Here, she continues to be part of the agricultural insurance, 
  # She uses the money from her husband to invest in a private insurance. 
  
  PartA <- Private_insurance_family_money + Agri_insurance 
  PartB <- Agri_insurance_inv + Private_insurance_inv_family_money
  
  profit_with_family_money_12 <- (PartA - PartB)
  
  
  NPV_profit_with_family_money_12 <- discount(profit_with_family_money_12,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_family_money_12 <- NPV_profit_with_family_money_12 - NPV_no_branch
  
  # Way 13: She convinces her husband to invest in her pension.
  # Here, she continues to be part of the agricultural insurance, 
  # She uses the money from her husband to invest in ETF. 
  
  PartA <- ETF_family_money + Agri_insurance
  PartB <-  ETF_inv_family_money + Agri_insurance_inv
  
  profit_with_family_money_13 <- (PartA - PartB)
  
  
  NPV_profit_with_family_money_13 <- discount(profit_with_family_money_13,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_family_money_13 <- NPV_profit_with_family_money_13  - NPV_no_branch
  
  # Way 14: She convinces her husband to invest in her pension.
  # Here, she continues to be part of the agricultural insurance, 
  # She uses the money from her husband to invest in a mixed investment. 
  
  PartA <- Mix_family_money + Agri_insurance
  PartB <- Agri_insurance_inv + Mix_inv_family_money
  
  profit_with_family_money_14 <- (PartA - PartB)
  
  
  NPV_profit_with_family_money_14 <- discount(profit_with_family_money_14,
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_family_money_14 <- NPV_profit_with_family_money_14 - NPV_no_branch
  
  
  
  ####Return list####
  
  return(list(NPV_no_branch =  NPV_no_branch,
              
              #way 1
NPV_profit_with_Own_business_branch_1 =  NPV_profit_with_Own_business_branch_1, 
NPV_decision_profit_with_Own_business_branch_1 = NPV_decision_profit_with_Own_business_branch_1,
Cashflow_decision_gender_way_A =  profit_with_Own_business_branch_1,
              
              #way2
NPV_profit_with_Own_business_branch_2 =  NPV_profit_with_Own_business_branch_2, 
NPV_decision_profit_with_Own_business_branch_2 = NPV_decision_profit_with_Own_business_branch_2,
Cashflow_decision_gender_way_B = profit_with_Own_business_branch_2,
              
              #way3
NPV_profit_with_Own_business_branch_3 =  NPV_profit_with_Own_business_branch_3, 
NPV_decision_profit_with_Own_business_branch_3 = NPV_decision_profit_with_Own_business_branch_3,
Cashflow_decision_gender_way_C =  profit_with_Own_business_branch_3 ,
              
              #way4
NPV_profit_with_off_farm_job_4 =  NPV_profit_with_off_farm_job_4, 
NPV_decision_profit_with_off_farm_job_4 = NPV_decision_profit_with_off_farm_job_4,
Cashflow_decision_gender_way_D =  profit_with_off_farm_job_4,
              
              #way5
NPV_profit_with_off_farm_job_5 =  NPV_profit_with_off_farm_job_5, 
NPV_decision_profit_with_off_farm_job_5 = NPV_decision_profit_with_off_farm_job_5,
Cashflow_decision_gender_way_E =  profit_with_off_farm_job_5,
              
              #way6
NPV_profit_with_off_farm_job_6 =  NPV_profit_with_off_farm_job_6, 
NPV_decision_profit_with_off_farm_job_6 = NPV_decision_profit_with_off_farm_job_6,
Cashflow_decision_gender_way_F =  profit_with_off_farm_job_6,
              
              #way7
NPV_profit_with_off_farm_job_7 =  NPV_profit_with_off_farm_job_7, 
NPV_decision_profit_with_off_farm_job_7 = NPV_decision_profit_with_off_farm_job_7,
Cashflow_decision_gender_way_G =  profit_with_off_farm_job_7,
              
              #way8
NPV_profit_with_on_farm_job_8 =  NPV_profit_with_on_farm_job_8, 
NPV_decision_profit_with_on_farm_job_8 = NPV_decision_profit_with_on_farm_job_8,
Cashflow_decision_gender_way_H =  profit_with_on_farm_job_8,
              
              #way9
NPV_profit_with_on_farm_job_9 =  NPV_profit_with_on_farm_job_9, 
NPV_decision_profit_with_on_farm_job_9 = NPV_decision_profit_with_on_farm_job_9,
Cashflow_decision_gender_way_I =  profit_with_on_farm_job_9,
              
              #way10
NPV_profit_with_on_farm_job_10 =  NPV_profit_with_on_farm_job_10, 
NPV_decision_profit_with_on_farm_job_10 = NPV_decision_profit_with_on_farm_job_10,
Cashflow_decision_gender_way_J =  profit_with_on_farm_job_10,
              
              #way11
NPV_profit_with_on_farm_job_11 =  NPV_profit_with_on_farm_job_11, 
NPV_decision_profit_with_on_farm_job_11 = NPV_decision_profit_with_on_farm_job_11,
Cashflow_decision_gender_way_K =  profit_with_on_farm_job_11,
              
              #way12
NPV_profit_with_family_money_12 =  NPV_profit_with_family_money_12, 
NPV_decision_profit_with_family_money_12 = NPV_decision_profit_with_family_money_12,
Cashflow_decision_gender_way_L =  profit_with_family_money_12,
              
              #way13
NPV_profit_with_family_money_13 =  NPV_profit_with_family_money_13, 
NPV_decision_profit_with_family_money_13 = NPV_decision_profit_with_family_money_13,
Cashflow_decision_gender_way_M =  profit_with_family_money_13,
              
              #way14
NPV_profit_with_family_money_14 =  NPV_profit_with_family_money_14, 
NPV_decision_profit_with_family_money_14 = NPV_decision_profit_with_family_money_14,
Cashflow_decision_gender_way_N =  profit_with_family_money_14
              
  )) 
  
}

####Simulation####
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 10e4,
  functionSyntax = "plainNames"
)

