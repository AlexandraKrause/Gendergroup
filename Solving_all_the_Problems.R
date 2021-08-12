library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)

####TO DO List ####


####Get data####

input_table_gender <-read.csv2("./input_table_gender_final_trial_years_woRisk_Alina_test.csv", dec = ",")

input_table_gender <- input_table_gender %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))


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


  
  # Default option: Our decision maker is a farm wife and does nothing special. 
  # Apart from the mandatory agricultural insurance, she invests nothing in her retirement.
  # The agricultural Insurance is not payed by her, but from her family.
  # Default always same for all 14 options; We use it to compare all other options with
  
  PartA <- Agri_insurance + Agri_insurance_inv # this is payed by the husband /in-laws
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
  
  # Way 8: She gets an official working contract and gets officially paid on farm by her husband.  
  # Here, she stops to be part of the agricultural insurance. 
  # Instead, she is part of the mandatory state insurance.
  # She invests no additional money in her pension.
  
  
  profit_with_on_farm_job_8 <- (State_insurance_on_farm - State_insurance_inv_on_farm)
  
  
  NPV_profit_with_on_farm_job_8 <- discount(profit_with_on_farm_job_8,
                                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_on_farm_job_8 <- NPV_profit_with_on_farm_job_8 - NPV_no_branch
  
  
  # Way 9: She gets an official working contract and gets officially paid on farm by her husband.  
  # Here, she stops to be part of the agricultural insurance. 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in private insurance. 
  
  PartA <- Private_insurance_on_farm + State_insurance_on_farm 
  PartB <- State_insurance_inv_on_farm + Private_insurance_inv_on_farm
  
  profit_with_on_farm_job_9 <- (PartA - PartB)
  
  
  NPV_profit_with_on_farm_job_9 <- discount(profit_with_on_farm_job_9,
                                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_on_farm_job_9 <- NPV_profit_with_on_farm_job_9 - NPV_no_branch
  
  
  # Way 10: She gets an official working contract and gets officially paid on farm by her husband.  
  # Here, she stops to be part of the agricultural insurance, 
  # Instead, she is part of the mandatory state insurance.
  # She invests a part of her income in ETF. 
  
  PartA <- ETF_on_farm + State_insurance_on_farm 
  PartB <- State_insurance_inv_on_farm + ETF_inv_on_farm
  
  profit_with_on_farm_job_10 <- (PartA - PartB)
  
  
  NPV_profit_with_on_farm_job_10 <- discount(profit_with_on_farm_job_10,
                                             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_decision_profit_with_on_farm_job_10 <- NPV_profit_with_on_farm_job_10 - NPV_no_branch
  
  
  # Way 11: She gets an official working contract and gets officially paid on farm by her husband.  
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
  
  NPV_decision_profit_with_family_money_13 <- NPV_profit_with_family_money_13 - NPV_no_branch
  
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
  numberOfModelRuns = 100,
  functionSyntax = "plainNames"
)

####Plot distributions####

#Plot Net Present Value (NPV) distributions
#We can use the plot_distributions() function to produce 
#one of the several plotting options for distribution outputs.
#There we show an overlay of the full results of the 
#Monte Carlo simulation (200 model runs) of the decision options, 
#i.e. the expected NPV if we choose to do the
#intervention. 

#Plot own branch 
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Own_business_branch_1",
                                             "NPV_decision_profit_with_Own_business_branch_2",
                                             "NPV_decision_profit_with_Own_business_branch_3"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
#Plot off farm Job
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_off_farm_job_4",
                                             "NPV_decision_profit_with_off_farm_job_5",
                                             "NPV_decision_profit_with_off_farm_job_6",
                                             "NPV_decision_profit_with_off_farm_job_7"
                                            
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

#Plot on farm job
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_on_farm_job_8",
                                             "NPV_decision_profit_with_on_farm_job_9",
                                             "NPV_decision_profit_with_on_farm_job_10",
                                             "NPV_decision_profit_with_on_farm_job_11"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


#Plot Family Money
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_family_money_12",
                                             "NPV_decision_profit_with_family_money_13",
                                             "NPV_decision_profit_with_family_money_14"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


####Boxplots####
# We can use the same function to show the distributions of the
# decisions as boxplots. Boxplots show the median (central line), 
# the 25th and 75th percentiles (sides of boxes) and any outliers 
# (light circles outside of boxes).
#'boxplot' own branch 
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Own_business_branch_1",
                                             "NPV_decision_profit_with_Own_business_branch_2",
                                             "NPV_decision_profit_with_Own_business_branch_3"
                                    ),
                                    method = 'boxplot', 
                                    base_size = 7)
#'boxplot' off farm Job
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_off_farm_job_4",
                                             "NPV_decision_profit_with_off_farm_job_5",
                                             "NPV_decision_profit_with_off_farm_job_6",
                                             "NPV_decision_profit_with_off_farm_job_7"
                                             
                                    ),
                                    method = 'boxplot', 
                                    base_size = 7)

#'boxplot' on farm job
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_on_farm_job_8",
                                             "NPV_decision_profit_with_on_farm_job_9",
                                             "NPV_decision_profit_with_on_farm_job_10",
                                             "NPV_decision_profit_with_on_farm_job_11"
                                    ),
                                    method = 'boxplot', 
                                    base_size = 7)


#'boxplot' Family Money
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_family_money_12",
                                             "NPV_decision_profit_with_family_money_13",
                                             "NPV_decision_profit_with_family_money_14"
                                    ),
                                    method = 'boxplot', 
                                    base_size = 7)
####Cashflow analysis####

#Here we plot the distribution of annual cashflow over the 
#entire simulated period for the intervention (n_years). 
#For this we use the plot_cashflow() function which uses the 
#specified cashflow outputs from the mcSimulation() function 
#(in our case Cashflow_decision_do) to show cashflow over time.




# CashflowA <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_A" ) #without the correct variable name this code will not work.
# 
# CashflowB <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_B" ) 
# 
# CashflowC <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_C" ) 
# 
# CashflowD <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_D" ) 
# 
# CashflowE <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_E" )
# 
# CashflowF <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_F" )
# 
# CashflowG <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_G" )
# 
# CashflowH <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_H" )
# 
# CashflowI <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_I" )
# 
# CashflowJ <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_J" )
# 
# CashflowK <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_K" )
# 
# CashflowL <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_L" )
# 
# CashflowM <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_M" )
# 
# CashflowN <- plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_gender_way_N" )

CashflowA <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_A",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowB <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_B",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowC <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_C",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowD <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_D",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowE <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_E",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowF <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_F",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowG <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_G",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowH <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_H",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowI <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_I",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowJ <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_J",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowK <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_K",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowL <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_L",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")
CashflowM <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_M",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")

<<<<<<< HEAD
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
=======
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

>>>>>>> 3b85d5a0ea9ee4f255ce9f7e2d385f207f0b6086

####PLS####

#We apply a post-hoc analysis to the mcSimulation() outputs
#with plsr.mcSimulation() to determine the Variable 
#Importance in the Projection (VIP) score and coefficients of 
#a Projection to Latent Structures (PLS) regression model. 
#This function uses the outputs of the mcSimulation() selecting
#all the input variables from the decision analysis function 
#in the parameter object and then runs a PLS regression with an 
#outcome variable defined in the parameter resultName. 
#We use the code names(mcSimulation_results$y)[n] to select the
#correct results for our 14 ways. 
# Here we provide also a legend of the objects 
# in  mcSimulation_results$y

names(mcSimulation_results$x)
names(mcSimulation_results$y)


#	3	 "NPV_decision_profit_with_Own_business_branch_1"
#	62	"NPV_decision_profit_with_Own_business_branch_2"
#	121	"NPV_decision_profit_with_Own_business_branch_3"
#	180	"NPV_decision_profit_with_off_farm_job_4"
#	239	"NPV_decision_profit_with_off_farm_job_5"
#	298	"NPV_decision_profit_with_off_farm_job_6"
#	357	"NPV_decision_profit_with_off_farm_job_7"
#	416	"NPV_decision_profit_with_on_farm_job_8"
#	475	"NPV_decision_profit_with_on_farm_job_9"
#	534	 "NPV_decision_profit_with_on_farm_job_10"
#	593	"NPV_decision_profit_with_on_farm_job_11"
#	652	"NPV_decision_profit_with_family_money_12"
#	711	"NPV_decision_profit_with_family_money_13"
#	770	"NPV_decision_profit_with_family_money_14"

#	Pls of	 "NPV_decision_profit_with_Own_business_branch_1"
pls_result_1 <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_Own_business_branch_2"
pls_result_2 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[62], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_Own_business_branch_3"
pls_result_3 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[121], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_4"
pls_result_4 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[180], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_5"
pls_result_5 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[239], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_6"
pls_result_6 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[298], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_7"
pls_result_7 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[357], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_on_farm_job_8"
pls_result_8 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[416], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_on_farm_job_9"
pls_result_9 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[475], ncomp = 1)

#	Pls of	 "NPV_decision_profit_with_on_farm_job_10"
pls_result_10 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[534], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_on_farm_job_11"
pls_result_11 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[539], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_family_money_12"
pls_result_12 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[652], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_family_money_13"
pls_result_13 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[711], ncomp = 1)

#	Pls of	"NPV_decision_profit_with_family_money_14"
pls_result_14 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[770], ncomp = 1)


#We run the plot_pls() on the results from plsr.mcSimulation() 
#The colors of the bars represent the positive or negative coefficient 
#of the given input variable with the output variable.

plot_pls(pls_result_1, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_2, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_3, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_4, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_5, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_6, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_7, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_8, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_9, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_10, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_11, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_12, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_13, threshold = 0.8, input_table = input_table_gender)
plot_pls(pls_result_14, threshold = 0.8, input_table = input_table_gender)


####EVPI####
#Note From Alina: I do not understand how to make this evpi run

# We calculate Value of Information (VoI) analysis 
# with the Expected Value of Perfect Information (EVPI). 
# As we learned in Lecture 8 on forecasts, EVPI measures 
# the expected opportunity loss that is incurred when the decision-maker 
# does not have perfect information about a particular variable. 
# EVPI is determined by examining the influence of that variable on the output value of a decision model.
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_Own_business_branch_1")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_Own_business_branch_1")

names(mcSimulation_results$y[1:3])
colnames(mcSimulation_results$y)[238]

colnames(mcSimulation_results$y[1:3])



mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,61,62)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_Own_business_branch_2")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_Own_business_branch_2")



mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,120,121)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_Own_business_branch_3")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_Own_business_branch_3")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,179,180)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_4 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_4")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,238,239)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_5 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_5")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,297,298)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_6 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_6")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,356,357)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_off_farm_job_7 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_7")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,415,416)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_8 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_8")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,474,475)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_9 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_9")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,533,534)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_10 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_10")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,538,539)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_on_farm_job_11 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_11")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,651,652)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_with_family_money_12 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_with_family_money_12")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,710,711)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_with_family_money_13 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_with_family_money_13")

#does not work
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[c(1,769,770)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_profit_with_with_family_money_14 ")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_with_family_money_14")


## Note from Alina: I suggest we dont use the coumpound figure. 
## in the compound figute, we are forced to use the wrong input table as an input, therefore we get bad results for some plots.
# compound_figure(mcSimulation_object = mcSimulation_results, 
#                 input_table = input_table_gender, plsrResults = pls_result_1, 
#                 EVPIresults = evpi, decision_var_name = "NPV_profit_with_Own_business_branch_1", 
#                 cashflow_var_name = "Cashflow_decision_gender", 
#                 base_size = 7)


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


