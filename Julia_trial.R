library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)


####first step:get data####

#input_table_gender <-read.csv2("./input_table_gender.csv", dec = ",")
#input_table_gender <-read.csv2("./input_table_gender_4th.csv", dec = ",")
input_table_gender <-read.csv2("./input_table_gender_final_trial.csv", dec = ",")


input_table_gender <- input_table_gender %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))

str(input_table_gender)

####function####


# Way 1 = common and own branch and agri insurance
# Way 2= common and own branch and private insurance 
# Way 3= common and own branch and ETF
# not yet added for each : common and mix

# Way 4 = common and Job away from farm and state insurance 
# Way 5 = common and Job away from farm and private insurance
# Way 6 = common and Job away from farm and ETF


# Way 7 = common and family money and agri insurance
# Way 8= common and family money and private insurance
# Way 9 = common and family money and ETF

# Way 10 = common and on farm job and state insurance
# Way 11 = common and on farm job and private insurance
# Way 12 = common and on farm job and ETF


Way <- 2


decision_function <- function(x, varnames){
  
  
  Agri_insurance <- vv(var_mean = Agri_insurance, 
                       var_CV = var_cv_17, 
                       n =204)
  
  Agri_insurance_inv <- vv(var_mean = Agri_insurance_inv, 
                           var_CV = var_cv_40, 
                           n = 480)
  
  
  State_insurance <- vv(var_mean = State_insurance, 
                        var_CV = var_cv_17, 
                        n = 204)
  
  State_insurance_inv <- vv(var_mean = State_insurance_inv, 
                            var_CV = var_cv_40, 
                            n = 480)
  
  ETF <- vv(var_mean = ETF, 
            var_CV = var_cv_17, 
            n = 204)
  
  ETF_inv <- vv(var_mean = ETF_inv, 
                var_CV = var_cv_40, 
                n = 480)
  
  Mix <- vv(var_mean = Mix, 
            var_CV = var_cv_17, 
            n = 204)
  
  Mix_inv <- vv(var_mean = Mix_inv, 
                var_CV = var_cv_40, 
                n = 480)
  
  
  Costs_for_child_care <- vv(var_mean = Costs_for_child_care, 
                             var_CV = var_cv_6, 
                             n = 72)
  
  Costs_for_elderly_care <- vv(var_mean = Costs_for_elderly_care, 
                               var_CV = var_cv_10, 
                               n = 120)
  
  # not needed I think
  
  #Family_money <- vv(var_mean = Family_money, 
  # var_CV = var_cv_40, 
  #  n = 480)
  
  # Off_farm_job<- vv(var_mean = Off_Farm_job, 
  # var_CV = var_cv_40, 
  # n = 480)
  
  #Own_branch <- vv(var_mean = Own_branch, 
  # var_CV = var_cv_40, 
  #  n = 480)
  
  # Off_farm_job <- vv(var_mean = Off_Farm_job, 
  # var_CV = var_cv_40, 
  # n = 480)
  
}

# investment costs  ####

pension_investment_costs <- (Costs_for_childcare+ Costs_for_elderly_care)


#### calculate ex-ante risks ####
Husband_risk <-
  chance_event(Husband_risk, 1, 0, n = 1)

Divorce_risk <-
  chance_event(Divorce_risk, 1, 0, n = 1)

Man_Death_risk <-
  chance_event(Man_Death_risk, 1, 0, n = 1)

Bancruptcy_risk <-
  chance_event(Man_Death_risk, 1, 0, n = 1)

Child_Elderly_risk_obstacle <-
  chance_event(Man_Death_risk, 1, 0, n = 1)


# Way 1- 3 = common and own branch

#man death auch risk?

if(Way == 1){
  
  pension_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_own_business_branch_agri_ins <- ((Agri_insurance - Agri_insurance_inv)- pension_investment_costs) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
  
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_own_business_branch_agri_ins,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}

if(Way == 2){
  pension_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_own_business_branch_private_ins <- ((Private_insurance_own_branch - Private_insurance_inv_own_branch) - pension_investment_costs) * (1- Husband_risk * Bancruptcy_risk * Divorce_risk)
  
  NPV_no_branch <- discount(pension_default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_own_business_branch_private_ins,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}

if(Way == 3){
  pension_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_own_business_branch__ETF <- ((ETF_own_branch - ETF_insurance_inv_own_branch) - pension_investment_costs) * (1- Husband_risk * Bancruptcy_risk * Divorce_risk)
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_own_business_branch__ETF,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}

# way 4- 6 common and off farm job 

if(Way == 4){
  
  pension_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_job_away_from_farm_state_ins <- ((State_insurance_off_farm - State_insurance_inv_off_farm) - pension_investment_costs) * (1- Husband_risk)
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_job_away_from_farm_state_ins,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}

if(Way == 5){
  
  pension_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_job_away_from_farm_private_ins <- ((Private_insurance_off_farm - Private_insurance_inv_off_farm) - pension_investment_costs) * (1- Husband_risk)
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_job_away_from_farm_private_ins,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}

if(Way == 6){
  
  pension_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_job_away_from_farm_ETF <- ((ETF_off_farm - ETF_inv_off_farm)- pension_investment_costs) * (1- Husband_risk)
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_job_away_from_farm_ETF,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}


# Way 7- 9 = common and family money  

if(Way == 7){
  
  pension_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_family_money_agri_insurance <- (Family_money + Agri_insurance - Agri_insurance_inv)  * (1-Bancruptcy_risk * Man_Death_risk * Divorce_risk * Husband_risk)
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_family_money_agri_insurance,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
  
}

if(Way == 8){
  
  pension_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_family_money_private_insurance <- (Private_insurance_family_money - Private_insurance_inv_family_money)  * (1-Bancruptcy_risk * Man_Death_risk * Divorce_risk * Husband_risk)
  
  NPV_no_branch <- discount(profit_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(profit_with_family_money_private_insurance,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
  
}

if(Way == 9){
  
  pension_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_family_money_ETF <- (ETF_family_money - ETF_inv_family_money)  * (1-Bancruptcy_risk * Man_Death_risk * Divorce_risk * Husband_risk)
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_family_money_ETF,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
  
}

# Way 10- 12 = common and on farm job

if(Way == 10){
  
  profit_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  pension_with_on_farm_job_state_insurance <- ((State_insurance_on_farm - State_insurance_inv_on_farm) - pension_investment_costs) * (1- Husband_risk* Divorce_risk * Bancruptcy_risk)
  
  NPV_no_branch <- discount(profit_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(pension_with_on_farm_Job_state_insurance,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}

if(Way == 11){
  
  profit_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  profit_with_on_farm_job_private_insurance <- ((Private_insurance_on_farm - Private_insurance_inv_on_farm) - pension_investment_costs) * (1- Husband_risk* Divorce_risk * Bancruptcy_risk)
  
  NPV_no_branch <- discount(profit_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(profit_with_on_farm_job_private_insurance,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}

if(Way == 12){
  
  profit_Default <- (Agri_insurance - Agri_insurance_inv) * (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  profit_with_on_farm_job_ETF <- ((ETF_on_farm - ETF_inv_on_farm) - pension_investment_costs) * (1- Husband_risk* Divorce_risk * Bancruptcy_risk)
  
  NPV_no_branch <- discount(pension_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(profit_with_on_farm_job_ETF,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
}


mcSimulation_results_way1 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way2 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way3 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way4 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way5 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way6 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way7 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way8 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way9 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way10 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way11 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way12 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way1, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way2, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way3, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way4, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way5, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way6, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way7, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way8, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way9, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way10, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way11, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way12, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
