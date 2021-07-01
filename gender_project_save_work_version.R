library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)

#notes. job on farm no child and elderly care? i mean, money from family for retirement is s.th. else. 

####first step:get data####

#input_table_gender <-read.csv2("./input_table_gender.csv", dec = ",")
#input_table_gender <-read.csv2("./input_table_gender_4th.csv", dec = ",")
input_table_gender <-read.csv2("./input_table_gender_final.csv", dec = ",")


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


# Way 1 = common and own branch
# Way 2 = common and Job away from farm 
# Way 3 = common and Job away from farm + 50 Euro mehr insurance + 20 Euro paying more
# Way 4 = common and family money  
# Way 5 = common and Payment of wife (on farm job)
# Way 6 = common and Own branch + all other thing (ETF,Private insurance and Mix)

Way <- 2


decision_function <- function(x, varnames){
  
  Default_option <- vv(var_mean = Default_option,
                         var_CV = var_cv_40, 
                         n = 40 )

  #Own_business_brach:
  
  Own_branch <- vv(var_mean = Own_branch, 
                            var_CV = var_cv_40, 
                            n = 40)
  
  Off_Farm_job <- vv(var_mean = Off_Farm_job, 
                     var_CV = var_cv_40, 
                     n = 40)
  
  
  Costs_for_child_care <- vv(var_mean = Costs_for_child_care, 
                             var_CV = var_cv_6, 
                             n = 6)
  
  Costs_for_elderly_care <- vv(var_mean = Costs_for_elderly_care, 
                               var_CV = var_cv_10, 
                               n = 10)
  
  State_insurance <- vv(var_mean = State_insurance, 
                        var_CV = var_cv_17, 
                        n = 17)
  
  State_insurance_inv <- vv(var_mean = State_insurance_inv, 
                              var_CV = var_cv_40, 
                              n = 40)
  
  Family_money <- vv(var_mean = Family_money, 
                     var_CV = var_cv_40, 
                     n = 40)
  
  Off_Farm_job<- vv(var_mean = Off_Farm_job, 
                       var_CV = var_cv_40, 
                       n = 40)
  
  
  Agri_insurance <- vv(var_mean = Agri_insurance, 
                       var_CV = var_cv_17, 
                       n = 17)
  
  Agri_insurance_inv <-  vv(var_mean = Agri_insurance_inv, 
                              var_CV = var_cv_40, 
                              n = 40)
  
  
  ETF <- vv(var_mean = ETF, 
            var_CV = var_cv_17, 
            n = 17)
  
  ETF_inv <- vv(var_mean = ETF_inv, 
                  var_CV = var_cv_40, 
                  n = 40)
  
  Mix <- vv(var_mean = Mix, 
            var_CV = var_cv_17, 
            n = 17)
  
  Mix_inv <- vv(var_mean = Mix_inv, 
                  var_CV = var_cv_40, 
                  n = 40)
  
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
  
  
  # Way 1 = common and own branch
  
  #man death auch risk?

  if(Way == 1){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Own_business_branch <- (Own_branch + Agri_insurance - Agri_insurance_inv - ((Costs_for_child_care - Costs_for_elderly_care) * (1-Child_Elderly_risk_obstacle))) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
    
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Own_business_branch,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  # Way 2 = common and Job away from farm 
  #child care risk zusätzlich zu den child care kosten..?

  if(Way == 2){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Job_away_of_farm <- ((Off_Farm_job + State_insurance - State_insurance_inv - ((Costs_for_child_care + Costs_for_elderly_care) * (1- Child_Elderly_risk_obstacle)))* 1- Husband_risk)
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Job_away_of_farm,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  # Way 3 = common and Job away from farm + 50 Euro mehr insurance + 20 Euro paying more

  if(Way == 3){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Job_away_of_farm <- ((Off_Farm_job + (State_insurance + 50) - (State_insurance_inv + 20) - ((Costs_for_child_care + Costs_for_elderly_care) * 1- Child_Elderly_risk_obstacle)) * 1- Husband_risk)
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Job_away_of_farm,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  # Way 4 = common and family money  
  
  if(Way == 4){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Family_money <- (Family_money + Agri_insurance - Agri_insurance_inv)  * (1-Bancruptcy_risk * Man_Death_risk * Divorce_risk * Husband_risk)
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Family_money,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  
}

# Way 5 = common and Payment of wife (on farm job)
  
if(Way == 5){
  
  profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  profit_with_On_Farm_Job <- ((Farm_job_payed + State_insurance - State_insurance_inv) * 1- Husband_risk)
  
  NPV_no_branch <- discount(profit_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(profit_with_On_Farm_Job,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
 }

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
