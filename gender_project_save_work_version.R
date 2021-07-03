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

Way <- 1


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
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Own_business_branch  - profit_Default))
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
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Job_away_of_farm  - profit_Default))
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
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Job_away_of_farm  - profit_Default))
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
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Family_money  - profit_Default))
  
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
              NPV_decision = NPV_decision,
              Cashflow_decision_gender =  profit_with_On_Farm  - profit_Default))
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

## Cashflow: Here we plot the distribution of annual cashflow 
# over the entire simulated period for the intervention (n_years). 
# For this we use the plot_cashflow() function 
# which uses the specified cashflow outputs from the mcSimulation() function to show cashflow over time.
# In the example, the cashflow_var_name is "Cashflow_decision_do".
# In our case, it is not implemented yet in the return function. 
# First, I implemented it in line 148, 170, 192, 214 and 237 in the return function 
# (Cashflow_decision_gender = profit_with_Own_business_branch - profit_Default), 
# Unfortunately I think the values show, that we actually don't really have distinguished time horizons
# between pension payment and retirement. 
# I suggest rearrange our time structure to years - 
# now its is a mix of monthly and anual values. 
# renaming all the n = 40 / n=17 to n_month, which should be specified in the beginning of our script.


plot_cashflow(mcSimulation_object = mcSimulation_results_way1, cashflow_var_name = "Cashflow_decision_gender")



## Projection to Latent Structures (PLS) analysis
# We apply a post-hoc analysis to the mcSimulation() outputs with plsr.mcSimulation().
# The idea is to determine the Variable Importance in the Projection (VIP) score 
# and coefficients of a Projection to Latent Structures (PLS) regression model. 
# This function uses the outputs of the mcSimulation(). 
# It selects all the input variables from the decision analysis function 
# in the parameter object and then runs a PLS regression 
# with an outcome variable defined in the parameter resultName. 
# In the example they use the code names(mcSimulation_results$y)[3] 
# to select the outcome variable NPV_decision_do, 
# which is the third element of the list y in our mcSimulation_results outputs 
# I our case, selecting the third element of the list y in our mcSimulation_results outputs 
# as well. it is called NPV decision.

pls_result <- plsr.mcSimulation(object = mcSimulation_results_way1,
                                resultName = names(mcSimulation_results_way1$y)[3], ncomp = 1)


## Plot PLS: 
# We run the plot_pls() on the results from plsr.mcSimulation() 
# with a number of standard settings. 
# The plot_pls() function uses the text in the label column 
# as replacement for the default text in the variable column.
# This will cause problems, because the label column in our input_table_gender is 
# not helpful for labeling a plot. (it says something like financing, percent and options)
# Potential TO Do: In our input_table_gender, we need to adjust the column label, 
# and fill it analogue to the example input table with appropriate lables for plotting. 
# solution: we can leave out the input_table = input_table_gender, 
# becasue out labels column messes up. 

plot_pls(pls_result, threshold = 0, input_table = input_table_gender)

# Rauslassen


# We calculate Value of Information (VoI) analysis with the Expected Value of Perfect Information (EVPI). 
#As we learned in Lecture 8 on forecasts, EVPI measures 
# the expected opportunity loss that is incurred when the decision-maker 
# does not have perfect information about a particular variable. 
# EVPI is determined by examining the influence of that variable on the output value of a decision model.
mcSimulation_table <- data.frame(mcSimulation_results_way1$x, mcSimulation_results_way1$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
plot_evpi(evpi, decision_vars = "NPV_decision")


## in the compound figute, we are forced to use the wrong input table as an input, therefore we get bad results for some plots.
compound_figure(mcSimulation_object = mcSimulation_results_way1, 
                input_table = input_table_gender, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision", 
                cashflow_var_name = "Cashflow_decision_gender", 
                base_size = 7)

#way 2
plot_cashflow(mcSimulation_object = mcSimulation_results_way2, cashflow_var_name = "Cashflow_decision_gender")
pls_result <- plsr.mcSimulation(object = mcSimulation_results_way2,
                                resultName = names(mcSimulation_results_way2$y)[3], ncomp = 1)
plot_pls(pls_result, threshold = 0, input_table = input_table_gender)
mcSimulation_table <- data.frame(mcSimulation_results_way2$x, mcSimulation_results_way2$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
plot_evpi(evpi, decision_vars = "NPV_decision")
compound_figure(mcSimulation_object = mcSimulation_results_way2, 
                input_table = input_table_gender, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision", 
                cashflow_var_name = "Cashflow_decision_gender", 
                base_size = 7)

#way 3
plot_cashflow(mcSimulation_object = mcSimulation_results_way3, cashflow_var_name = "Cashflow_decision_gender")
pls_result <- plsr.mcSimulation(object = mcSimulation_results_way3,
                                resultName = names(mcSimulation_results_way3$y)[3], ncomp = 1)
plot_pls(pls_result, threshold = 0, input_table = input_table_gender)
mcSimulation_table <- data.frame(mcSimulation_results_way3$x, mcSimulation_results_way3$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
plot_evpi(evpi, decision_vars = "NPV_decision")
compound_figure(mcSimulation_object = mcSimulation_results_way3, 
                input_table = input_table_gender, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision", 
                cashflow_var_name = "Cashflow_decision_gender", 
                base_size = 7)

