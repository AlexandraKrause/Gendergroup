library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)



####first step:get data####

#input_table_gender <-read.csv2("./input_table_gender.csv", dec = ",")
input_table_gender <-read.csv2("./input_table_gender_4th.csv", dec = ",")


input_table_gender <- input_table_gender %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))

str(input_table_gender)

n_years <- 40
n_years
########

# #Reminder about the make_variables function
# make_variables <- function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                              as.numeric(x[1,i]),envir=.GlobalEnv)
# }#Then call:
#   make_variables(as.estimate(input_table_gender))

####function####


# Way 1 = common and own branch
# Way 2 = common and Job away from farm 
# Way 3 = common and Job away from farm + 50 Euro mehr insurance + 20 Euro mehr bezahlt
# Way 4 = common and Family money to buy pension options
# Way 5 = common and Payment of Wife for farm work
# Way 6 = common and Own branch + all other thing

#Way 5 and 6 are still missing

Way <- 1
# note from Alina: only way 1 and 2 work, all others dont.

decision_function <- function(x, varnames){
  
  Default_option_2 <- vv(var_mean = Default_option_2,
                         var_CV = var_cv_40,
                         n = 40 )

  Default_option_3 <- vv(var_mean = Default_option_3,
                         var_CV = var_cv_40,
                         n = 40)

  Default_option_3_costs <- vv(var_mean = Default_option_3_costs,
                               var_CV = var_cv_40,
                               n = 40)


  Own_business_branch <- vv(var_mean = Own_branch,
                            var_CV = var_cv_40,
                            n = 40)

  Off_Farm_job <- vv(var_mean = Off_Farm_job,
                     var_CV = var_cv_40,
                     n = 40)


  Costs_for_child_care <- vv(var_mean = Costs_for_child_care,
                             var_CV = var_cv_40,
                             n = 40)

  Costs_for_elderly_care <- vv(var_mean = Costs_for_elderly_care,
                               var_CV = var_cv_40,
                               n = 40)

  State_insurance <- vv(var_mean = State_insurance,
                        var_CV = var_cv_17,
                        n = 40)

  State_insurance_costs <- vv(var_mean = State_insurance_inv,
                             var_CV = var_cv_40,
                              n = 40)

  Family_money <- vv(var_mean = Family_money,
                     var_CV = var_cv_40,
                     n = 40)

  Farm_job_payed <- vv(var_mean = Farm_job_payed,
                       var_CV = var_cv_40,
                       n = 40)


  Agri_insurance <- vv(var_mean = Agri_insurance,
                       var_CV = var_cv_17,
                       n = 40)

  Agri_insurance_costs <-  vv(var_mean = Agri_insurance_inv,
                              var_CV = var_cv_40,
                              n = 40)

  
  ETF <- vv(var_mean = ETF, 
            var_CV = var_cv_17, 
            n = 40)
  
  ETF_costs <- vv(var_mean = ETF_costs, 
                  var_CV = var_cv_40, 
                  n = 40)
  
  Mix <- vv(var_mean = ETF, 
            var_CV = var_cv_17, 
            n = 40)
  
  Mix_costs <- vv(var_mean = Mix_costs, 
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
  
  Late_transfer_risk_obstacle <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  Child_Elderly_risk_obstacle <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  
  # use chance_event() 
  # assuming  0 Own_business_branch  at all in the event of no child care option
  Costs_for_child_care_adjusted_Own_business_branch <- chance_event(chance = 
                                                                      Costs_for_child_care, 
                                                                    value_if = 0,
                                                                    value_if_not = Own_branch,
                                                                    n = var_cv_40)
  # use chance_event() 
  # assuming  0 Own_business_branch  at all in the event of no elderly care option
  Costs_for_elderly_care_adjusted_Own_business_branch <- chance_event(chance = 
                                                                        Costs_for_elderly_care, 
                                                                      value_if = 0,
                                                                      value_if_not = Own_branch,
                                                                      n = var_cv_40)
  
  
  if(Way == 1){
    
    profit_without_Own_business_branch <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Own_business_branch <- (Own_business_branch + Agri_insurance - Agri_insurance_costs - Costs_for_child_care - Costs_for_elderly_care)
    
    
    NPV_no_branch <- discount(profit_without_Own_business_branch,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Own_business_branch,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    

  
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision,
                Cashflow_decision_gender = profit_with_Own_business_branch - profit_without_Own_business_branch))
  }
  
  if(Way == 2){
    
    profit_without_Job_away_of_farm <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Job_away_of_farm <- (Off_Farm_job + State_insurance - State_insurance_costs - Costs_for_child_care - Costs_for_elderly_care)
    
    NPV_no_branch <- discount(profit_without_Job_away_of_farm,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Job_away_of_farm,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  if(Way == 3){
    
    profit_without_Own_business_branch <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Job_away_of_farm <- (Off_Farm_job + (State_insurance + 50) - (State_insurance_costs + 20) - Costs_for_child_care - Costs_for_elderly_care)
    
    NPV_no_branch <- discount(profit_without_Own_business_branch,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Job_away_of_farm,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  if(Way == 4){
    
    profit_without_Own_business_branch <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Family_money <- (Family_money + Agri_insurance - Agri_insurance_costs)
    
    NPV_no_branch <- discount(profit_without_Own_business_branch,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Family_money,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision, 
                risk))
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

# mcSimulation_results_way4 <- decisionSupport::mcSimulation(
#   estimate = decisionSupport::as.estimate(input_table_gender),
#   model_function = decision_function,
#   numberOfModelRuns = 200,
#   functionSyntax = "plainNames"
# )

# mcSimulation_results_way4 <- decisionSupport::mcSimulation(
#   estimate = decisionSupport::as.estimate(input_table_gender),
#   model_function = decision_function,
#   numberOfModelRuns = 200,
#   functionSyntax = "plainNames"
# )

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way2, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way1, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)



# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way4, 
#                                     vars = c("NPV_no_branch", "NPV_branch"),
#                                     method = 'smooth_simple_overlay', 
#                                     base_size = 7)




## Cashflow: Here we plot the distribution of annual cashflow 
# over the entire simulated period for the intervention (n_years). 
# For this we use the plot_cashflow() function 
# which uses the specified cashflow outputs from the mcSimulation() function to show cashflow over time.
# In the example, the cashflow_var_name is "Cashflow_decision_do".
# In our case, it is not implemented yet in the return fuction. 
# First, I implemented it in line 185 in the return function 
# (Cashflow_decision_gender = profit_with_Own_business_branch - profit_without_Own_business_branch), 
# but I think the values show, that we actually dont really have distingueshed time horizons
# betrween pension payment and retirement. 
# I suggest renaming all the n = 40 to n_years, which should be specified in the beginning of our script.
# I selected the MsSimulation results of way 1 and only implemented the cashflow for way 1


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

plot_pls(pls_result, threshold = 0, )

#input_table = input_table_gender Rauslassen


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


