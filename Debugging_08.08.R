library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(decisionSupport)

#devtools::install_github("eikeluedeling/decisionSupport")


variable = c("Private_insurance_own_branch", "Private_insurance_inv_own_branch", 
             "Agri_insurance", "Agri_insurance_inv", "n_years_payment", "n_years_revenue", 
             "ETF_own_branch", "ETF_inv_own_branch")
distribution = c("norm", "norm", "norm", "norm", "const", "const", "norm", "norm")
lower = c(3480, 960, 7092, 2893, 40, 17, 8640, 960)
upper = c(5160, 1440, 7103.28, 2902.56, 40, 17, 12000, 1440)
var_slight = 1

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)


# n_years_payment = 40
# n_years_revenue = 17
# 
# Agri_insurance <- c(rep (0, n_years_payment),vv(var_mean = 7092,
#                                                 var_CV = var_slight,
#                                                 n = n_years_revenue))
# 
# Agri_insurance_inv <- c(vv(var_mean = 2893,
#                            var_CV = var_slight,
#                            n = n_years_payment), rep(0, n_years_revenue))
# 
# Private_insurance_own_branch <- c(rep (0, n_years_payment),vv(var_mean = 3480,
#                                                               var_CV = var_slight,
#                                                               n = n_years_revenue))
# 
# Private_insurance_inv_own_branch <- c(vv(var_mean = 960,
#                                          var_CV = var_slight,
#                                          n = n_years_payment), rep(0, n_years_revenue))
# 
# ETF_own_branch <- c(rep(0,n_years_payment),vv(var_mean = 8640,
#                                               var_CV = var_slight,
#                                               n = n_years_revenue))
# 
# ETF_inv_own_branch <- c(vv(var_mean = 960,
#                            var_CV = var_slight,
#                            n = n_years_payment), rep(0, n_years_revenue))
# 
# 
# cashflow_option1 <- (Private_insurance_own_branch + Agri_insurance -
#                        Private_insurance_inv_own_branch- Agri_insurance_inv)
# 
# cashflow_option2 <- (ETF_own_branch + Agri_insurance -
#                        ETF_inv_own_branch - Agri_insurance_inv)
# 
# NPV_profit_with_Own_business_branch_1 <- discount(cashflow_option1,
#                                                   discount_rate = 1, calculate_NPV = TRUE)
# 
# NPV_profit_with_Own_business_branch_2 <- discount(cashflow_option2,
#                                                   discount_rate = 1, calculate_NPV = TRUE)
# 
# 
# 
# 
# 
# Agri_insurance <- c(rep (0, 40),vv(var_mean = 7092,
#                                                 var_CV = 1,
#                                                 n = 17))
# 
# Agri_insurance_inv <- c(vv(var_mean = 2893,
#                            var_CV = 1,
#                            n = 40), rep(0, 17))
# 
# Default_option <- c(vv(var_mean = 2893,
#                        var_CV = 1, 
#                        n = 40), rep(0,17))
# 
# 
# Cash1 <- Agri_insurance + Default_option
# Investment2 <- Agri_insurance_inv
# profit_Default <- (Cash1 - Investment2)
# 
# NPV_no_branch <- discount(profit_Default,
#                           discount_rate = 1, calculate_NPV = TRUE) 



profit1 <- function(x){
  
  Agri_insurance <- c(rep (0, n_years_payment),vv(var_mean = Agri_insurance,
                                               var_CV = var_slight,
                                               n = n_years_revenue))

  Agri_insurance_inv <- c(vv(var_mean =Agri_insurance_inv,
                             var_CV = var_slight,
                             n = n_years_payment), rep(0, n_years_revenue))

  Private_insurance_own_branch <- c(rep (0, n_years_payment),vv(var_mean = Private_insurance_own_branch,
                                                             var_CV = var_slight,
                                                             n = n_years_revenue))

  Private_insurance_inv_own_branch <- c(vv(var_mean = Private_insurance_inv_own_branch,
                                            var_CV = var_slight,
                                            n = n_years_payment), rep(0, n_years_revenue))

  ETF_own_branch <- c(rep(0,n_years_payment),vv(var_mean = ETF_own_branch,
                                              var_CV = var_slight,
                                              n = n_years_revenue))

  ETF_inv_own_branch <- c(vv(var_mean = ETF_inv_own_branch,
                             var_CV = var_slight,
                             n = n_years_payment), rep(0, n_years_revenue))
  
  cashflow_option1 <- (Private_insurance_own_branch + Agri_insurance - 
                         Private_insurance_inv_own_branch- Agri_insurance_inv) 
                       
  cashflow_option2 <- (ETF_own_branch + Agri_insurance - 
                         ETF_inv_own_branch- Agri_insurance_inv)
  
  NPV_profit_with_Own_business_branch_1 <- discount(cashflow_option1,
                                                    discount_rate = 1, calculate_NPV = TRUE)
  
  NPV_profit_with_Own_business_branch_2 <- discount(cashflow_option2,
                                                    discount_rate = 1, calculate_NPV = TRUE)
  
  return(list(Agri_insurance = Agri_insurance, 
              Private_insurance_own_branch = Private_insurance_own_branch,
              ETF_own_branch = ETF_own_branch, 
              Agri_insurance_inv = Agri_insurance_inv, 
              Private_insurance_inv_own_branch = Private_insurance_inv_own_branch,
              ETF_inv_own_branch = ETF_inv_own_branch, 
              cashflow_option_one = cashflow_option1, 
              cashflow_option_two = cashflow_option2,
              NPV_profit_with_Own_business_branch_1 = NPV_profit_with_Own_business_branch_1,
              NPV_profit_with_Own_business_branch_2 = NPV_profit_with_Own_business_branch_2))
}


predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


plot_cashflow(mcSimulation_object = predictionProfit1,
              cashflow_var_name = "cashflow_option_one",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4",
              color_5_95 = "green1",
              color_median = "red")

plot_cashflow(mcSimulation_object = predictionProfit1,
              cashflow_var_name = "cashflow_option_two",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4",
              color_5_95 = "green1",
              color_median = "red")




decisionSupport::plot_distributions(predictionProfit1, 
                                    vars = c("NPV_profit_with_Own_business_branch_1",
                                             "NPV_profit_with_Own_business_branch_2"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

