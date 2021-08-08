# devtools::install_github("eikeluedeling/decisionSupport")

# Plotting the cashflow:

# Create the estimate object (for multiple options):
library(decisionSupport)

variable = c("revenue_option1", "costs_option1", "n_years", "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 20, 8000,  500)
upper = c(100000, 50000, 20, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

# Define the model function without name for the return value:

profit1 <- function(x) {
  
cashflow_option1 <- vv(revenue_option1 - costs_option1, n = n_years, var_CV = 100)

cashflow_option2 <- vv(revenue_option2 - costs_option2, n = n_years, var_CV = 100)

return(list(Revenues_option1 = revenue_option1,
            Revenues_option2 = revenue_option2,
            Costs_option1 = costs_option1,
            Costs_option2 = costs_option2,
            Cashflow_option_one = cashflow_option1,
            Cashflow_option_two = cashflow_option2))
}

# Perform the Monte Carlo simulation:

predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")

# Plot the cashflow distribution over time

plot_cashflow(mcSimulation_object = predictionProfit1, 
              cashflow_var_name = "Cashflow_option_one",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", 
              color_5_95 = "green1",
              color_median = "red")

pls_result <- plsr.mcSimulation(object = predictionProfit1,
                                resultName = names(predictionProfit1$y)[3], ncomp = 1)

plot_pls(pls_result, threshold = 0)

is.na(predictionProfit1$y)
