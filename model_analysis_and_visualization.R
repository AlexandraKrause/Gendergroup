


# Alinas Addition to the model script

## Cashflow: Here we plot the distribution of annual cashflow 
# over the entire simulated period for the intervention (n_years). 
# For this we use the plot_cashflow() function 
# which uses the specified cashflow outputs from the mcSimulation() function to show cashflow over time.
# In the example, the cashflow_var_name is "Cashflow_decision_do".
# In our case, it could be "NPV_branch", but I don't know, if this is correct.
# 

# As the n_years is not implemented in our model, I think that the function won't work immediately.
# I suggest renaming all the n = 40 to n_years, which should be specified in the beginning of our script.
# As I selected the MsSimulation results of way 1 and 
# the script does not work,  can not run the plot_cashflow function.


plot_cashflow(mcSimulation_object = mcSimulation_results_way1, cashflow_var_name = "NPV_branch")


##another option to create a cashflow




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
# will not work, because we miss the cashflow in our in the return funktion. 
# TODO we need to add something analugue to
# "Cashflow_decision_do = result_interv - result_n_interv)" to the return function. 
# The plsr.mcSimulation will only work if we include the chashflow in the return function.

pls_result <- plsr.mcSimulation(object = mcSimulation_results_way1,
                                resultName = names(mcSimulation_results_way1$y)[3], ncomp = 1)


## Plot PLS: 
# We run the plot_pls() on the results from plsr.mcSimulation() 
# with a number of standard settings. 
# The plot_pls() function uses the text in the label column 
# as replacement for the default text in the variable column.
# This will cause problems, because the label column in our input_table_gender is 
# not helpful for labeling a plot. (it says something like financing, percent and options)
# TO Do: In our input_table_gender, we need to adjust the column label, 
# and fill it analogue to the example input table with appropriate lables for plotting. 

plot_pls(pls_result, input_table_gender = input_table_gender, threshold = 0)

## EVPI analysis
## I think this is not possible to code without a running model


# graphical options to visualize uncertainty intervals of outcomes of Monte Carlo simulations. 
# We create a data set of yield distributions of three different farming practices 
# and use the function  with mcmc_areas() function from the bayesplot library (Gabry and Mahr 2021).
#here, we need a data frame with our simulation outcomes. 
# I do not know yet how to generate that. Maybe this works: 
# as.data.frame.mcSimulation: Coerce Monte Carlo simulation results to a data frame.
# https://rdrr.io/cran/decisionSupport/man/as.data.frame.mcSimulation.html

library(bayes)
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

color_scheme_set("blue")

mcmc_areas(test,prob = 0.9,point_est = "median")
