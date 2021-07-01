
## Cashflow: Here we plot the distribution of annual cashflow 
# over the entire simulated period for the intervention (n_years). 
# For this we use the plot_cashflow() function 
# which uses the specified cashflow outputs from the mcSimulation() function to show cashflow over time.
# In the example, the cashflow_var_name is "Cashflow_decision_do".
# In our case, it is not implemented yest in the return fuction. 
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


# graphical options to visualize uncertainty intervals of outcomes of Monte Carlo simulations. 
# We create a data set of yield distributions of three different farming practices 
# and use the function  with mcmc_areas() function from the bayesplot library (Gabry and Mahr 2021).
#here, we need a data frame with our simulation outcomes. 
# I do not know yet how to generate that. Maybe this works: 
# as.data.frame.mcSimulation: Coerce Monte Carlo simulation results to a data frame.
# https://rdrr.io/cran/decisionSupport/man/as.data.frame.mcSimulation.html
#I do not know how to find the information for this plot. converting the simulation in data frame does not work. I do not know were to find the values.

library(bayes)
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

color_scheme_set("blue")


library(bayes)
mcmc_areas(mcSimulation_results_way1,prob = 0.9,point_est = "median")

data1 <- as.data.frame(mcSimulation_results_way1)

library(bayesplot)
mcmc_areas(data1,prob = 0.9,point_est = "median")

# the same for way 2, i ll try

plot_cashflow(mcSimulation_object = mcSimulation_results_way2, cashflow_var_name = "Cashflow_decision_gender")



pls_result <- plsr.mcSimulation(object = mcSimulation_results_way2,
                                resultName = names(mcSimulation_results_way2$y)[3], ncomp = 1)


plot_pls(pls_result, threshold = 0, )


mcSimulation_table <- data.frame(mcSimulation_results_way2$x, mcSimulation_results_way2$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
plot_evpi(evpi, decision_vars = "NPV_decision")


compound_figure(mcSimulation_object = mcSimulation_results_way2, 
                input_table = input_table_gender, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision", 
                cashflow_var_name = "Cashflow_decision_gender", 
                base_size = 7)