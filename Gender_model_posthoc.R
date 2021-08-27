# Gender model post-hoc

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


#Plot distributions one by one
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_no_branch",
                                    method = 'boxplot_density')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_profit_with_off_farm_job_6",
                                    method = 'boxplot_density')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_profit_with_family_money_13",
                                    method = 'boxplot_density')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_profit_with_family_money_14",
                                    method = 'boxplot_density')



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
CashflowN <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_gender_way_N",
                           x_axis_name = "Year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")

#CashflowA
#CashflowB
#CashflowC
#CashflowD
#CashflowE
#CashflowF
#CashflowG
#CashflowH
#CashflowI
#CashflowJ
#CashflowK
#CashflowL
#CashflowM
#CashflowN





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
                                  resultName = names(mcSimulation_results$y)[3],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_Own_business_branch_2"
pls_result_2 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[62],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_Own_business_branch_3"
pls_result_3 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[121],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_4"
pls_result_4 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[180],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_5"
pls_result_5 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[239],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_6"
pls_result_6 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[298],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_off_farm_job_7"
pls_result_7 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[357],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_on_farm_job_8"
pls_result_8 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[416],
                                  ncomp = 1)

#	Pls of	"NPV_decision_profit_with_on_farm_job_9"
pls_result_9 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = names(mcSimulation_results$y)[475],
                                  ncomp = 1)

#	Pls of	 "NPV_decision_profit_with_on_farm_job_10"
pls_result_10 <- plsr.mcSimulation(object = mcSimulation_results,
                                   resultName = names(mcSimulation_results$y)[534],
                                   ncomp = 1)

#	Pls of	"NPV_decision_profit_with_on_farm_job_11"
pls_result_11 <- plsr.mcSimulation(object = mcSimulation_results,
                                   resultName = names(mcSimulation_results$y)[539],
                                   ncomp = 1)

#	Pls of	"NPV_decision_profit_with_family_money_12"
pls_result_12 <- plsr.mcSimulation(object = mcSimulation_results,
                                   resultName = names(mcSimulation_results$y)[652],
                                   ncomp = 1)

#	Pls of	"NPV_decision_profit_with_family_money_13"
pls_result_13 <- plsr.mcSimulation(object = mcSimulation_results,
                                   resultName = names(mcSimulation_results$y)[711],
                                   ncomp = 1)

#	Pls of	"NPV_decision_profit_with_family_money_14"
pls_result_14 <- plsr.mcSimulation(object = mcSimulation_results,
                                   resultName = names(mcSimulation_results$y)[770],
                                   ncomp = 1)


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

# We calculate Value of Information (VoI) analysis 
# with the Expected Value of Perfect Information (EVPI). 
# As we learned in Lecture 8 on forecasts, EVPI measures 
# the expected opportunity loss that is incurred when the decision-maker 
# does not have perfect information about a particular variable. 
# EVPI is determined by examining the influence of that 
#variable on the output value of a decision model.
mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, 
                   first_out_var = "NPV_profit_with_Own_business_branch_1")
plot_evpi(evpi,
          decision_vars = "NPV_decision_profit_with_Own_business_branch_1")

names(mcSimulation_results$y[1:3])
names(mcSimulation_results$y)[711]

colnames(mcSimulation_results$y[1:3])



mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[c(1,61,62)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_Own_business_branch_2")
plot_evpi(evpi,
          decision_vars = "NPV_decision_profit_with_Own_business_branch_2")



mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,120,121)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_Own_business_branch_3")
plot_evpi(evpi,
          decision_vars = "NPV_decision_profit_with_Own_business_branch_3")


mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,179,180)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_off_farm_job_4")
plot_evpi(evpi,
          decision_vars = "NPV_decision_profit_with_off_farm_job_4")


mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,238,239)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_off_farm_job_5")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_off_farm_job_5")


mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,297,298)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_off_farm_job_6")
plot_evpi(evpi, 
          decision_vars = "NPV_decision_profit_with_off_farm_job_6")


#here is an evpi:

mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,356,357)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_off_farm_job_7")
plot_evpi(evpi,
          decision_vars = "NPV_decision_profit_with_off_farm_job_7")


mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,415,416)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_on_farm_job_8")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_8")


mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,474,475)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_on_farm_job_9")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_9")


mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[c(1,533,534)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_on_farm_job_10")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_10")


mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,592,593)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_on_farm_job_11")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_on_farm_job_11")


mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,651,652)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_family_money_12")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_family_money_12")


#here is an evpi:


mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[c(1,710,711)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_family_money_13")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_family_money_13")

#here is an evpi:

mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[c(1,769,770)])
evpi <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_profit_with_family_money_14")
plot_evpi(evpi, decision_vars = "NPV_decision_profit_with_family_money_14")

mcSimulation_results$y[c(1,769,770)]





