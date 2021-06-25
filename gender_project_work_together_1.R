library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
#questions: child caree in risks and(!) if statements? doubled? easier option?
#die kosten auch in die tabelle mit werten schreiben, obwohl in R implementiert? einfachere mgl.?
# kann man die kosten vereinfachen und in die anderen for-schleifen einfügen?:
#ETF_costs<-TRUE
#ETF_costs =200
#das ist nicht notwendig, würde den code aber hübscher machen
#die ersten calcs weglöschen? muss noch ausprobieren
#no_plan klappt nicht
#default options stehen bestimmt überall doppelt drin


#was noch zu tun wäre:
#bei den costs put together für jede variante einzeln einfügen
#sowas:
#Overall_benefits<- Family_money+ Farm_job_payed + Own_branch + Off_Farm_job + Agri_insurance + Private_insurance +State_insurance + ETF + Mix

#Overall_benefits_no_plan<-Default_option_2 + Default_option_3
#für overall costs und overall benefits

####first step:get data####

input_table_gender <-read.csv2("./input_table_gender.csv", dec = ",")
names(input_table_gender)
str(input_table_gender)

test <- na.omit(input_table_gender)

####function####

#First we generate a model as a function.
#We use the decisionSupport functions vv() to produce 
#time series with variation from a pre-defined mean and 
#coefficient of variation,chance_event() to simulate whether events 
#occur and discount() to discount values along a time series.
#discount: diskontierung: auf uhrsprungsjahr zur?ckrechnen

decision_function <- function(x, varnames){
  
####vvs####  
# use vv() to add variability to the 
# random draws of own business branch and of 
#state insurance, private insurance, ETF
# over a 65 year simulation 
# over a 82 year simulation (estimated death)
# 40 years insurance, paying 12 month a year =480
# 82 years alive after paying 480 euro/ month:insurance (82-25-40=17)
# ETF: 82 x 12= 984
# 17 years, 12 month:204
# Age of retirenment: 65
# marriage age 25
  
  
Own_business_branch <- vv(var_mean = Own_branch, 
                          var_cv_40 = 5, 
                          n = 2)
  
  
Off-Farm_job <- vv(var_mean = Off-Farm_job, 
                   var_cv_40 = 5, 
                    n = 2)
  
Family_money <- vv(var_mean = Family_money, 
                    var_cv_40 = var_cv_40, 
                    n = 50)
  
  
Farm_job_payed <- vv(var_mean = Farm_job_payed, 
                      var_cv_40 = 5, 
                      n = 2)
  
ETF <- vv(var_mean = ETF, 
          var_cv_82 = 5, 
          n = 2)    
  
Private_insurance <- vv(var_mean = Private_insurance, 
                        var_cv_40 = 5, 
                        n = 2)

Private_insurance_inv <- vv(var_mean = Private_insurance_inv, 
                            var_cv_17 = 5, 
                            n = 2)
  
State_insurance <- vv(var_mean = State_insurance, 
                      var_cv_17 = 5, 
                      n = 2)
  
State_insurance_inv <- vv(var_mean = State_insurance_inv, 
                          var_cv_17 = 5, 
                          n = 2)
  
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
  
  
####add variability####
  
  
####start with own business branch####
# adjust branches for probability that other branches before were not "taken"
#65 jahre rente mal 12 Monate=780
  
# use chance_event() 
# assuming  0 Own_business_branch  at all in the event of no child care option
Costs_for_child_care_adjusted_Own_business_branch <- chance_event(chance = 
                                                      Costs_for_child_care, 
                                                      value_if = 0,
                                                      value_if_not = Own_branch,
                                                      n = var_CV_40)
# use chance_event() 
# assuming  0 Own_business_branch  at all in the event of no elderly care option
Costs_for_elderly_care_adjusted_Own_business_branch <- chance_event(chance = 
                                                      Costs_for_elderly_care, 
                                                      value_if = 0,
                                                      value_if_not = Own_branch,
                                                      n = var_CV_40)
  
  
  
# calculate pension without own business branch
profit_without_Own_business_branch <- Default_option_3 + Default_option_2
profit_with_Own_business_branch <- Own_branch
  
  
# use 'discount' to calculate net present value 
#'discount_rate' is expressed in percent
NPV_no_branch <- discount( profit_without_Own_business_branch,
                             discount_rate = 5, calculate_NPV = TRUE)    
NPV_branch <- discount(profit_with_Own_business_branch,
                         discount_rate = 5, calculate_NPV = TRUE)
  
# calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_branch-NPV_no_branch
  
return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
  
#### then private insurance####
  
# calculate pension without private insurance
profit_without_private_insurance <- Default_option_3 + Default_option_2

# calculate pension with private insurance
profit_with_private_insurance <- Private_insurance
  
# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_pi <- discount(profit_without_private_insurance,
                        discount_rate = 5, calculate_NPV = TRUE)
NPV_pi <- discount(profit_with_private_insurance,
                     discount_rate = 5, calculate_NPV = TRUE)
  
#calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_pi-NPV_no_pi
  
  return(list(NPV_no_pi =  NPV_no_pi,
              NPV_pi =  NPV_pi, 
              NPV_decision = NPV_decision))
  
  
#### then state insurance####
  
# calculate pension without state insurance
profit_without_State_insurance <- Default_option_3 + Default_option_2
  
# calculate pension with state insurance
profit_with_State_insurance <- State_insurance
  
# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_si <- discount(profit_without_State_insurance,
                        discount_rate = 5, calculate_NPV = TRUE)
NPV_si <- discount(profit_with_State_insurance,
                     discount_rate = 5, calculate_NPV = TRUE)
  
# calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_si-NPV_no_si
  
return(list(NPV_no_si =  NPV_no_si,
              NPV_si =  NPV_si, 
              NPV_decision = NPV_decision))
  
  
#### then ETF ####

# calculate pension without ETF
profit_without_ETF <- Default_option_3 + Default_option_2
  
# calculate pension with ETF
profit_with_ETF <- ETF

# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_ETF <- discount(profit_without_ETF,
                         discount_rate = 5, calculate_NPV = TRUE)
NPV_ETF <- discount(profit_with_ETF,
                      discount_rate = 5, calculate_NPV = TRUE)
  
# calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_ETF-NPV_no_ETFv
  
  return(list(NPV_no_ETF =  NPV_no_ETF,
              NPV_ETF =  NPV_ETF, 
              NPV_decision = NPV_decision))

##then Mix private insurance and ETF##

#### then Mix ####

# calculate pension without Mix
profit_without_ETF <- Default_option_3 + Default_option_2

# calculate pension with Mix
profit_with_Mix <- Mix

# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_Mix <- discount(profit_without_Mix,
                       discount_rate = 5, calculate_NPV = TRUE)
NPV_Mix <- discount(profit_with_Mix,
                    discount_rate = 5, calculate_NPV = TRUE)

# calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_ETF-NPV_no_Mix

return(list(NPV_no_Mix =  NPV_no_Mix,
            NPV_Mix =  NPV_Mix, 
            NPV_decision = NPV_decision))
}

####Model branches####
# Estimate the pension without plan
No_Plan <- Default_option_2 + Default_option_3
  
  if (Default_option_2 + Default_option_3) {
    Family_money <- FALSE
    Farm_job_payed <- FALSE
    Own_branch <- FALSE
    Off_Farm_job <- FALSE}

#no cost
#doppeln sich die child and elderly care sachen mit den risks?

####first branch of the tree: own business branch

for (Own_branch in c(FALSE,TRUE))
{
  
  if (Own_branch)
  {
    Costs_for_child_care <- TRUE
    Costs_for_elderly_care <- TRUE

  } else
  {
    Own_branch <- FALSE

  }
}

  
#### Second branch of the tree:Job away from the farm
  
for (Off_Farm_job in c(FALSE,TRUE))
  {
    
    if (Off_Farm_job)
    {
      Costs_for_child_care <- TRUE
      Costs_for_elderly_care <- TRUE
      
    } else
    {
      Off_Farm_job <- FALSE
    } 
    }
if (Off_Farm_job)
{
  State_insurance <- TRUE

}

      
####Pension options: Third branch of the tree:agricultural 
#insurance without default option
      
      
      for (Agri_insurance in c(FALSE,TRUE))
      {
        
        if (Agri_insurance)
        {
          Family_money <- TRUE
          Farm_job_payed <- TRUE
          Own_branch <- TRUE
 
      } 
      else
      {
        Agri_insurance <- FALSE
      } 
}
####Pension options: Forth branch of the tree: State inurance
        
        
        for (State_insurance in c(FALSE,TRUE))
        {
          
          if (State_insurance)
          {
            Family_money <- TRUE
            Farm_job_payed <- TRUE
            Own_branch <- TRUE
            Off_Farm_job <- TRUE
          } else
          {
            State_insurance <- FALSE
          } 
        }
####Pension options: Fifth branch of the tree: ETF


for (ETF in c(FALSE,TRUE))
{
  
  if (ETF)
  {
    Family_money <- TRUE
    Farm_job_payed <- TRUE
    Own_branch <- TRUE
    Off_Farm_job <- TRUE

} else
{
  ETF <- FALSE
} 
}

####Pension options: Sixth branch of the tree: Mix


for (Mix in c(FALSE,TRUE))
{
  
  if (ETF)
  {
    Family_money <- TRUE
    Farm_job_payed <- TRUE
    Own_branch <- TRUE
    Off_Farm_job <- TRUE
    
  } else
  {
    Mix <- FALSE
  } 
}  
  
  
  
#### Costs ####



####Pension options: State insurance####

for (State_insurance_inv in c(FALSE,TRUE))
{
  
  if (Farm_job_payed)
  {
    State_insurance_inv<- TRUE 
    State_insurance_inv= 200
  }
  if (Family_money)
  {
    State_insurance_inv<- TRUE 
    State_insurance_inv= 100
  }
  if (Own_branch)
  {
    State_insurance_inv<- TRUE 
    State_insurance_inv= 300
  }
    if (Off_Farm_job)
    {
      State_insurance_inv<- TRUE 
      State_insurance_inv= 200
    }

  else
    {
    State_insurance <- FALSE
    } 
}



####Pension options:  ETF


for (ETF_costs in c(FALSE,TRUE))
{
  if (Farm_job_payed)
  {
    ETF_costs<- TRUE 
    ETF_costs= 200

  }
  if (Family_money)
  {
    ETF_costs<- TRUE
    ETF_costs = 100
  }
  if (Own_branch)
  {
    ETF_costs<- TRUE
    ETF_costs = 300
  }
  if (Off_Farm_job)
  {
    State_insurance_inv<- TRUE 
    State_insurance_inv= 200
  
} else
{
  ETF <- FALSE
} 
}



####Pension options:  Mix


for (Mix_costs in c(FALSE,TRUE))
{

    if (Farm_job_payed)
    {
      Mix_costs<-TRUE
      Mix_costs = 200
    }
    if (Family_money)
    {
      Mix_costs<-TRUE
      Mix_costs = 100
    }
    if (Own_branch)
    {
      Mix_costs<-TRUE
      Mix_costs = 300
    }
   if (Off_Farm_job)
   {
     State_insurance_inv<- TRUE 
     State_insurance_inv= 200
    
  } else
  {
    Mix <- FALSE
  } 
}

#####Pension options:  Private insurance


for (Private_insurance_inv in c(FALSE,TRUE))
{
  if (Farm_job_payed)
  {
    Private_insurance_inv<-TRUE
    Private_insurance_inv = 200
  }
  if (Family_money)
  {
    Private_insurance_inv<-TRUE
    Private_insurance_inv = 100
  }
  if (Own_branch)
  {
    MPrivate_insurance_inv<-TRUE
    Private_insurance_inv = 300
  }
  if (Off_Farm_job)
  {
    Private_insurance_inv<- TRUE 
    Private_insurance_inv= 200
    
  } else
  {
    Private_insurance <- FALSE
  } 
}

######Pension options: Agricultral insurance


for (Agri_insurance_inv in c(FALSE,TRUE))
{
  if (Farm_job_payed)
  {
    Agri_insurance_inv<-TRUE
    Agri_insurance_inv = 200
  }
  if (Family_money)
  {
    Agri_insurance_inv<-TRUE
    Agri_insurance_inv = 100
  }
  if (Own_branch)
  {
    Agri_insurance_inv<-TRUE
    Agri_insurance_inv = 300
  }
  if (Off_Farm_job)
  {
    Agri_insurance_inv<- TRUE 
    Agri_insurance_inv= 200
    
  } else
  {
    Agri_insurance <- FALSE
  } 
}

####Put everything together####
####Costs####
Costs<-(State_insurance_inv + Agri_insurance_inv + Private_insurance_inv + State_insurance_inv + ETF_costs + Mix_costs)



if (Overall_costs) {
    Overall_costs <-
    State_insurance_inv + 
    Agri_insurance_inv + 
    Private_insurance_inv + 
    State_insurance_inv + 
    ETF_costs + 
    Mix_costs
} else
  Overall_costs <- 0

    

if (Overall_costs_no_plan) {
  Overall_costs_no_plan <-
  Default_option_3_costs
} else
  No_Plan <- 0

maintenance_cost <- rep(0, n_years)


#### Benefits  ####

if (Overall_benefits) {
  Overall_benefits <-
    State_insurance + 
    Agri_insurance + 
    Private_insurance + 
    State_insurance + 
    ETF + 
    Mix
} else
  Overall_costs <- 0
  
Overall_benefits <- Family_money + Farm_job_payed + Own_branch + Off_Farm_job +
  Agri_insurance + Private_insurance + State_insurance + ETF + Mix

Overall_benefits_no_plan <-Default_option_2 + Default_option_3



#NPV_interv <-
#  discount(result_interv, discount_rate, calculate_NPV = TRUE)

#NPV_n_interv <-
#  discount(result_n_interv, discount_rate, calculate_NPV = TRUE)

# Beware, if you do not name your outputs 
# (left-hand side of the equal sign) in the return section, 
# the variables will be called output_1, _2, etc.

#return(list(Interv_NPV = NPV_interv,
#            NO_Interv_NPV = NPV_n_interv,
#            NPV_decision_do = NPV_interv - NPV_n_interv,
#            Cashflow_decision_do = result_interv - result_n_interv))
#}





####perform a monte carlo simulation####

#Using the model function above,
#we can perform a Monte Carlo simulation with the mcSimulation() 
#function from decisionSupport. This function generates 
#distributions of all variables in the input table as well 
#as the specified model outputs (see return() function above)
#by calculating random draws in our defined example_decision_function().
#Make sure that all the variables in the input table are included
#in the model (erroneous variables listed there can cause issues
#with some of the post-hoc analyses).

#The numberOf ModelRuns argument is an integer indicating 
#the number of model runs for the Monte Carlo simulation. 
#Unless the model function is very complex, 
#10,000 runs is a reasonable choice 
#(for complex models,10,000 model runs can take a while, 
#so especially when the model is still under development, 
#it often makes sense to use a lower number).

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results

