

# Default: she is a wife and does nothing special
profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
# this is our default option, only agricultural insurance , works for all of us


# Way 1 own branch with investment in private insurance
profit_with_Own_business_branch <- (Private_insurance_inv + Private_insurance + Agri_insurance - Agri_insurance_inv - Costs_for_child_care - Costs_for_elderly_care) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
 # have her own branch but spend nothing more in private insurance
  # she has different risks
  # she has to pay for child and elderly care  
  # Potential Udate branch 1: Private_insurance_inv + Private_insurance
  # -Own_branch*0.1 is equivalent to Private_insurance_inv

  # does she want to invest in other things?
  # maybe (if we have time) implement another branch of investing in other things, such as etf or mix.


# Way 2 = Job away from farm not specific pension plan, but she pays in state insurance
profit_with_Job_away_of_farm <- (State_insurance - State_insurance_inv - Costs_for_child_care - Costs_for_elderly_care)  * (1- Husband_risk)
  
# Way 3 = Job away from farm and she invests in her retirement with private insurance
profit_with_Job_away_of_farm <- (State_insurance - State_insurance_inv - Private_insurance_inv + Private_insurance - Costs_for_child_care - Costs_for_elderly_care) * (1- Husband_risk)

# Way 4 = Family money investment in private insurance
profit_with_Family_money <- (- Private_insurance_inv + Private_insurance + Agri_insurance - Agri_insurance_inv)  * (1-Bancruptcy_risk * Man_Death_risk * Divorce_risk * Husband_risk)

# Way 5 = Payment of wife (on farm job)
profit_with_On_Farm_Job <- ((State_insurance - State_insurance_inv) * (1- Husband_risk))

# Way 6 = Payment of wife (on farm job) with investment in private insurance
profit_with_On_Farm_Job <- ((State_insurance - State_insurance_inv - Private_insurance_inv + Private_insurance ) * 1- Husband_risk)

  