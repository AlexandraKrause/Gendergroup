# Gendergroup
https://rdrr.io/cran/decisionSupport/man/vv.html
In our model we compare multiple branches. First there are differnt options to finance pension schemes and also different pension schemes the women can choose from. Up until now we have not managed to implement all branches (the "put together "part after the calculation of benefits and costs for each option), but we are in the process of completing the missing branches. We have discount rates for differnt years that apply for retirenment and working years: We have multiple risks and  chances for events to happpen.  We have variability in all branches ( inflation, worth of money, costs during her lifetime; paying in different years than receiving money).


In the immediate future:
- the error! Values from the input table are not recognized
- we used dummy data up till now. Next there will be actual data
- the "put together" function is a note right now. It will be implemented as if statements. Overall benefits and costs for each possibility. as  a test it is now just put together in the worst way

In the far future:

- we will put a discount rate for the job (when she has enough money to pay more for ETF and private insurance and Mix of both) with two branches (one before 10 years and one after 10 years)
- model analysis tools 
- visuals ( mcmc_areas() function from the bayesplot library)
- plot cashflow to see tyüical pofile of pension insurance : negative flow for several decades, later on positive flow
- project report markdown, implement model in mermaid





![Uploading grafik.png…]()


