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
- implement agroforestry as pension option: buy land and plant trees and take care for them for a wood / fruit /nut pension






![grafik](https://user-images.githubusercontent.com/82711784/123506463-a21fd180-d664-11eb-93ca-42c400e4a434.png)


Decision maker profile

- 25 year old female
- married into a farm
- in-laws are still the owners of the farm --> no official employment of daughter in-law --> she counts only as a contributing family member in agricultural insurance
- husband will become owner in approximately 5 years --> she then becomes full member in agricultural insurance
- profession: gardener
- owns no noteworthy wealth or properties

Farm characteristics:

- dairy farm

Possiblity of own branch of farm:

- farm shop

