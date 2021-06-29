# Gendergroup
https://rdrr.io/cran/decisionSupport/man/vv.html
In our model we compare multiple branches. First there are differnt options to finance pension schemes and also different pension schemes the women can choose from. Up until now we have not managed to implement all branches (the "put together "part after the calculation of benefits and costs for each option), but we are in the process of completing the missing branches. We have discount rates for differnt years that apply for retirenment and working years: We have multiple risks and  chances for events to happpen.  We have variability in all branches ( inflation, worth of money, costs during her lifetime; paying in different years than receiving money).

how to use the code?
-line 53 shows which way to use. it is neat this way. type in the way you want to see!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ich bin rex 
what i changed till my last explanation, that had to be simplified (i cannot explain the whole code here, just what had to go):
-state insurance only as a necessity for off-farm job to make coding easier for me
-risk of no child care serveces raus, weil das risk nicht für alles gilt und ich keine ahnung habe wie ich es nur für einige optionen nennen kann. evtl. geht das gar nicht.
-self employment was a false line in the input table. it doubeled with off-farm job. is out now.

To inform you guys baout what i did so far:

-late transfer of risk out? i am tired now and cannot remember if it was important for all job options-i think not (see risk of child care prob) 
- to say that with each option, she would pay different amonts of money into etf and private insurance and state insurance is too hard. that would be 25 more ways to code. state insurance therefore is only a necessity for off-farm job now and for private insurance and etf maybe julia wants to do it, but the way i found to do ths (example way 3) is a bit noo like and takes a lot of time (50 euro more than in given range f.e. in every nwe way....would be 12 new ways without private)
- put a discount rate for the job (when she has enough money to pay more for ETF and private insurance and Mix of both) with two branches (one before 10 years and one after 10 years) is the same: waaaaaaaaaay more branches as far as i got it. 


In the future:
-input actual values/ranges
- model analysis tools 
- visuals ( mcmc_areas() function from the bayesplot library)
- plot cashflow to see tyüical pofile of pension insurance : negative flow for several decades, later on positive flow
- project report markdown, implement model in mermaid
- implement agroforestry as pension option: buy land and plant trees and take care for them for a wood / fruit /nut pension? Is it your idea, julia? it was not in lour workshop, i guess we could do that as one way but it would be very simplistic. Do you have values?

Questions for Cory:
-how to do way 6 best, since it is an extra option for all branches?
-fließen risks und chance_events überhaupt mit ein? Müssen die beim net present value nochmal mit berechnet werden?
-is this code ok for them or did i miss s.th.?


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

