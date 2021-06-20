#install.packages("decisionSupport")
library(decisionSupport)
#install.packages("DiagrammeR")
library (DiagrammeR)


mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1.5px")
