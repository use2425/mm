library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(ggcorrplot)
library(cfa)


data(PoliticalDemocracy)
head(PoliticalDemocracy)

model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

fit <- sem(model, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)

semPaths(fit, what = 'std', layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)
semPaths(fit, whatLabels = "est", style="lisrel",main="SEM Diagram")

?HolzingerSwineford1939
data(HolzingerSwineford1939)
head(HolzingerSwineford1939)

model <-'# measurement model
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5+ x6
  speed =~ x7 + x8 + x9
   # regressions
    visual ~ textual
    textual ~ speed
    visual ~ speed
  # residual correlations
    x1 ~~ x2 + x3
    x4 ~~ x6
    x5 ~~ x6
    x7 ~~ x8
    x8 ~~ x9
'
fit <- sem(model, data=HolzingerSwineford1939)
summary(fit, standardized=TRUE)

semPaths(fit, whatLabels = "est", style="lisrel",main="SEM Diagram")

################################################################################
#Confirmatory Factor Anaylsis

library(lavaan)
library(semPlot)
library(tidyverse)
library(kableExtra)
library(GGally)
library(cfa)

HSmodel <-'# measurement model
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5+ x6
  speed =~ x7 + x8 + x9
  
  x1 ~~ x4
  x2 ~~ x5
  x3 ~~ x6
  x4 ~~ x7
  x5 ~~ x8
  x6 ~~ x9'

HSfit <- cfa(HSmodel, data=HolzingerSwineford1939)
summary(HSfit, standardized=TRUE)

#Visualize the result (optional)
inspect(fit, 'std.lv')

# Plot the standardized factor loadings
semPaths(fit, "std", layout = "tree2")
semPaths(HSfit, whatLabels = "est", style="lisrel",main="cfa diagram")
