install.packages("lavaan")
library(lavaan)
data(HolzingerSwineford1939)
summary(HolzingerSwineford1939)

#checkthefactorability
KMO(select(HolzingerSwineford1939, x1:x9))
#specify the model
hs.mod <- 'visual =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6
           speed =~ x7 + x8 + x9'

#Estimate (fit) the model
hs.fit <- cfa(hs.mod, data = HolzingerSwineford1939)

#Model Fit and Step 4: Interpret
summary(hs.fit, fit.measures = TRUE)

'''The model fit statistics st chi-square test statistic is 71.682
with 18 degrees of freedom and a p-value of 0.000'''

#since p<0.05 we reject null hypo

# Visualize the results (optional)
inspect(hs.fit, "std.lv")

'''1.the loading of 0.882 for "visual" and x1 indicates that x1
contributes significantly to the visual latent variable
2.Loading of textual and speed is 0 
3.Uniqueness of x1 is 57.6%
4.covariance between "visual" and "textual" is 0.449'''

install.packages("semPlot")
library(semPlot)
# Plot the standardized factor loadings
semPaths(hs.fit, "std", layout = "tree2")

#A visualization of the path diagram with estimates as labels;