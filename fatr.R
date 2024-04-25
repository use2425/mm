install.packages("psych",dependencies = T) 
library(psych)



mydata=read.csv("C:/Users/Tara/OneDrive/Desktop/fac.csv")

summary(mydata)
str(mydata)

install.packages("psych",dependencies = T) 
library(psych)

install.packages("corr")
library(corrr)

install.packages("corrplot")
library(corrplot)

install.packages("GPArotation") 
library(GPArotation)

install.packages("visdat")
library(visdat)

install.packages("ggplot")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)
install.packages("tidyverse")
library(tidyverse)
install.packages("summarytools")
library(summarytools)


#EDA

vis_dat(mydata, warn_large_data=FALSE, large_data_size=130000)


colSums(is.na(mydata))

mydata=na.omit(mydata)
# Convert categorical variables to factors
mydata$Gender <- as.factor(mydata$Gender)
mydata$Customer.Type <- as.factor(mydata$Customer.Type)
mydata$Type.of.Travel <- as.factor(mydata$Type.of.Travel)
mydata$Class <- as.factor(mydata$Class)
mydata$satisfaction <- as.factor(mydata$satisfaction)

# Perform label encoding
mydata$Gender <- as.numeric(mydata$Gender) - 1  # Convert to 0 for Male, 1 for Female
mydata$Customer.Type <- as.numeric(mydata$Customer.Type) - 1  # Convert to 0 for Loyal Customer, 1 for disloyal Customer
mydata$Type.of.Travel <- as.numeric(mydata$Type.of.Travel) - 1  # Convert to 0 for Personal Travel, 1 for Business travel
mydata$Class <- as.numeric(mydata$Class) - 1  # Convert to 0 for Eco Plus, 1 for Business
mydata$satisfaction <- as.numeric(mydata$satisfaction) - 1  # Convert to 0 for neutral or dissatisfied, 1 for satisfied

# Now all categorical variables are converted to numeric
# Now you can proceed with calculating the correlation matrix

c <- cor(mydata)
print(c)

corrplot(cor(mydata),method = 'circle')
#kmo test
#KMO value above 0.6 is considered acceptable, while values above 0.7 or 0.8 are preferable.

library(psych)
KMO(mydata)

#if less then 5 then use below 3 lines
#mydat <- mydata[, KMO(mydata)$MSAi>0.50] # Get rid of all variables with MSA < 0.50
#mydata <- mydat
#round( KMO(mydata)$MSA, 2 )


#Bartlett’s test for sphericity
#In this test, a significant p-value (usually below 0.05) indicates that the variables 
#are suitable for structure detection and thus appropriate for factor analysis.

library(psych)
cortest.bartlett(mydata)

#Determine Number of Factors to Extract

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values

scree(c, pc=FALSE)

fa.parallel(c, fa="fa")

#Extract (and rotate) factors
# You are essentially searching for a clearer association between individual factors and the various variables

Nfacs <- 5  # This is for four factors. You can change this as needed.

fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit)

load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)


loads <- fit$loadings
fa.diagram(loads)






