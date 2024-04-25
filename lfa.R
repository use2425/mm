#Data Preparation
library(tidyverse)
library(ggplot2)
library(class)
library(caret) 

library(ggmosaic)
library(kableExtra)
library(lmtest)
library(car)

#Data Import
df=read.csv("C:/Users/pravin/Documents/phtots/train.csv")
df=df[,9:21]

#Check Data Type
str(df)
colSums(is.na(df))

install.packages("psych")  # Install the psych package
library(psych)             # Load the psych package



m=cor(df)
r=fa.parallel(m,fa="fa")

'''INTERPRETATION
Parallel analysis suggests that the number of factors = 3''

e=fa(m,3,rotate = "promax")
e
fa.diagram(e$loadings)

#Factor score
out<-factanal(x=df,factors = 3,scores = "regression")
o=out$scores
o

#BINDING DATA
data=read.csv("F:/MSC_SEM2/Stat/Practicals/Practical1 FA/train.csv")
str(data)
nd=cbind(data,o)
str(nd)

df1=nd
str(nd)
# Assuming your DataFrame is named df
library(dplyr)
Newdf <- df1%>%
  select(Gender, Customer.Type,Age, Type.of.Travel, Class, Flight.Distance,
         Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes, satisfaction,
         Factor1, Factor2, Factor3)
# View the selected columns
str(Newdf)

#Now we have to convert the satisfaction ,Customer type values to 0 and 1 and remaing chr to factors
airlines=Newdf
airlines$Customer.Type <-factor(airlines$Customer.Type, levels = c("disloyal Customer","Loyal Customer"),
                                labels = c(0,1))
airlines$satisfaction <-factor(airlines$satisfaction, levels = c("neutral or dissatisfied","satisfied"), labels =
                                 c(0,1))
airlines$Type.of.Travel=as.factor(airlines$Type.of.Travel)
airlines$Class=as.factor(airlines$Class)
airlines$Gender=as.factor(airlines$Gender)
glimpse(airlines)
colSums(is.na(airlines))

#Check Missing Value
colSums(is.na(df_airlines))

#After we checked if there is any NA values, we found out that 310 observations are NA on the
#arrival_delay_in_minutes column. Here, we are going to assume that the NA value on are 0
airlines$Arrival.Delay.in.Minutes <- 
  ifelse(is.na(airlines$Arrival.Delay.in.Minutes)
         , '0', airlines$Arrival.Delay.in.Minutes)
airlines$Arrival.Delay.in.Minutes <- 
  as.numeric(airlines$Arrival.Delay.in.Minutes)
colSums(is.na(airlines))

#TEST_DATASET
d_test=read.csv("C:/Users/Abhay/Downloads/ap_test.csv")
d_test=d_test[,9:21]
str(d_test)
colSums(is.na(d_test))

m=cor(d_test)
r=fa.parallel(m,fa="fa")

e=fa(m,3,rotate = "promax")
fa.diagram(e$loadings)

#Factor score
outt<-factanal(x=d_test,factors = 3,scores = "regression")
oo=outt$scores
oo

#BINDING DATA
der=read.csv("C:/Users/Abhay/Downloads/ap_test.csv")
dd=cbind(der,oo)
str(dd)

#Now We Only Want the factor score and the remaining columns which i have not included while performing
#Factor analysis
dd1 <- dd%>%
  select(Gender,Customer.Type,Age, Type.of.Travel,Class, Flight.Distance,
         Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes, satisfaction,
         Factor1, Factor2, Factor3)
str(dd1)

#Now we have to convert the satisfaction ,Customer type values to 0 and 1 and remaing chr to factors
dd1$satisfaction <-factor(dd1$satisfaction, levels = c("neutral or dissatisfied","satisfied"), labels = c(0,1))
dd1$Customer.Type <-factor(dd1$Customer.Type, levels = c("disloyal Customer","Loyal Customer"), labels = c(0,1))
dd1$Class=as.factor(dd1$Class)
dd1$Gender=as.factor(dd1$Gender))
glimpse(dd1)

#Check Missing Value
colSums(is.na(dd1))

'''INTERPRETATION
After we checked if there is any NA values, we found out that 310 observations are NA on the
arrival_delay_in_minutes column. Here, we are going to assume that the NA value on are 0
'''



#After we checked if there is any NA values, we found out that 83 observations are NA on the
#arrival_delay_in_minutes column. Here, we are going to assume that the NA value on are 0
dd1$Arrival.Delay.in.Minutes <-
  ifelse(is.na(dd1$Arrival.Delay.in.Minutes)
         ,'0', dd1$Arrival.Delay.in.Minutes)
dd1$Arrival.Delay.in.Minutes <-
  as.numeric(dd1$Arrival.Delay.in.Minutes)
colSums(is.na(dd1))

#Model Fitting
model_reg1 <- glm(satisfaction ~ ., data = airlines, family = "binomial")
summary(model_reg1)

'''INTERPRETATION
We found that there are 2 variable that are not signicant (GenderMale,Flight Distance). Let us try deselect
this variable and check the AIC
However let us check the multicolinearity of this model. If there is multicoleniarity in some variable, we
can deselect also these variables'''







#We found that there are 2 variable that are not signicant (GenderMale,Flight Distance). Let us try deselect
#this variable and check the AIC
#However let us check the multicolinearity of this model. If there is multicoleniarity in some variable, we
#can deselect also these variables

vif(model_reg1)
#we found out that there are 2 variables Departure.Delay.in.Minutes and Arrival.Delay.in.Minutes are
#colinear (VIF > 10)

columns_to_keep <- c("Customer.Type", "Age", "Type.of.Travel", "Class", "Factor1", 
                     "Factor2", "Factor3","satisfaction")
df_subset <-airlines[, columns_to_keep]
str(df_subset)

#multicolnearity with this model
model_reg2 <- glm(satisfaction ~ ., data = df_subset,family = "binomial")
vif(model_reg2)

'''INTERPRETATION
NO MULTICOLLINEARITYY
Prediction
Now let us predict the probability of satisfaction using our test data with our model_reg2 and saved in
new column named pred_result'''



#Now let us predict the probability of satisfaction using our test data with our model_reg2 and saved in
#new column named pred_result
str(dd1)
cols_to_keep <- c("Customer.Type","Age","Type.of.Travel","Class","Factor1",
                  "Factor2","Factor3","satisfaction")
df_sub <-dd1[, columns_to_keep]
str(df_sub)

dd1$pred_result <- predict(object = model_reg2,
                           newdata = dd1,
                           type = "response")
dd1$pred_label <- ifelse(dd1$pred_result < 0.5 ,0, 1)
dd1$pred_label <- as.factor(dd1$pred_label)
head(dd1)

#Model Evaluation

conf_log <- confusionMatrix(data = dd1$pred_label,reference = dd1$satisfaction, positive = "1")
conf_log

'''INTERPRETATION
If we look at the summary model evaluation, we can see that our model perform very well in all of the metrics from
Accuracy,Recall, Specificity and Precision'''
