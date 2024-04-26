library(tidyverse)
library(ggplot2)
library(class)
library(caret) 
library(ggmosaic)
library(kableExtra)
library(lmtest)
library(car)

airlines=read.csv("C:/Users/Abhay/Downloads/ap_train.csv")
str(airlines)

df_airlines <- airlines %>% 
  select(-X,-id) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(satisfaction = 
           factor(satisfaction, 
                  levels = c("neutral or dissatisfied","satisfied"), 
                  labels = c(0, 1)),
         Customer.Type =
           factor(Customer.Type, 
                  levels = c("disloyal Customer","Loyal Customer"), 
                  labels = c(0, 1)))
glimpse(df_airlines)

colSums(is.na(df_airlines))
df_airlines$Arrival.Delay.in.Minutes <- 
  ifelse(is.na(df_airlines$Arrival.Delay.in.Minutes)
         , '0', df_airlines$Arrival.Delay.in.Minutes)
df_airlines$Arrival.Delay.in.Minutes <- 
  as.numeric(df_airlines$Arrival.Delay.in.Minutes)
colSums(is.na(df_airlines))

summary(df_airlines)
d_test=read.csv("C:/Users/Abhay/Downloads/ap_test.csv")

str(d_test)
df_test <- d_test %>% 
  select(-X,-id) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(satisfaction = 
           factor(satisfaction, 
                  levels = c("neutral or dissatisfied","satisfied"), 
                  labels = c(0, 1)),
         Customer.Type =
           factor(Customer.Type, 
                  levels = c("disloyal Customer","Loyal Customer"), 
                  labels = c(0, 1)))
glimpse(df_test)

colSums(is.na(df_test))

df_test$Arrival.Delay.in.Minutes <- 
  ifelse(is.na(df_test$Arrival.Delay.in.Minutes)
         , '0', df_test$Arrival.Delay.in.Minutes)
df_test$Arrival.Delay.in.Minutes <- 
  as.numeric(df_test$Arrival.Delay.in.Minutes)
colSums(is.na(df_test))

#data visualization
ggplot(gather(df_airlines %>% select_if(is.numeric)), aes(value)) + 
  geom_histogram(bins = 10, fill="blue") + 
  facet_wrap(~key, scales = 'free_x')
#Below frequency data visualization for each categorical variable


ggplot(gather(df_airlines %>% select_if(is.factor)), aes(value)) + 
  geom_bar(bins = 10,fill="firebrick") + 
  facet_wrap(~key, scales = 'free_x') + labs(x="Categorical",
                                             y="Value")
#Model Fitting
#Cross Validation

prop.table(table(df_airlines$satisfaction))

#Now we split data for train and test
RNGkind(sample.kind = "Rounding") 
set.seed(901)
airlines_train <- df_airlines
airlines_test <- df_test

#Now let us recheck the class imbalance between data train and test
prop.table(table(airlines_train$satisfaction)
prop.table(table(airlines_test$satisfaction))
#We can say that our target variable in both our data train and test are balance

#Model
model_reg1 <- glm(satisfaction ~ ., data = airlines_train, family = "binomial")
summary(model_reg1)

#let us check the multicolinearity of this model. If there is multicoleniarity in some variable, we
#can deselect also these variables

vif(model_reg1)

#Now let us check the linearity assumption from model_reg1
data.frame(prediction=model_reg1$fitted.values,
           error=model_reg1$residuals) %>% 
  ggplot(aes(prediction,error)) +
  geom_hline(yintercept=0) +
  geom_point() +
  geom_smooth() +
  theme_bw()

# let us deselect flight distance, Departure.Delay.in.Minutes and Arrival.Delay.in.Minutes variables since
#model_reg1 doesn’t pass the multicolinearity assumption.

df_airlines2 = airlines_train %>% 
  select(-Departure.Delay.in.Minutes,-Arrival.Delay.in.Minutes)
model_reg2 <- glm(satisfaction ~ ., data = df_airlines2, family = "binomial")
summary(model_reg2)

#Now let us check the multicolnearity with this model
vif(model_reg2)

#linearity check
data.frame(prediction=model_reg2$fitted.values,
           error=model_reg2$residuals) %>% 
  ggplot(aes(prediction,error)) +
  geom_hline(yintercept=0) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#summary between models
Model <- c("Model_Reg1", "Model_Reg2")
MultiColinearity <- c("yes","no" )
Linearity <-c("yes","yes" )
df <- data.frame(Model, AIC, MultiColinearity,Linearity )
print (df)

#Prediction
#Now let us predict the probability of satisfaction using our test data with our model_reg2 and saved in
#new column named pred_result
airlines_test$pred_result <- predict(object = model_reg2,
                                     newdata = airlines_test,
                                     type = "response")
#Now classify the data in airlines_test based on pred_result and saved in new column namen pred_label
airlines_test$pred_label <- ifelse(airlines_test$pred_result < 0.5 ,0, 1)
airlines_test$pred_label <- as.factor(airlines_test$pred_label)
head(airlines_test)

glimpse(airlines_test)

ggplot(data=airlines_test,mapping=aes(x=pred_result)) +
  geom_density(fill="green",col="black") +
  geom_vline(xintercept = 0.5 , linetype="dashed") +
  theme_bw()

#If we look at above plot, the result shows higher density <0.5, means higher customers number that aren’t
#satisfied with the airlines.

conf_log <- confusionMatrix(data = airlines_test$pred_label,reference = airlines_test$satisfaction, positive = "1")
conf_log
#If we look at the summary model evaluation, we can see that our model perform very well in all of the metrics from
#Accuracy,Recall, Specificity and Precision.






