#Exploring the data :
mobile_train_df=read.csv("C:/Users/pravin/Documents/phtots/mobileprice_train.csv")
mobile_test_df=read.csv("C:/Users/pravin/Documents/phtots/mobileprice_test.csv")
str(mobile_test_df)
str(mobile_train_df)
#Since our test dataset does not have our target variable (price range) so we will split the 
#data into train and test set on our training set instead:

index <- sample(nrow(mobile_train_df),nrow(mobile_train_df)*0.80)
mobile_train = mobile_train_df[index,]
mobile_test = mobile_train_df[-index,]
#Let’s fit a multinomial logistic regression model since we are dealing with categorical target variable: 
library(nnet)

mobile_fit<- multinom(price_range~., data=mobile_train_df)
summary(mobile_fit)

'''Let’s conduct variable selection to find the best variables for our model. As you can see 
here, the variable selection attempts to find the best model that has the smallest AIC (it 
provides a mean for model selection), so let’s use these variables to build our best model:'''



mobile_back <- step(mobile_fit, direction = "both")
summary(mobile_back)

mobile_best = multinom(formula = price_range ~ battery_power + dual_sim + 
                         int_memory + mobile_wt + n_cores + pc + px_height + px_width + ram + sc_h + wifi, 
                       data = mobile_test)

#in-sample prediction with the best model
pred_resp <- predict(mobile_best, newdata= mobile_train, type="class")
table(mobile_train$price_range, pred_resp)

tab = table(mobile_train$price_range, pred_resp) 
round((sum(diag(tab))/sum(tab))*100,2)

'''Now let’s perform out-of-sample prediction. 
As you can see here, the model also does a good job at classifying on the test set'''


# Predicting the class for test dataset
pred_test <- predict(mobile_best, newdata = mobile_test, "class")
# Building classification table 
tab1 <- table(mobile_test$price_range, pred_test) 
tab1
round((sum(diag(tab1))/sum(tab1))*100,2)

