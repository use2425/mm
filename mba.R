install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

df=read.csv("C:/Users/Tara/OneDrive/Desktop/groceries mba.csv")
df
str(df)
transactions <- read.transactions(file = "C:/Users/Tara/OneDrive/Desktop/groceries mba.csv",
                               format = "basket", sep = ",", rm.duplicates = TRUE)
# Explore the summary of your transacƟon data
summary(transactions)
# Plot the absolute item frequency plot 
itemFrequencyPlot(transactions,
                  type = "absolute", 
                  topN = 10, 
                  horiz = TRUE, 
                  main = 'Absolute item frequency')
# Mine association rules with specified parameters
rules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.5))
# Sort rules by confidence in descending order 
rules_lift_sorted <- sort(rules, by = "confidence", decreasing = TRUE) 
# Get the top 10 rules based on confidence 
top_10_rules <- head(rules_lift_sorted, n = 10)
# Print the top 10 rules using the inspect function
inspect(top_10_rules) 
'''Rules: All top 10 rules have a right-hand side (rhs) of whole milk, except for one rule that has other 
vegetables as the rhs. 
Support: The support values are relatively low (around 0.001), indicating that these combinaƟons 
are not very common in the dataset. 
Confidence: All rules have a confidence of 100%, indicating a strong associaƟon between the left hand side (lhs) and rhs of the rules. 
Lift: Lift values around 3.9 for rules with whole milk and 5.2 for the rule with other vegetables 
suggest that purchasing the lhs items significantly increases the likelihood of purchasing the rhs 
items. 
Count: The count of transactions that support each rule is low (ranging from 10 to 17), reflecƟng the 
relatively low support values.'''

# Plot the top 10 rules 
plot(top_10_rules, method = "graph")
