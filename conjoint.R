
pizza_data = read.csv('C:/Users/Tara/OneDrive/Desktop/dataset/pizza_data.csv')
pizza_data

pizza_data$brand = as.numeric(factor(pizza_data$brand))
pizza_data$price = as.numeric(factor(pizza_data$price))
pizza_data$weight = as.numeric(factor(pizza_data$weight))
pizza_data$crust = as.numeric(factor(pizza_data$crust))
pizza_data$cheese = as.numeric(factor(pizza_data$cheese))
pizza_data$size = as.numeric(factor(pizza_data$size))
pizza_data$toppings = as.numeric(factor(pizza_data$toppings))
pizza_data$spicy = as.numeric(factor(pizza_data$spicy))

pizza_data <- na.omit(pizza_data)
pizza_data

is.na(pizza_data)

str(pizza_data)
# Assuming 'pizza_data' is your data frame
pizza_data <- pizza_data[, -ncol(pizza_data)]
pizza_data

tprefm <- matrix(sample(0:16, 10*16, replace=TRUE), ncol=16)
colnames(tprefm) <- paste0("profil", 1:16)
tprefm <- as.data.frame(tprefm)
tprefm

str(tprefm)


col_names = colnames(pizza_data)
col_names




install.packages('conjoint')
library(conjoint)
install.packages("caUtilities")

library(caUtilities)

# Calculating utility value 
#caModel = caModel(y=tprefm[1,] , x = pizza_data)
#caModel

caUtilities(y=tprefm[1,], x=pizza_data, z=col_names)



conjoint_model = Conjoint(y=tprefm , x = pizza_data , z = col_names)
conjoint_model
#price or weight



