library(corrplot)
library(ggplot2)
library(psych)
library(DescTools)
library(factoextra)
library(paran)

#Import the training CSV
ap_train<- read.csv("D:/kaushik/MSC/MVAsem2/Logi data/ap_train.csv")

#Convert non-numerical col to numerical col
ap_train_encode <- c("Gender", "Customer.Type", "Type.of.Travel", "Class", "satisfaction")
for (col in ap_train_encode) {
  ap_train[[col]] <- as.numeric(factor(ap_train[[col]]))
}
head(ap_train,2)

# Convert values to 0 and 1
ap_train$satisfaction <- ifelse(ap_train$satisfaction == 2, 0, 1)
head(ap_train,3)

#Check for null value
ap_train_nn <- na.omit(ap_train)
null_count_ap <- colSums(is.na(ap_train_nn))
print(null_count_ap)

# Exclude columns "ID", "X", and "satisfaction"
cols_to_plot <- colnames(ap_train_nn)[!(colnames(ap_train_nn) %in% c("id", "X", "satisfaction"))]
variables <- c("v1", "v2", "v3", "v4")

# Set up the plotting layout
par(mfrow = c(1, length(variables)))

# Create a boxplot for each column
for (col in cols_to_plot) {
  boxplot(ap_train_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Specify the columns to Winsorize
cols_to_winsorize <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")

# Apply Winsorization to the specified columns
for (col in cols_to_winsorize) {
  ap_train_nn[[col]] <- Winsorize(ap_train_nn[[col]], probs = c(0.10, 0.90))  # Trim 5% from both tails
}

variables1 <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
# Set up the plotting layout
par(mfrow = c(1, length(variables1)))

# Create a boxplot for each column
for (col in variables1) {
  boxplot(ap_train_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Convert the correlation matrix to a data frame
cap <- cor(ap_train_nn)
cor_df <- as.data.frame(as.table(cap))

# Create the ggplot2 correlation plot
names(cor_df) <- c("Variable1", "Variable2", "Correlation")
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black") +  # Add text labels
  scale_fill_gradient2(low = "blue", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust=1, size = 8, face = "italic"),
        axis.text.y = element_text(size = 8, face = "italic"))


# Reset the plotting layout
par(mfrow = c(1, 1))

# Filter the correlation matrix for variables with correlation greater than 0.2 with "satisfaction"
satisfaction_correlation <- cap[abs(cap["satisfaction", ]) > 0.2, ]

# Extract variable names
relevant_variables <- rownames(satisfaction_correlation)

# Subset the original dataframe based on relevant variables
relevant_data <- ap_train_nn[, relevant_variables]

# Print the first few rows of the relevant data
head(relevant_data)

print(colnames(relevant_data))

#Factor Analysis
#Create a new Dataframe with all relevant Columns
Fa_cols <- c("Inflight.wifi.service", "Food.and.drink", "Online.boarding",
             "Seat.comfort", "Inflight.entertainment", "On.board.service",
             "Leg.room.service", "Baggage.handling", "Checkin.service",
             "Inflight.service", "Cleanliness")

# Convert the correlation matrix to a data frame
fa_df <- relevant_data[Fa_cols]
capdr <- cor(fa_df)
cor_dfr <- as.data.frame(as.table(capdr))
print(capdr)

# Parallel analysis (using the paran package)
optimal_num_factors_parallel <- fa.parallel(capdr)$nfact

# Factor analysis
model <- fa(capdr , 3, rotate = "promax")
loadings <- model$loadings
fa.diagram(loadings, factors = colnames(loadings), node.size = 2, mar = c(0.1, 0.1, 0.1, 0.1),
           node.color = "lightblue", arrow.color = "darkblue")


# Getting factor scores
factor_names <- colnames(model$loadings)
print(factor_names)
out<-fa(fa_df,3,scores = "regression")
factor_scores=out$scores

# Add factors and thier scores to relevant_data dataframe
relevant_data <- cbind(relevant_data, factor_scores)
head(relevant_data,2)

# Build logistic regression model
logit_model <- glm(satisfaction ~ MR1 + MR2 + MR3 + Type.of.Travel + Class + Flight.Distance, data = relevant_data, family = "binomial")

# Summarize the model
summary(logit_model)


##################################################################################################################3
ap_test<- read.csv("D:/kaushik/MSC/MVAsem2/Logi data/ap_test.csv")

#Testing the model
head(ap_test)
ap_test_encode <- c("Gender", "Customer.Type", "Type.of.Travel", "Class", "satisfaction")
for (col in ap_train_encode) {
  ap_test[[col]] <- as.numeric(factor(ap_test[[col]]))
}
# Convert values to 0 and 1
ap_test$satisfaction <- ifelse(ap_test$satisfaction == 2, 0, 1)
head(ap_test)

#Check for null value
ap_test_nn <- na.omit(ap_test)
null_count_ap <- colSums(is.na(ap_test_nn))
print(null_count_ap)

# Exclude columns "ID", "X", and "satisfaction"
cols_to_plot <- colnames(ap_test_nn)[!(colnames(ap_test_nn) %in% c("id", "X", "satisfaction"))]
variables <- c("v1", "v2", "v3", "v4")

# Set up the plotting layout
par(mfrow = c(1, length(variables)))

# Create a boxplot for each column
for (col in cols_to_plot) {
  boxplot(ap_test_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Specify the columns to Winsorize
cols_to_winsorize <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")

# Apply Winsorization to the specified columns
for (col in cols_to_winsorize) {
  ap_test_nn[[col]] <- Winsorize(ap_test_nn[[col]], probs = c(0.10, 0.90))  # Trim 5% from both tails
}

variables1 <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
# Set up the plotting layout
par(mfrow = c(1, length(variables1)))

# Create a boxplot for each column
for (col in variables1) {
  boxplot(ap_test_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

#Factor Analysis
Fa_colst <- c("Inflight.wifi.service", "Food.and.drink", "Online.boarding",
              "Seat.comfort", "Inflight.entertainment", "On.board.service",
              "Leg.room.service", "Baggage.handling", "Checkin.service",
              "Inflight.service", "Cleanliness")

# Convert the correlation matrix to a data frame
fa_dft <- ap_test_nn[Fa_colst]
capdr <- cor(fa_dft)
cor_dfr <- as.data.frame(as.table(capdr))
print(capdr)

# Parallel analysis (using the paran package)
library(paran)
optimal_num_factors_parallel <- fa.parallel(capdr)$nfact

# Factor analysis
modelt <- fa(capdr , 3, rotate = "promax")
loadings <- modelt$loadings
fa.diagram(loadings, factors = colnames(loadings), node.size = 2, mar = c(0.1, 0.1, 0.1, 0.1),
           node.color = "lightblue", arrow.color = "darkblue")


# Assuming 'model' is your factor analysis model
factor_namest <- colnames(modelt$loadings)
print(factor_namest)
out<-fa(fa_dft,3,scores = "regression")
factor_scores=out$scores

# Add factors to relevant_data dataframe
ap_test_nn <- cbind(ap_test_nn, factor_scores)
head(ap_test_nn)

# Predict on test data
predicted <- predict(logit_model, newdata = ap_test_nn, type = "response")

# calculate accuracy
predicted_class <- ifelse(predicted > 0.5, 1, 0)  # Convert probabilities to classes
accuracy <- mean(predicted_class == ap_test_nn$satisfaction)
print(paste("Accuracy:", accuracy))

# Interpret coefficients
summary(logit_model)
