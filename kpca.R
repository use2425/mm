# Load required library
library(kernlab)

# Load the iris dataset
data(iris)

# Separate features and labels
X <- iris[, -5] # Features
y <- iris[, 5] # Labels

# Perform Kernel PCA
kpca_model <- kpca(~., data = X, kernel = "rbfdot", kpar = list(sigma = 0.1))

# Get the transformed data
X_transformed <- as.data.frame(predict(kpca_model, X))

# Print the transformed data
print(head(X_transformed))

''': the output provides a glimpse of the transformed data with reduced dimensionality, which may be
useful for further analysis or visualization'''

# Plot the transformed data
plot(X_transformed[,1], X_transformed[,2], col = y, pch = 19, xlab = "Principal Component 1", ylab = "Principal Component 2")

# Add legend
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19, title = "Species")

# Add title
title("Kernel PCA on Iris Dataset")

# Highlight components with high variance
points(X_transformed[,1][which.max(var(X_transformed))], X_transformed[,2][which.max(var(X_transformed))], col = "red", pch = 20, cex = 2)
points(X_transformed[,1][which.min(var(X_transformed))], X_transformed[,2][which.min(var(X_transformed))], col = "blue", pch = 20, cex = 2)

'''two points on a scatter plot, with one point representing the
maximum variance and colored red, and the other point representing the minimum variance and colored
blue'''
