
install.packages("fda.usc")
install.packages("fda")
library(fda.usc)
library(fda)
# Define time points
time_points <- seq(1, nrow(iris), length.out = nrow(iris))

# Create a basis object
basis <- create.bspline.basis(rangeval = range(time_points), nbasis = length(time_points))

# Combine the matrices in the list into a single matrix
iris_matrix <- do.call(cbind, iris_functions)

# Construct the functional data object
iris_fd <- Data2fd(argvals = time_points, y = iris_matrix, basisobj = basis)

# Perform Functional PCA
fpca_result <- pca.fd(iris_fd, nharm = 2)

# Plot the Functional PCA results
plot(fpca_result)
