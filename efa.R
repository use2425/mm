# Load the psych package
install.packages("psych")
library(psych)

# Load the mtcars dataset
data(mtcars)

# Check data suitability for EFA
# Bartlett's test of sphericity
bartlett_test <- cortest.bartlett(mtcars)
print(bartlett_test)  # p-value should be < 0.05 for suitability

# KMO measure of sampling adequacy
kmo_test <- KMO(cor(mtcars))
print(kmo_test)  # Overall KMO > 0.6 is generally acceptable

# Determine the optimal number of factors using parallel analysis
fa_parallel <- fa.parallel(mtcars, fa = "fa", fm = "ml")
print(fa_parallel)  # Check the number of suggested factors

# Perform Exploratory Factor Analysis (EFA) with a specified number of factors
num_factors <- fa_parallel$nfact  # Use the suggested number of factors
efa_results <- fa(mtcars, nfactors = num_factors, rotate = "varimax", fm = "ml")

# Print the factor analysis results
print(efa_results)  # Loadings, uniquenesses, etc.

# Visualize the factor loadings
fa.diagram(efa_results)

