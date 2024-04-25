df=read.csv("C:/Users/Tara/OneDrive/Desktop/fac.csv")
df
#Check for null values and remove it 
df<- na.omit(df) 
null_count <- colSums(is.na(df)) 
print(null_count) 

# Compute the Correlation matrix
# Select columns 9 through 22 from the data frame
df <- df[, 9:22]

# Compute the correlation matrix
e <- cor(df)
cor_matrix <- cor(df, method = "pearson")

# Print the Correlation matrix
print(cor_matrix)
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "circle")
'''The higher the value, the most positively correlated the two variables are. 
like Ease.of.online.booking& Inflight.wifi.service , inflight.entertainment&
Food.and.drink etc
• The closer the value to -1, the most negatively correlated they are.
like Seat.comfort& Inflight.wifi.service ,Departure.Arrival.time.convenient 
& Food.and.drink etc'''

install.packages("psych")
library(psych)
#Determine Number of Factorsto Extract
# Perform parallel analysis
fa.parallel(e, fa = "fa")
#so 3 factors

# Install and load the semTools package
install.packages("semTools")
library(semTools)
c=cor_matrix
install.packages("GPArotation")
library(GPArotation)
# Assuming 'e' is your factor analysis result
e <- fa(c,5, rotate = "promax")  # Example factor analysis

# Plot the factor diagram
fa.diagram(e$loadings)

#Extract (and rotate) factors:
e=fa(c,5,rotate = "promax")
e
###Test of the hypothesis states that 5 Factors are sufficient 
###The root mean square of the residuals is 0.03
