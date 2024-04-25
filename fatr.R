df=read.csv("C:/Users/Tara/OneDrive/Desktop/fa.csv")
df
#Check for null values and remove it 
df<- na.omit(df) 
null_count <- colSums(is.na(df)) 
print(null_count) 


# Compute the correlation matrix
e <- cor(df)
e

'''Variables like "Academic record", "Experience", "Potential", and "Job Fit" exhibit strong positive correlations
with each other
Variables like "Communication", "Company Fit", "Likeability", and "Organization" show moderate positive 
correlations with several other variables'''

install.packages("corrplot")
library(corrplot)

# Clear the plotting device
plot.new()
dev.off()


corrplot(cor(df), method = "circle")
'''The higher the value, the most positively correlated the two variables are.
like Potential & Academic Record ,Job Fit & Company Fit ,Resume & Letter etc
• The closer the value to -1, the most negatively correlated they are.
like Experience & Communication,Organization & Experience,Self Confidence & Letter etc'''

install.packages("psych")
library(psych)
#Determine Number of Factorsto Extract
# Perform parallel analysis
fa.parallel(e, fa = "fa")
#so 5 factors

# Install and load the semTools package
install.packages("semTools")
library(semTools)
c=cor_matrix
install.packages("GPArotation")
library(GPArotation)
# Assuming 'e' is your factor analysis result
e <- fa(c,5, rotate = "promax")  # Example factor analysis
e
# Plot the factor diagram
fa.diagram(e$loadings)
#The factors are grouped into four categories: MR1, MR2, MR3, MR4 and MR5

'''the factor analysis suggests that job satisfaction is influenced by a 
combination of factors including how well someone fits the job and the company, their 
communication and organization skills, the quality of their resume and cover letter, and 
their personal attributes'''

#Extract (and rotate) factors:


e=fa(c,5,rotate = "promax")
e
###Test of the hypothesis states that 5 Factors are sufficient 
###The root mean square of the residuals is 0.03
