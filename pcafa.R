install.packages("corrr")
library("corr")
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library(factoextra) 
df=read.csv("C:/Users/Tara/OneDrive/Desktop/fa.csv")
df
#Check for null values
colSums(is.na(df))
#Normalizing the data
data_normalized =scale(df)
head(data_normalized)
#Compute the Correlation matrix :
corr_matrix <- cor(data_normalized)
corr_matrix
ggcorrplot(corr_matrix)
'''The higher the value, the most positively correlated the two variables are.
like Potential & Academic Record ,Job Fit & Company Fit ,Resume & Letter etc
• The closer the value to -1, the most negatively correlated they are.
like Experience & Communication,Organization & Experience,Self Confidence & Letter etc'''
#Applying PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
'''The cumulative proportion of Comp.1 ,Comp.2,Comp.3 explains nearly 85% of the total variance. 
This means that the first three principal components can accurately represent the data'''.

#Loading matrix :
data.pca$loadings[, 1:3]

#Visualization of the principal component :
fviz_eig(data.pca, addlabels = TRUE)
#The first three components can be considered to be the most significant.

#To check similarities and disimilarities
#Biplot of the attributes :
fviz_pca_var(data.pca, col.var = "black")

#Potential, Academic Record ,Experience ,Job fit have a positive correlation to each. 
#Company fit,Self-confidence,Apperance,Likeability,organization 
#are grouped together because they have the highest values

#Contribution of each variable :
fviz_cos2(data.pca, choice = "var", axes = 1:3)

#Biplot Combined With cos2 :
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


'''High cos2 attributes are colored in Blue: Experience,Letter,Organization
• Mid cos2 attributes have an orange color: Potential,Academic record,Communication,Resume
• Finally, low cos2 attributes have a black color: Job Fit,Compan fit'''
