install.packages("corrr")
library("corr")
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library(factoextra) 
df=read.csv("C:/Users/Tara/OneDrive/Desktop/apple_quality.csv")
df
#Check for null values
df=na.omit(df)
colSums(is.na(df))
head(df)
str(df)
df$Acidity=as.numeric(df$Acidity)
df=df[,2:8]

#Normalizing the data
data_normalized =scale(df)
head(data_normalized)
#Compute the Correlation matrix :
corr_matrix <- cor(data_normalized)
corr_matrix
ggcorrplot(corr_matrix)
'''The higher the value, the most positively correlated the two variables are.
like Acidity&size,Acidity&Juiciness,Crunchiness&Size etc
• The closer the value to -1, the most negatively correlated they are.
like Juiciness &Size, Crunchiness &Sweetness etc'''

#Applying PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)

'''The cumulative proportion of Comp.1 ,Comp.2,Comp.3 , Comp.4 explains nearly 89% of the total variance.
This means that the first Four principal components can accurately represent the data.'''.

#Loading matrix :
data.pca$loadings[, 1:4]
''' The Loading Matrix Shows that the first principal component has high positive values for 
Size & Crunchiness  and so on'''


#Visualization of the principal component :
fviz_eig(data.pca, addlabels = TRUE)
#The first three components can be considered to be the most significant.

#To check similarities and disimilarities
#Biplot of the attributes :
fviz_pca_var(data.pca, col.var = "black")

'''Crunchiness,Size have a positive correlation to each other.
Acidity, wetness,juiciness have higher magnitude compared to Weight, and hence are 
well represented compared to Weight.
 negatively correlated is Ripeness'''


#Contribution of each variable :
fviz_cos2(data.pca, choice = "var", axes = 1:3)

#Biplot Combined With cos2 :
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


'''High cos2 attributes are colored in Blue: Ripeness.Mid cos2 attributes have an orange color:Acidity,Crunchiness,Juiciness
.Finally, low cos2 attributes have a black color: Weight'''
