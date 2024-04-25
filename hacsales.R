install.packages("factoextra")
install.packages("cluster")
install.packages("NbClust")
install.packages("ggplot2")
library(ggplot2)
library(factoextra) 
library(cluster) 
library(NbClust) 
df=read.csv("C:/Users/Tara/Downloads/Sales_Product_Details.csv")
df
#Convert non-numerical col to numerical col 
df$Product_Description <- as.numeric(factor(df$Product_Description))
df$Product_Line <- as.numeric(factor(df$Product_Line)) 
df$Product_Category <- as.numeric(factor(df$Product_Category)) 
df$Raw_Material <- as.numeric(factor(df$Raw_Material)) 
df$Region <- as.numeric(factor(df$Region)) 

#Check for null values and remove it 
df1 <- na.omit(df) 
null_count <- colSums(is.na(df1)) 
print(null_count) 
# Standardize the data 
df1 <- scale(df1) 
head(df1,2) 
# Visualize opƟmal number of clusters using WCSS
fviz_nbclust(df1, kmeans, method = "wss") 
# Compute the distance matrix using Euclidean distance 

res.dist <- dist(df, method = "euclidean")

# Perform hierarchical clustering
res.hc <- hclust(res.dist)

# Print the hierarchical clustering result
print(res.hc)
#Number of objects: There are 30 objects (data points) in total. 
# Visualize the dendrogram with cluster membership 
fviz_dend(res.hc, cex = 0.5, k = 3, color_labels = TRUE) 

##As we can see there are 3 clusters forming (red, green , blue). 
# Compute copheneƟc distance
res.coph <- cophenetic(res.hc)
# CorrelaƟon between copheneƟc distance and the original distance
correlation <- cor(res.dist, res.coph) 
print(correlation)
# Perform hierarchical clustering with a different method (opƟonal)
res.hc2 <- hclust(res.dist, method = "average") 
correlation2 <- cor(res.dist, cophenetic(res.hc2))
print(correlation2)

# Visualize the dendrogram with cluster membership by average method 
fviz_dend(res.hc2, cex = 0.5, k = 3, color_labels = TRUE)
# Determine the cluster membership 
grp <- cutree(res.hc, k = 3) 
print(grp) 
# Display the distribuƟon of clusters
table_grp <- table(grp) 
print(table_grp)
'''Data points 1, 2, 4, 6, 19, 26, and 28 belong to cluster 1. 
 Data points 3, 5, 7, 9, 10, 11, 13, 15, 18, 21, 23, 25, 27, and 29 belong to cluster 2. 
 Data points 8, 12, 14, 16, 17, 20, 22, 24, and 30 belong to cluster''' 
