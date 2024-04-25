data("USArrests")
install.packages("MVN")
library(MVN)
outlier=mvn(USArrests,multivariateOutlierMethod = 
              "quan",showNewData = TRUE)
#Based on the plot above, there are 8 outliers in the data. But I’ll just discard the 
'''Alaska data because it’s so far away from the other observations.'''

df = USArrests[-2,]
df =scale(USArrests)
library(factoextra)
fviz_nbclust(df, FUN=hcut, method = "silhouette“

#From the plot above, the optimum value is 2. Then the data will be 
grouped into 2 clusters

# Perform hierarchical clustering with average linkage method
metode_al <- hclust(dist(df), method = "average")
hc_ave <- cophenetic(metode_al)
cor.ave <- cor(as.dist(dist(df)), hc_ave)

#single link

metode_sl <- hclust(dist(df), method = "single")
hc_sl<- cophenetic(metode_sl)
cor.sl <- cor(as.dist(dist(df)), hc_sl)

#complete

metode_cl <- hclust(dist(df), method = "complete")
hc_cl<- cophenetic(metode_cl)
cor.cl <- cor(as.dist(dist(df)), hc_cl)

#ward method
metode_w <- hclust(dist(df), method = "ward")
hc_w<- cophenetic(metode_w)
cor.w <- cor(as.dist(dist(df)), hc_w)

#centroid
metode_cd <- hclust(dist(df), method = "centroid")
hc_cd<- cophenetic(metode_cd)
cor.cd <- cor(as.dist(dist(df)), hc_cd)

cbind.data.frame(cor.ave,cor.cl,cor.sl,cor.w,cor.cd)

''Based on the results of the cophenetic correlation coefficient of the five 
methods, it is known that the average linkage method has the largest 
value, which is 0.7154281. Then the clustering will use the average 
linkage method'''

plot(metode_al)
rect.hclust(metode_al,2)

'''Based on the dendogram and figure above, cluster 1 consists of 31
states and cluster 2 consists of 18 states'''

km <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(km, data = df)

group2=cutree(metode_al,2)
table(group2)
table(group2/nrow(df))
aggregate(df,list(group2),mean)

'''cluster1:
1.Characterized by higher mean values of Murder, Assault, UrbanPop, and 
Rape compared to Cluster 2.
2. The mean Murder rate, Assault rate, UrbanPop (percentage of the 
population living in urban areas), and Rape rate are all higher compared 
to Cluster 2'''

'''cluster2:
1.This cluster represents states with lower levels of crime across all 
variables.
2.Based on the provided mean values, Cluster 2 may be characterized as 
states with lower crime rates and a lower percentage of urban 
population'''
