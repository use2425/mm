library(factoextra)
library(cluster)
library(NbClust)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(maps)
library(dplyr)

# Read the CSV file (replace 'your_file.csv' with the actual file path)
df <- read.csv("D:/kaushik/MSC/MVAsem2/Sales_Product_Details.csv")
head(df,2)

#Convert non-numerical col to numerical col
df$Product_Description <- as.numeric(factor(df$Product_Description))
df$Product_Line <- as.numeric(factor(df$Product_Line))
df$Product_Category <- as.numeric(factor(df$Product_Category))
df$Raw_Material <- as.numeric(factor(df$Raw_Material))
df$Region <- as.numeric(factor(df$Region))
head(df,2)

#Check for null values and remove it
df1 <- na.omit(df)
null_count <- colSums(is.na(df1))
print(null_count)
head(df1)

# Standardize the data
df1 <- scale(df1)
head(df1,2)

# Visualize optimal number of clusters using WCSS
fviz_nbclust(df1, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df1,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 3 clusters
km1 <- kmeans(df1, centers = 3, nstart = 25)
km1

#plot results of final k-means model
fviz_cluster(km1, data = df1)

#find means of each cluster
cluster_means <- aggregate(df, by=list(cluster=km1$cluster), mean)
cluster_means

# Combine cluster assignments with original data
final_data <- cbind(df, cluster = km1$cluster)

# Function to create bar graphs for Product_ID against Quantity for all clusters together
plot_product_id_vs_quantity_all_clusters <- function(cluster_data) {
  ggplot(cluster_data, aes(x = as.factor(Product_ID), y = Quantity)) +
    geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
    labs(title = "Product ID vs. Quantity",
         x = "Product ID", y = "Average Quantity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# Create bar graph for all clusters together
plot_list <- list()
for (i in unique(final_data$cluster)) {
  cluster_subset <- final_data[final_data$cluster == i, ]
  plot_list[[i]] <- plot_product_id_vs_quantity_all_clusters(cluster_subset) +
    ggtitle(paste("Cluster", i))
}

# Combine plots into a single plot with subplots
all_plots <- do.call(grid.arrange, c(plot_list, ncol = 3))

# Display the combined plot
print(all_plots)

# Create pie charts for each cluster
plots <- lapply(unique(final_data$cluster), function(cluster_num) {
  cluster_subset <- final_data[final_data$cluster == cluster_num, ]
  region_counts <- table(cluster_subset$Region)
  pie_data <- data.frame(region = names(region_counts), count = as.numeric(region_counts))
  ggplot(pie_data, aes(x = "", y = count, fill = region)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    labs(title = paste("Region Distribution for Cluster", cluster_num)) +
    theme_void() +
    theme(legend.position = "right")
})

# Combine plots into a single plot with subplots
combined_plot <- wrap_plots(plots, nrow = 1)

# Display the combined plot
print(combined_plot)

# Add cluster information to the dataset
df$Cluster <- as.factor(km1$cluster)

# Create scatter plot for latitude and longitude
# Plot map focusing on specified limits
world <- map_data("world")
plot_map <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray90", color = "black") +
  geom_point(data = df, aes(x = Longitude, y = Latitude, color = Cluster), size = 3) +
  labs(title = "Latitude and Longitude Scatter Plot") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green")) +  # Define colors for clusters
  coord_cartesian(xlim = c(-10, 10), ylim = c(48, 55)) +  # Adjust the limits as specified
  geom_text(data = df, aes(label = Cluster  , x = Longitude, y = Latitude), size = 3, vjust = -0.5, position = position_jitter(width = 0.1, height = 0.1))  # Add jittering to text labels

# Display the plot
print(plot_map)

# Group points with the same coordinates and cluster, and print corresponding dates
grouped_df <- df %>%
  group_by(Latitude, Longitude, Cluster) %>%
  summarise(Dates = paste(Date, collapse = ", "))

print(grouped_df)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compute the distance matrix using Euclidean distance
res.dist <- dist(df)

# Perform hierarchical clustering
res.hc <- hclust(d = res.dist, method = "ward.D2")

# Print the hierarchical clustering result
print(res.hc)

# Visualize the dendrogram with cluster membership
fviz_dend(res.hc, cex = 0.5, k = 3, color_labels = TRUE)

# Compute cophenetic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and the original distance
correlation <- cor(res.dist, res.coph)
print(correlation)

# Perform hierarchical clustering with a different method (optional)
res.hc2 <- hclust(res.dist, method = "average")
correlation2 <- cor(res.dist, cophenetic(res.hc2))
print(correlation2)

# Visualize the dendrogram with cluster membership by average method
fviz_dend(res.hc2, cex = 0.5, k = 3, color_labels = TRUE)

# Determine the cluster membership
grp <- cutree(res.hc, k = 3)
print(grp)

# Display the distribution of clusters
table_grp <- table(grp)
print(table_grp)


#=================================================================================
#============================================================================================

# Read the CSV file (replace 'your_file.csv' with the actual file path)
data <- read.csv("D:/kaushik/MSC/MVAsem2/marketing.csv")
head(data,2)

#Convert non-numerical col to numerical col
data$Education <- as.numeric(factor(data$Education))
data$Marital_Status <- as.numeric(factor(data$Marital_Status))
head(data)

#Check for null values and remove it
data <- na.omit(data)
null_countm <- colSums(is.na(data))
print(null_countm)
head(data)

# Remove non-numeric columns
data <- data[, sapply(data, is.numeric)]

# Remove second and third last columns
data<- data[, -c(ncol(data) - 2, ncol(data) - 1)]
head(data,2)

# Standardize the data
data_scaled <- scale(data)
head(data_scaled,2)

# Visualize optimal number of clusters using WCSS & silhouette
fviz_nbclust(data_scaled, kmeans, method = "wss")
fviz_nbclust(data_scaled, kmeans ,method='silhouette')


#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 3 clusters
km1m <- kmeans(data_scaled, centers = 2, nstart = 25)
km1m

#plot results of final k-means model
fviz_cluster(km1m, data = data_scaled)

#find means of each cluster
cluster_means <- aggregate(data, by=list(cluster=km1m$cluster), mean)
cluster_means

# Combine cluster assignments with original data
final_datam <- cbind(data, cluster = km1m$cluster)

# Create pie charts for each cluster
plots <- lapply(unique(final_datam$cluster), function(cluster_num) {
  cluster_subset <- final_datam[final_datam$cluster == cluster_num, ]
  region_counts <- table(cluster_subset$Marital_Status)
  pie_data <- data.frame(Marital_Status = names(region_counts), count = as.numeric(region_counts))
  ggplot(pie_data, aes(x = "", y = count, fill = Marital_Status)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    labs(title = paste("Marital Status Distribution for Cluster", cluster_num)) +
    theme_void() +
    theme(legend.position = "right")
})

# Combine plots into a single plot with subplots
combined_plot <- wrap_plots(plots, nrow = 1)

# Display the combined plot
print(combined_plot)

# Function to create bar graphs for Product_ID against Quantity for all clusters together
plot_Education_vs_income_all_clusters <- function(cluster_data) {
  ggplot(cluster_data, aes(x = as.factor(Education), y = Income)) +
    geom_bar(stat = "summary", fun = "mean", fill = "purple", color = "black") +
    labs(title = "Education vs. Income",
         x = "Education", y = "Average Income") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# Create bar graph for all clusters together
plot_list <- list()
for (i in unique(final_datam$cluster)) {
  cluster_subset <- final_datam[final_datam$cluster == i, ]
  plot_list[[i]] <- plot_Education_vs_income_all_clusters(cluster_subset) +
    ggtitle(paste("Cluster", i))
}

# Combine plots into a single plot with subplots
all_plots <- do.call(grid.arrange, c(plot_list, ncol = 2))

# Display the combined plot
print(all_plots)

