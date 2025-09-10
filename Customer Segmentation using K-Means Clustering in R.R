install.packages(c("ggplot2","dplyr","cluster","factoextra"))
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
data <- read.csv('Mall_Customers.csv')
head(data)
summary(data)
# Scatter Plot of Income vs Spending Score
ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) +
  geom_point(color = 'blue') +
  labs(title = "Income vs Spending Score", x = "Annual Income (k$)", y = "Spending Score (1-100)")
customer_data <- data[, c('Annual.Income..k..', 'Spending.Score..1.100.')]
# Scale Data
customer_data_scaled <- scale(customer_data)
# Elbow Method
fviz_nbclust(customer_data_scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method for Optimal Clusters")
set.seed(123)  # For reproducibility
# Apply k-means with k=5
kmeans_result <- kmeans(customer_data_scaled, centers = 5, nstart = 25)

# Add cluster info to original data
data$Cluster <- as.factor(kmeans_result$cluster)
ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Customer Segments", x = "Annual Income (k$)", y = "Spending Score (1-100)") +
  theme_minimal()
