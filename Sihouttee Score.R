library(cluster)
library(factoextra)
#install.packages("factoextra")
library(dplyr)
data("mtcars")
library(corrplot)

df <- scale(mtcars)
corrmatrix <- cor(df)
corrplot(corrmatrix, method = 'number')

# Removed qsec, am adn gear column, they are not on the same level.

df <- df[-c(7,9,10)]

# Clustering

#km <- kmeans(df, centers = 3, nstart=25)
#ss <- silhouette(km$cluster, dist(df))
#mean_ss <- mean(ss[, 3])
silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

fviz_nbclust(df, kmeans, method='silhouette')
 # Plotting shows the optimal number of clusters as 2.
