# K-Means Clustering Analysis Guide (https://uc-r.github.io/kmeans_clustering)

library(tidyverse)
library(cluster)
library(factoextra)

df <- na.omit(USArrests)
df <- scale(df)

# calculate and visualize dissimilarity distances
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# compute k-means
k2 <- kmeans(df, centers = 2, nstart = 25)

# visualize clusters
fviz_cluster(k2, df)

# use pairwise scatter plots to visualize clusters using the original variables
df %>%
  as_tibble() %>% 
  mutate(cluster = k2$cluster,
         state = names(k2$cluster)) %>%
  ggplot(aes(UrbanPop, Murder)) + geom_text(aes(color = factor(cluster), label = state))

# run kmeans for different numbers of clusters
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# compare clusters using plot grid
p1 <- fviz_cluster(k2, df, geom = "point") + ggtitle("k = 2")
p2 <- fviz_cluster(k3, df, geom = "point") + ggtitle("k = 3")
p3 <- fviz_cluster(k4, df, geom = "point") + ggtitle("k = 4")
p4 <- fviz_cluster(k5, df, geom = "point") + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


# use elbow method to determine appropriate number of clusters
set.seed(123)

# func to compute total within-cluster sum of squares
wss <- function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

# compute and plot wss values for k = 1 to k = 15
k.values <- 1:15
wss.values <- map_dbl(k.values, wss)

plot(k.values, wss.values)

ggplot(mapping = aes(k.values, wss.values)) + 
  geom_point() + 
  geom_line() +
  ylab("Total within-cluster sum of squares") +
  xlab("Number of clusters K")

# function for using elbow method (instead of the above)
fviz_nbclust(df, kmeans, method = "wss")
