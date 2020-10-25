# k-means clustering
set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x, y, col = 'blue', pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

# steps
# 1. randomly choose starting centroids
# 2. assign to the closest centroid
# 3. recalculate centroids
# 4. reassign values
# 5. update centroids

# kmeans(x, centers, iter.max, nstart)
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)

# get clusters
kmeansObj$cluster

par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3) # plot centroids

# heatmaps
set.seed(1234)
dataMatric <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj2 <- kmeans(dataMatric, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatric)[, nrow(dataMatric):1], yaxt = "n")        # image of original data
image(t(dataMatric)[, order(kmeansObj2$cluster)], yaxt = "n") # reorder the rows of data.frame













