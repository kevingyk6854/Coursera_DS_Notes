library(here)

load(here::here("data", "samsungData.rda"))
names(samsungData)[1:12]

table(samsungData$activity)

# plotting average acceleration for first subject
par(mfrow = c(1,2), mar = c(5, 4, 1, 1))
samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1) # subset vector with subject col = 1
plot(sub1[,1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[,2], col = sub1$activity, ylab = names(sub1)[2])
legend(x = "bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), pch = 1)

# clustering based just on average acceleration
source(here::here("code", "myplclust.R"))
distanceMatrix <- dist(sub1[, 1:3]) # calculate a distance matrix using the dist() with a Euclidean distance as the default
hclustering <- hclust(distanceMatrix) # do a hierarchical clustering of the data
myplclust(hclustering, lab.col = unclass(sub1$activity)) # for visualisation

# result: the average acceleration features do not appear to be able to discriminate between the six differnet behaviours

# ======================================================================

# plotting max acceleration for the first subject
par(mfrow = c(1,2))
plot(sub1[,10], col = sub1$activity, ylab = names(sub1)[10], pch = 19)
plot(sub1[,11], col = sub1$activity, ylab = names(sub1)[11], pch = 19)

# clustering based just on maximum acceleration
par(mfrow = c(1,1))
distanceMatrix <- dist(sub1[, 10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity)) # for visualisation

# result: seems to separate out moving from non-moving, but within moving/non-moving cluster a bit hard to tell what is what as they still mixed

# ======================================================================

# Singular Value Decomposition (SVD) X = UDV^T
svd1 = svd(scale(sub1[, -c(562, 563)])) # remove last two as they are the activity identifier and the subject identifier which not interesting
par(mfrow = c(1,2))
plot(svd1$u[,1], col = sub1$activity, pch = 19)
plot(svd1$u[,2], col = sub1$activity, pch = 19)

# result: v1 separates out the moving from non-moving clusters
##        v2 is vague, not clear what is different

# Find maximum contributor -> producing the most variation
plot(svd1$v[,2], pch = 19)

# New clustering with maximum contributor
maxContrib <- which.max(svd1$v[,2])
par(mfrow = c(1,1))
distanceMatrix <- dist(sub1[, c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity)) # for visualisation

names(samsungData)[maxContrib]
# result: "fBodyAcc.meanFreq...Z"

# ======================================================================

# 
# K-Means clustering (nstart = 1, first try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6)
table(kClust$cluster, sub1$activity)
# result: k-means has trouble separating out the laying, sitting and standing from non-moving clusters

# K-Means clustering (nstart = 1, second try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 1) # nstart will create multiple configurations / multiple random initial values (centroids) and show the best one
table(kClust$cluster, sub1$activity)

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

## Q&A
## Q: Why does the K-Means algorithm produce different clustering solutions every time you run it?
## A: K-Means chooses are random starting point by default

# ======================================================================

# cluster 1 variable centers (laying)
plot(kClust$centers[1, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")

# cluster 2 variable centers (walking)
plot(kClust$centers[4, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")














