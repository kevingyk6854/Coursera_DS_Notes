# matrix data
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

# cluster the data
heatmap(dataMatrix)

# add a pattern
set.seed(678910)
for (i in 1:10) {
  # flip a coin 
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip) {
    # five of the columns have a mean of 0 and the other five have a mean of 3
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each = 5)
  }
}

# the data after adding a pattern
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
# the clustered data
heatmap(dataMatrix)

# patterns in rows and columns
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3))
image(t(dataMatrix)[, nrow(dataMatrix):1])
plot(rowMeans(dataMatrixOrdered), 40:1, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

## SCD
# 1. component of the SVD - U and V
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3), mar = rep(2, 4))
image(t(dataMatrix)[, nrow(dataMatrix):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)

# 2. component of the SVD - Variance explained
par(mfrow = c(1,2), mar = rep(2, 4))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19) # proportion or weight or likelihood











