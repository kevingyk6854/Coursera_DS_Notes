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

## SVD
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

# 3. relationship between PCA and SVD --- basically they are the same
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)

plot(pca1$rotation[, 1], svd1$v[, 1], pch=19, xlab="Principal Component 1", ylab="Right Singular Vector (V) 1")
abline(c(0,1))

# 4. back to components of the SVD - variance explained
constantMatrix <- dataMatrixOrdered * 0
for (i in 1:dim(dataMatrixOrdered)[1]) {constantMatrix[i,] <- rep(c(0,1), each = 5)}
svd1 <- svd(constantMatrix)
par(mfrow = c(1,3))
image(t(constantMatrix)[, nrow(constantMatrix):1])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19) # proportion or weight or likelihood


# ================================================
# add second pattern
set.seed(678910)
for (i in 1:40) {
  # flip a coin
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5) # 50%的概率硬币朝上或朝下
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  
  # if coin is heads add a common pattern to that row
  if (coinFlip1) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
  }
  if (coinFlip2) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
  }
}

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

# singular value decompostion - true patterns
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), xlab = "Column", ylab = "Pattern 1", pch = 19)
plot(rep(c(0, 1), 5), xlab = "Column", ylab = "Pattern 2", pch = 19) # proportion or weight or likelihood

# v and patterns of variance in rows
plot(svd2$v[, 1], xlab = "Column", ylab = "First right singular vector (V1)", pch = 19)
plot(svd2$v[, 2], xlab = "Column", ylab = "Second right singular vector (V2)", pch = 19)

# d and variance explained
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained", pch = 19)
# result: the first five columns represents a large amount of variation in the data set
# the second component only captures about 18% or so percent of variation and it could trail off from there



# ==========================================
# missing value -> svd() will cause an error
dataMatrix2 <- dataMatrixOrdered
## randamly insert some missing value
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA

svd1 <- svd(scale(dataMatrix2))
# result: Error in svd(scale(dataMatrix2)) : infinite or missing values in 'x'


# =================================================================
# face example
faceData <- load(here::here("data", "face.rda"))
faceData
image(t(faceData)[, nrow(faceData):1])

# variance 方差 explained
face_svd1 <- svd(scale(faceData))
plot(face_svd1$d^2/sum(face_svd1$d^2), xlab = "Singular vector", ylab = "Variance explained", pch = 19)

# create approximations
svd1 <- svd(scale(faceData))

# note: %*% is matrix multiplication
# here svd1$d[1] is a constant
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1] # X = U*D*V^T

# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])

par(mfrow = c(1,4))
image(t(approx1)[, nrow(approx1):1], main = "(a)")   # just use a single singular vector
image(t(approx5)[, nrow(approx5):1], main = "(b)")   # with the first 5 singular vectors
image(t(approx10)[, nrow(approx10):1], main = "(c)") # with the first 10 singular vectors
image(t(faceData)[, nrow(faceData):1], main = "(d)") # original one




