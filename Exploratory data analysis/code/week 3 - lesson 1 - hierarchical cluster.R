library(readr)
library(here)
library(dplyr)
library(ggplot2)

# hierarchical clustering
set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x, y, col = 'blue', pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

# dist(x, method)
dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame) # default to euclidean distance

# hclust - Hierarchical Clustering (get the tree)
hClustering <- hclust(distxy)
plot(hClustering)

source(here("code", "myplclust.R"))
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

# https://www.r-graph-gallery.com/

# heatmap()
# to use hierarchical clustering analysis to organise the rows and the columns of the tables
dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)











