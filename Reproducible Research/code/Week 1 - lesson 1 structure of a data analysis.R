library(kernlab)
library(here)
library(magrittr)

# spam email data set
data(spam)

# perform the subsampling -> generate a test and trainning set (prediction)
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

names(trainSpam)
# summaries
table(trainSpam$type)
## nonspam    spam 
##    1381     906 

# plots
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

# relationships between predictors
plot(log10(trainSpam[, 1:4] + 1))

# hierarchical cluster analysis
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

# new clustering - take a base 10 log transformation
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

# statistical prediction / modeling
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFuction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)

library(boot)
# model - logistic regression model
# see if we can predict an email is spam or not by using just a single variable
for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType") # numType ~ [single variable]
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFuction, 2)$delta[2]
}

# which predictor has minimum corss-validated error?
names(trainSpam)[which.min(cvError)]
## result: [1] "charDollar"

# get a measure of uncertainty
## use the best model from the group
predictonModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## get predictions on the test set
predictionTest = predict(predictonModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

## classify as 'spam' for those with prob > 0.5
predictedSpam[predictonModel$fitted > 0.5] = "spam"

## classification table
table(predictedSpam, testSpam$type)
## result: predictedSpam nonspam spam
##               nonspam    1346  458
##                  spam      61  449

## error rate
(61+458)/(1246+458+61+449)
## result: [1] 0.2344173



