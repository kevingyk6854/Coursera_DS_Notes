library(here)
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)

pollution <- read.csv(here("data", "avgpm25.csv"), 
                      colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)

# ------------------- One dimension ---------------------
# five-number summary
summary(pollution$pm25)

# boxplot
boxplot(pollution$pm25, col = "blue")
# overlaying the feature, could be used to set a surveyor's pole and check the quantile of data
abline(h = 12) # h -> horizontal, 12 is the national ambient air quality

# histogram
hist(pollution$pm25, col = "green")
rug(pollution$pm25) # could be used to find outliers and where the bulk of the data are

# change the breaks
hist(pollution$pm25, col = "green", breaks = 100)

# overlaying feature
hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2) # v -> vertical, lwd -> heavy
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

# barplot
barplot(table(pollution$region), col = "wheat", main = "Number of countries in each region")


# ------------------- Two dimension ---------------------
# multiple boxplots
boxplot(pm25 ~ region, data = pollution, col = "red")
# Analysis: in the west you see that on average it tends to be a little bit lower, but that, all of the extreme values of the distribution so to speak are in the western United States. So the west has an average that's lower but it has generally a larger spread,


# multiple histograms
par(mfrow = c(2,1), mar = c(4,4,2,1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
# Analysis: The western counties tend to be lower on average, but they have more extreme values that are higher. [COUGH] And the eastern counties are generally higher on average, but they tend to be but they don't have those kind of very high, extreme values.


# scatterplot
par(mfrow = c(1,1))
# as the latitude gets increases you're going north.
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)
# Analysis: here isn't a very strong particularly strong north-south trend. it does seem that the, the the pm2.5 tends to be higher in the middle latitudes, and not quite as high in the, in the upper and lower latitudes. I've added a d, a horizontal dashed line here at 12, so you can see roughly where the national ambient air quality standard is. And you can see that there are a number of dots that are above it, which are technically speaking would be out of compliance.


# scatterplot - using color
# group by different region
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)


# multiple scatterplots
par(mfrow = c(1,2), mar = c(5,4,2,1))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
# Analysis: On the left hand, left hand side I've got all the western counties and on the east, on the right hand side I've got all the eastern counties and I'm plotting them by latitude and pm2.5. So you can see that in the, in both cases the kind of higher pollution- counties tend to be in the middle latitudes. and, and the north and south, northern and southern latitudes tend to have generally lower pollution. This is both the case in the eastern and western United States, so that's kind of interesting.

# -------------------------------------------------------------------




