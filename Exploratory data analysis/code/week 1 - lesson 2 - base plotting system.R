library(here)
library(magrittr)
library(readr)
library(dplyr)
par(mfrow = c(1,1))

pollution <- read.csv(here("data", "avgpm25.csv"), 
                      colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)

# the base plotting system
library(datasets)
data("cars")
# scatterplot
with(cars, plot(speed, dist))

# boxplot
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")


# lattice plot
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))


# ggplot2 system
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)


# ------------------------ Base Graphics Parameters --------------------------
# default values for gobal graphics parameters
par("lty")
par("col")


# base plot with annotation
# 1.
with(airquality, plot(Wind, Ozone))
# add a title
title(main = "Ozone and Wind in New York City") 

# 2.
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
# plot all of the data points that are in the month of May in blue
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

# 3.
# add the argument type = "n" -> sets up the plot and initializes the graphics
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
# add a legend on the top right
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

# base plot with regression line
# 4.
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)


# multiple base plots
par(mfrow = c(1,2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1,3), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer = TRUE)
})

















