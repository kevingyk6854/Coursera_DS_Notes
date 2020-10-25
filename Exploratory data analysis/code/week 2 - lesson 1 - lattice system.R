# · The lattice plotting system does not have a "two-phase" aspect with
#   separate plotting and annotation like in base plotting
# · All plotting/annotation is done with a single function call
library(lattice)
library(datasets)

# simple lattice plot
xyplot(Ozone ~ Wind, data = airquality)

# convert "Month" to a factor variable
# plotting ozone versus wind by month. So you can interpret the formula as, 
# I want to look at the relationship between ozone and wind for each level of month 
# and so month goes from five, six, seven, eight, and nine.
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))


# lattice behaviour
p <- xyplot(Ozone ~ Wind, data = airquality) # nothing happens
print (p) # plot appears


# lattice panel functions
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x +f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2,1)) # plot with 2 panels

# custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...) # first call the default panel function for 'xyplot'
  panel.abline(h = median(y), lty = 2) # add a horizontal line at the median
})

# add a regression line
xyplot(y ~ x | f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...) # first call the default panel function for 'xyplot'
  panel.lmline(x, y, col = 2) # overlay a simple linear regression line
})















