library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(here)

str(mpg)
# ------------------------------------------------------------
# qplot()

# ggplot2 "hello world"
qplot(displ, hwy, data = mpg)

# modifying aesthetics
qplot(displ, hwy, data = mpg, color = drv) # group by drv

# adding a geom
qplot(displ, hwy, data = mpg, geom = c("point", "smooth")) # two types of geoms : point and smooth line

# histogram
qplot(hwy, data = mpg, fill = drv)

# facets == panels in lattice
# The idea is that you can create separate plots, which indicate, 
# again, subsets of your data indicated by a factor variable. 
# And you can make a panel of plots to look at separate subsets together. 
# So, one option to be to color code the subsets according to different colors

# here's no variable that indicates how many rows there should be in this plot. 
# And so that's why in the facets argument, I have a dot on the left-hand side.
qplot(displ, hwy, data = mpg, facets = .~drv)

qplot(hwy, data = mpg, facets = drv~., binwidth = 2)
# separated by a tilde. And so the variable on the right-hand side, 
# determines the columns of the panels. And the variable on the left-hand side 
# indicates the rows of this kind of matrix here


# density smooth
qplot(hwy, data = mpg, geom = "density")
qplot(hwy, data = mpg, geom = "density", color = drv)

# add a lm()
qplot(displ, hwy, data = mpg, color = drv) +
  geom_smooth(method = "lm")

qplot(displ, hwy, data = mpg, facets = .~drv) +
  geom_smooth(method = "lm")


# ------------------------------------------------------------

test <- load(here("data", "maacs.rda"))
# write.csv(maacs, here("data", "maacs.csv"))
str(maacs)

download.file("https://github.com/lupok2001/datasciencecoursera/raw/master/maacs.Rda",
              dest=here("data", "maacs_2.rda"),mode="wb")
test_2 <- load(here("data", "maacs_2.rda"))
str(maacs)

qplot(logpm25, NocturnalSympt, data = maacs, facets = .~bmicat, 
      geom = c("point", "smooth"), method = "lm")

# build up in layers
head(maacs[, 1:3])

ggplot(maacs, aes(logpm25, NocturnalSympt)) + 
  geom_point() + # auto-print plot object
  facet_grid(.~bmicat) + # add layer - facets (#num of plots = factors)
  geom_smooth(method = 'lm') # add layer - smooth 


# modify aesthetics
ggplot(maacs, aes(logpm25, NocturnalSympt)) + 
  geom_point(color = "steelblue", size = 4, alpha = 1/2)

ggplot(maacs, aes(logpm25, NocturnalSympt)) + 
  # assigning the color to be the value of a variable, according to the group
  geom_point(aes(color = bmicat), size = 4, alpha = 1/2)


# modify labels
ggplot(maacs, aes(logpm25, NocturnalSympt)) + 
  geom_point(aes(color = bmicat)) +
  labs(title = "MAACS Cohort") +
  labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")










