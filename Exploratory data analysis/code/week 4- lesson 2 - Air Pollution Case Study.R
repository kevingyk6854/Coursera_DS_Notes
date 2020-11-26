# library(readr)
library(here)
library(magrittr)
## Now the first: have a kind of a basic idea in mind of what you're looking for. 
## This could come in the form of a hypothesis or even more generally just 
## in the basic question, you know what are you trying to answer with this data set? 
# Question: Are air pollution levels lower now than they were before? 

pm0 <- read.table(here::here("data/EPA/pm25_data", "RD_501_88101_1999-0.txt"),
                  comment.char = "#", header = FALSE, sep = "|",
                  na.strings = "")
pm1 <- read.table(here::here("data/EPA/pm25_data", "RD_501_88101_2012-0.txt"),
                  comment.char = "#", header = FALSE, sep = "|",
                  na.strings = "")

dim(pm0)

# add col names to the table
cnames <- readLines(here::here("data/EPA/pm25_data", "RD_501_88101_1999-0.txt"), 1) %>%
  strsplit("|", fixed = TRUE)
names(pm0) <- make.names(cnames[[1]])
names(pm1) <- make.names(cnames[[1]])
head(pm0)

x0 <- pm0$Sample.Value
str(x0)
summary(x0)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00    7.20   11.50   13.74   17.90  157.10   13217 

x1 <- pm1$Sample.Value
summary(x1)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## -10.00    4.00    7.63    9.14   12.00  908.97   73133 

# get the proportion
mean(is.na(x0)) # 0.1125608 -> about 11% value missing in 1999
mean(is.na(x1)) # 0.05607125 -> about 5% value missing in 2000

##
# negative values appear which is unusual
negative <- x1 < 0
sum(negative, na.rm = TRUE) # 26474
mean(negative, na.rm = TRUE) # 0.0215034 -> about 2% negative value in 2000

# convert dates from 'int' to 'date' format
dates <- pm1$Date
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)

# draw a hist plot to show when these negative values appear
hist(dates[negative], "month")


# subset the data set and only choose New York
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

# format to "County.Code"."Site.ID"
site0 <- paste(site0[, 1], site0[, 2], sep = ".")
site1 <- paste(site1[, 1], site1[, 2], sep = ".")

# find out sites appear in the both periods
both <- intersect(site0, site1)
both

# re-subset the original data sets
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

# count how many rows for each county.site has
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

# re-subset and get data which has county = 63 and site = 2008
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

dim(pm1sub)

dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)


## building a panel plot
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))

plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm = T))

# calculate the range of y-axes scale
rng <- range(x0sub, x1sub, na.rm = T)
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))

plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))

# result: what actually is quite interesting. Is that not only are the average levels going down but these extreme values are also coming down across the years, too. So now, so we, so on average we're kind of breathing in lower levels of pollution.

## exploring change at the state level
# tapply() function takes the mean of a vector within subgroups determined by another vector
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm =T))
summary(mn0)

mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm =T))
summary(mn1)

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)

mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)

par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[, 2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 52), mrg[, 3]))

# connect each state
segments(rep(1999, 52), mrg[, 2], rep(2012, 52), mrg[, 3])
