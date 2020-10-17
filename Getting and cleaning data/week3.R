library(readr)
library(here)

if(!file.exists(./data)) {dir.create("./data")}
file_url <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(file_url, destfile = "./data/restaurants.csv", method = "curl")
rest_data <- read.csv(here::here("data", "restaurants.csv"))

# look at a bit of the data
head(rest_data, 3) # default, top 6
tail(rest_data, 3)
names(rest_data)

# make summary
summary(rest_data)
# get in depth information
str(rest_data)
# quantiles of quantitative variables
quantile(rest_data$councilDistrict, na.rm = TRUE)
quantile(rest_data$councilDistrict, probs = c(0.5, 0.75, 0.9)) # get 50%, 70% and 90%

# make table
table(rest_data$zipCode, useNA = "ifany") 
# if this are any missing values, they'll be an added column to this table which will be NA,
# and it will tell you the number of missing values there is.

# 2-d table
table(rest_data$councilDistrict, rest_data$zipCode) # councilDistrict * zipCode

# check for missing values
sum(is.na(rest_data$councilDistrict))

any(is.na(rest_data$councilDistrict))

all(rest_data$zipCode > 0)


# row and col sums
colSums(is.na(rest_data))

all(colSums(is.na(rest_data)) == 0) # check if 0 no NA values


# values with specific characteristics
table(rest_data$zipCode %in% c("21212", "21213"))

rest_data[rest_data$zipCode %in% c("21212", "21213"),]

# cross tabs
data("UCBAdmissions")
DF = as.data.frame(UCBAdmissions)
summary(DF)

# on the left this is going to be the variable that you want to be displayed actually in the table. 
# So this is frequency and the table. And then you might want to break that down by some different kinds of variables. 
# So you might break it down by gender and you might break it down by weather they were admitted or not. 
xt <- xtabs(Freq ~ Gender + Admit, data=DF)
xt
#          Admit
# Gender   Admitted Rejected
# Male       1198     1493
# Female      557     1278


# flat tables
warpbreaks$replicate <- rep(1:9, len=54) # kind of group
xt <- xtabs(breaks ~ ., data=warpbreaks)
xt

ftable(xt) # summary each replicate 1-9


# size of a data set
fake_data = rnorm(1e5)
object.size(fake_data)

print(object.size(fake_data), units='Mb')

# ---------------------------------------------------------------

## create variables

rest_data <- rest_data

# create sequences - sometimes you need an index for your data set
s1 <- seq(1, 10, by=2); s1          # increasing each new value by 2

s2 <- seq(1, 10, length=3); s2      # create exactly 3 values

x <- c(1,3,8,25,100); seq(along=x)  # 'along' creates a vector of the same length as x, but with consecutive indices could be used for looping or accessing subsets of data set

# subsetting variables
rest_data$nearMe = rest_data$neighborhood %in% c("Roland Park", "Homeland")
table(rest_data$nearMe)

# creating binary variables
rest_data$zipWrong = ifelse(rest_data$zipCode < 0, TRUE, FALSE)
table(rest_data$zipWrong, rest_data$zipCode < 0)

# creating categorical variables
rest_data$zipGroups = cut(rest_data$zipCode, breaks = quantile(rest_data$zipCode))
table(rest_data$zipGroups)
#               0 - 25%             25% - 50%             50% - 75%            55% - 100%
# (-2.123e+04,2.12e+04]  (2.12e+04,2.122e+04] (2.122e+04,2.123e+04] (2.123e+04,2.129e+04] 
#                   337                   375                   282                   332 

table(rest_data$zipGroups, rest_data$zipCode)

# easier cutting
library(Hmisc)
# using Hmisc::cut2() function, break into 4 groups according to the quantiles
rest_data$zipGroups = cut2(rest_data$zipCode, g=4)
table(rest_data$zipGroups)
# [-21226,21205) [ 21205,21220) [ 21220,21227) [ 21227,21287] 
#            338            375            300            314 

# create factor variables -- using factor()
rest_data$zcf <- factor(rest_data$zipCode)
rest_data$zcf[1:10] # 32 Levels (32 different zip codes)
class(rest_data$zcf)

# levels of factor variables
yesno <- sample(c("yes", "no"), size = 10, replace = TRUE)
yesnofac <- factor(yesno, levels = c("yes", "no"))
relevel(yesnofac, ref = "yes")

as.numeric(yesnofac)


# using the mutate function
library(Hmisc)
library(plyr)

# using plyr::mutate() to transform columns, here zipGroups has been cut by quantiles
rest_data2 = mutate(rest_data, zipGroups = cut2(zipCode, g=4))
table(rest_data2$zipGroups)

# mutate(rest_data, zipCode = zipCode + 1)



# --------------------------------------------------------------------------------
# reshaping

library(reshape2)
head(mtcars)

# reshape2包：
# melt-把宽格式数据转化成长格式。
# cast-把长格式数据转化成宽格式。（dcast-输出时返回一个数据框。acast-输出时返回一个向量/矩阵/数组。）
# 注：melt是数据融合的意思，它做的工作其实就是把数据由“宽”转“长”。
# cast 函数的作用除了还原数据外，还可以对数据进行整合。
# dcast 输出数据框。公式的左边每个变量都会作为结果中的一列，而右边的变量被当成因子类型，每个水平都会在结果中产生一列。

# melting data frames
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars = c("mpg", "hp"))
head(carMelt, n=3) # mpgs
tail(carMelt, n=3) # hps

# casting data frames
# formula takes the form LHS ~ RHS
# LHS variable values will be in rows. RHS variables values will become column names. 
cylData <- dcast(carMelt, cyl ~ variable) # summarise the data set
cylData
#   cyl mpg hp
# 1   4  11 11
# 2   6   7  7
# 3   8  14 14
# for cylinder = 4, there are 11 measures for mpg (miles per gallen) and 11 measures for hp

cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData
#   cyl      mpg        hp
# 1   4 26.66364  82.63636
# 2   6 19.74286 122.28571
# 3   8 15.10000 209.21429


# averaging values
head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

# split
spIns = split(InsectSprays$count, InsectSprays$spray)
spIns

# lapply
sprCount = lapply(spIns, sum)
sprCount

# combine (unlist / sapply)
unlist(sprCount)
sapply(spIns, sum)

# plyr package
ddply(InsectSprays, .(spray), summarise, sum=sum(count))


# ----------------------------------------------------------------------
# merging data 
reviews <- read.csv(here::here("data", "reviews.csv"))
solutions <- read.csv(here::here("data", "solutions.csv"))

head(reviews, 2)
head(solutions, 2)

# 1. merge()
# all equals true. Which means if, if there's a value that appears in one 
# but not in the other, it should include another row but with na values for the missing values that are, 
# don't appear in the other data frame. 
merged_data <- merge(reviews, solutions, by.x = "solution_id", by.y = "id", all = TRUE)
head(merged_data)

# default - merge all common column names
intersect(names(solutions), names(reviews))

merged_data2 <- merge(reviews, solutions, all = TRUE)
head(merged_data2)

# 2. using "join" in the plyr package
df1 <- data.frame(id = sample(1:10), x = rnorm(10))
df2 <- data.frame(id = sample(1:10), y = rnorm(10))
arrange(join(df1, df2), id)

# if have multiple data frames
df1 <- data.frame(id = sample(1:10), x = rnorm(10))
df2 <- data.frame(id = sample(1:10), y = rnorm(10))
df3 <- data.frame(id = sample(1:10), z = rnorm(10))

dfList = list(df1, df2, df3)
join_all(dfList) # join based on common variables


# swirl lesson 1-3 please check /ref/Swirl Data Wangling xxx.Rmd


