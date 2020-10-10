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





