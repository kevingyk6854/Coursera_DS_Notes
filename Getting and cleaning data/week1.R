# load XML file
library(XML)
library(RCurl)

fileUrl <- getURL("https://www.w3schools.com/xml/simple.xml")
doc <- xmlTreeParse(fileUrl, useInternal=TRUE)
doc

rootNode <- xmlRoot(doc)
names(rootNode)

xmlName(rootNode)

rootNode[[1]][[2]][[1]]

xmlSApply(rootNode[[1]], xmlValue)

# load JSON file
library(jsonlite)
json_data <- fromJSON("https://api.github.com/users/jtleek/repos")
names(json_data)


# week 1 quiz
library(here)
library(readr)
library(dplyr)
library(magrittr)
# 1. The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf

# How many properties are worth $1,000,000 or more?
ss06hid_1 <- read_csv(here::here("data/week1_test", "getdata_data_ss06hid.csv"))

property_1E6 <- ss06hid_1 %>%
  filter(VAL == 24) %>%
  count(n(), name="property_count")

property_1E6$property_count # 53

# 2. Use the data you loaded from Question 1. Consider the variable FES in the code book. 
# Which of the "tidy data" principles does this variable violate?
ss06hid_1$FES
# Tidy data has one variable per column.

# 3. Download the Excel spreadsheet on Natural Gas Aquisition Program here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx
# Read rows 18-23 and columns 7-15 into R and assign the result to a variable called:
library(openxlsx)
dat <- read.xlsx(here::here("data/week1_test", "getdata_data_DATA.gov_NGAP.xlsx"),
                        sheet = 1, cols = 7:15, rows = 18:23)

# What is the value of:
sum_val <- sum(dat$Zip*dat$Ext,na.rm=T)
sum_val # 36534720

# 4. Read the XML data on Baltimore restaurants from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
# How many restaurants have zipcode 21231?
library(XML)
library(RCurl)
# rest_Url <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
# rest_doc <- xmlTreeParse(rest_Url, useInternal=TRUE)

rest_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
rest_doc <- xmlTreeParse(sub("s", "", rest_Url), useInternal=TRUE)
rootNode <- xmlRoot(rest_doc)

zip_sum <- rootNode %>%
  xpathSApply("//zipcode", xmlValue) %>%
  data.frame() %>%
  filter(zip == 21231) %>%
  count(n()) # 127
# sum(zip == 21231)

# 5. The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv

# using the fread() command load the data into an R object
library(data.table)
ss06pid_1 <- fread(here::here("data/week1_test", "getdata_data_ss06pid.csv"))
head(ss06pid_1, 5)

# The following are ways to calculate the average value of the variable - pwgtp15, broken down by sex.
mean_pwgtp15 <- ss06pid_1[, mean(pwgtp15), by = SEX]
mean_pwgtp15

#  which will deliver the fastest user time?
DT <-ss06pid_1
# system.time(rowMeans(DT[DT$SEX==1]), rowMeans(DT[DT$SEX==2]))

system.time(mean(DT[DT$SEX==1,]$pwgtp15), mean(DT[DT$SEX==2,]$pwgtp15))

system.time(DT[,mean(pwgtp15),by=SEX])

system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))

system.time(tapply(DT$pwgtp15,DT$SEX,mean))

system.time(mean(DT$pwgtp15,by=DT$SEX))




