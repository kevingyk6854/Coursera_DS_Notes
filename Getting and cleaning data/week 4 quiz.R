library(here)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)


# 1. Apply strsplit() to split all the names of the data frame on the characters "wgtp". 
# What is the value of the 123 element of the resulting list?
hid_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
hid_data <- read.csv(hid_url)
# hid_data

split_names <- strsplit(names(hid_data), "^wgtp") # (regular expression/regex)正则
split_names[[123]] # ""   "15"


# 2. Remove the commas from the GDP numbers in millions of dollars and average them. 
# What is the average?
gdp_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
gdp_data <- read.csv(gdp_url, skip = 4, nrows = 190)
gdp_data <- gdp_data[,c(1, 2, 4, 5)] # only choose the first 5 rows
colnames(gdp_data) <- c("CountryCode", "Rank", "Country", "Total")

# remove commas
gdp_data$Total <- as.numeric(gsub(",", "", gdp_data$Total))
mean(gdp_data$Total) # 377652.4
# head(gdp_data)


# 3. In the data set from Question 2 
# what is a regular expression that would allow you to count the number of countries 
# whose name begins with "United"? Assume that the variable with the country names 
# in it is named countryNames. How many countries begin with United?
# fix character
gdp_data$Country <- as.character(gdp_data$Country)
gdp_data$Country[99] <- "Côte d’Ivoire"
gdp_data$Country[186] <- "São Tomé and Príncipe"

table(grepl("^United", gdp_data$Country))
# FALSE  TRUE 
# 187     3 


# 4. Match the data based on the country shortcode. Of the countries 
# for which the end of the fiscal year is available, how many end in June?
gdp_data <- read_csv(here::here("data/week3_quiz", "getdata_data_GDP.csv"), skip = 4, n_max = 190)
country_data <- read_csv(here::here("data/week3_quiz", "getdata_data_EDSTATS_Country.csv"))

gdp_data <- gdp_data[,c(1, 2, 4, 5)] # only choose the first 5 rows
colnames(gdp_data) <- c("CountryCode", "Rank", "Country", "Total")

merged_data <- dplyr::inner_join(gdp_data, country_data, by = c("CountryCode" = "CountryCode"))

table(grepl("Fiscal year end: June", merged_data$`Special Notes`))
# FALSE  TRUE 
# 176    13 



# 5. You can use the quantmod (http://www.quantmod.com/) package to 
# get historical stock prices for publicly traded companies on the NASDAQ and NYSE. 
# Use the following code to download data on Amazon's stock price and get the times 
# the data was sampled.
# How many values were collected in 2012? How many values were collected on Mondays in 2012?
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

# collected in 2012
table(grepl("^2012", sampleTimes))
# FALSE  TRUE 
# 3223   250 

# or
amzn2012 <- sampleTimes[grep("^2012", sampleTimes)]
NROW(amzn2012)
# 250

# collected on Mondays in 2012
NROW(amzn2012[weekdays(amzn2012) == "Monday"])
# 47

num_mon_2012 <- data.frame(df_date = amzn2012) %>%
  filter(weekdays(df_date) == "Monday") %>%
  nrow()
num_mon_2012 # 47

# ?NROW




