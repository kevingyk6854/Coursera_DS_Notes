library(here)
library(readr)
library(magrittr)
library(dplyr)

# 1. Create a logical vector that identifies the households on greater than 10 acres 
# who sold more than $10,000 worth of agriculture products. 
# Assign that logical vector to the variable agricultureLogical. 
# Apply the which() function like this to identify the rows of the data frame 
# where the logical vector is TRUE.
# 
# which(agricultureLogical)
# 
# What are the first 3 values that result?
# hid_data <- read_csv(here::here("data/week3_quiz", "getdata_data_ss06hid.csv"))
hid_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
hid_data <- read.csv(hid_url)
hid_data

agricultureLogical <- hid_data$ACR == 3 & hid_data$AGS == 6
which(agricultureLogical)

# 2. Using the jpeg package read in the following picture of your instructor into R
# https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg
# Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting data?
library(jpeg)
jpeg_data <- readJPEG(here::here("data/week3_quiz", "getdata_jeff.jpg"), native=TRUE)
quantile(jpeg_data, probs = c(0.3, 0.80))


# 3. Match the data based on the country shortcode. How many of the IDs match? 
# Sort the data frame in descending order by GDP rank (so United States is last). 
# What is the 13th country in the resulting data frame?
gdp_data <- read_csv(here::here("data/week3_quiz", "getdata_data_GDP.csv"), skip = 4, n_max = 190)
country_data <- read_csv(here::here("data/week3_quiz", "getdata_data_EDSTATS_Country.csv"))

merged_data <- dplyr::inner_join(gdp_data, country_data, by = c("X1" = "CountryCode")) %>%
  arrange(desc(X2))
nrow(merged_data)
merged_data[13,]$`Long Name`


# 4. What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
avg_rank <- merged_data %>% 
  filter(`Income Group` == 'High income: OECD' | `Income Group` == 'High income: nonOECD') %>%
  group_by(`Income Group`) %>%
  summarise(avg_gdp_rank = mean(X2))
avg_rank

# 5. Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. 
# How many countries are Lower middle income but among the 38 nations with highest GDP?
merged_data$income_group = cut(merged_data$X2, breaks = 5)
vs <- table(merged_data$income_group, merged_data$`Income Group`)
vs

vs[1,'Lower middle income'] # 5






