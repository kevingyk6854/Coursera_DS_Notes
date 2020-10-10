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
