library(here)
library(readr)
library(magrittr)
library(dplyr)

# select:  return a subset of the columns of a data frame
# filter:  extract a subset of rows from a data frame based on logical conditions
# arrange: reorder rows of a data frame
# rename:  rename variables in a data frame
# mutate:  add new variables / columns or transform existing variables
# summarise: generate summary statistics of different variables in the data frame, possibly within strata
chicago <- readRDS(here::here("data", "chicago.rds"))
str(chicago)

names(chicago)

# get city to dptp
chicago %>%
  select(city:dptp) %>%
  head()

# get except city to dptp
chicago %>%
  select(-(city:dptp)) %>%
  head()

i <- match("city", names(chicago))
j <- match("dptp", names(chicago))
head(chicago[,-(i:j)])

# ------------------------------------------------
# filter
chic.f <- filter(chicago, pm25tmean2 > 30)
head(chic.f, 5)

chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
head(chic.f, 5)

# ------------------------------------------------
# arrange
chicago <- arrange(chicago, date)
tail(chicago, 5)

chicago <- arrange(chicago, desc(date))
head(chicago, 5)

# ------------------------------------------------
# rename
chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)
names(chicago)

# ------------------------------------------------
# mutate
chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
chicago %>%
  select(pm25, pm25detrend) %>%
  head()

chicago <- mutate(chicago, tempcat = factor(1 * (tmpd>80), labels = c("cold", "hot")))
hotcold <- group_by(chicago, tempcat)
head(hotcold)

# ------------------------------------------------
# summarise
hotcold %>%
  dplyr::summarize(pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
years <- group_by(chicago, year)
dplyr::summarise(years, pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

# using pipline
chicago %>%
  mutate(month = as.POSIXlt(date)$mon + 1) %>%
  group_by(month) %>%
  dplyr::summarise(pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

# ------------------------------------------------


# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------



