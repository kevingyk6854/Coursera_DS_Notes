library(here)
library(magrittr)
library(readr)
library(dplyr)

pollution <- read.csv(here("data", "avgpm25.csv"), 
                      colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)