library(readr)
library(here)

if(!file.exists(./data)) {dir.create("./data")}
file_url <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(file_url, destfile = "./data/cameras.csv", method = "curl")
camera_data <- read.csv(here::here("data", "cameras.csv"))

# look at a bit of the data
# head(camera_data, 3) # default, top 6
# tail(camera_data, 3)
names(camera_data)

# fixing character vectors - tolower(), toupper()
tolower(names(camera_data))

# fixing character vectors - strsplit()
split_names <- strsplit(names(camera_data), "\\.") # (regular expression/regex)正则
split_names[[5]]
split_names[[6]]

# quick aside - lists
mylist <- list(letters = c("A", "B", "C"), numbers = 1:3, matrix(1:25, ncol = 5))
head(mylist)

mylist[1]
mylist[[1]]
mylist$letters

# fixing character vectors - sapply(X, function) (applies a function to each element in a vector or list)
split_names[[6]][1]
first_element <- function(x) {x[1]}
sapply(split_names, first_element)


# ---------------------------------------------------------------------------
reviews <- read.csv(here::here("data", "reviews.csv"))
solutions <- read.csv(here::here("data", "solutions.csv"))

# fixing character vectors - sub(pattern, replacement, x)
names(reviews)
sub("_", "", names(reviews),) # replace one "_" (underscore) to ""

# fixing character vectors - gsub() # replace all elements in a string
test <- "this_is_a_test"
sub("_", "", test)
gsub("_", "", test)

# finding values - grep(), grepl() # grep() return indexes, grepl() return a vector with T/F
grep("Alameda", camera_data$intersection)
table(grepl("Alameda", camera_data$intersection))

# return rows without "Alameda"
camera_data2 <- camera_data[!grepl("Alameda", camera_data$intersection),]
head(camera_data2)

# return the "values" where Alameda appears
grep("Alameda", camera_data$intersection, value = TRUE)

# return did not appear
grep("JeffStreet", camera_data$intersection) # interger(0)
length(grep("JeffStreet", camera_data$intersection)) # 0


# more useful string functions
library(stringr)
nchar("Jeffrey Leek") # length of characters in a string

substr("Jeffrey Leek", 1, 7) # get [1:7] characters

paste("Jeff", "Leek") # paste two strings together separated by space (default)
paste0("Jeff", "Leek") # paste with no space in between

str_trim("JEF      ") # trim off any excess space in a string

# ---------------------------------------------------------------------------
# working with dates
d1 = date()
d1
class(d1) # character

d2 = Sys.Date()
d2
class(d2) # Date

format(d2, "%a %b %d") # abbreviated weekday + abbreviated month + day

# creating dates
x = c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z = as.Date(x, "%d%b%Y") # day + ab month + 4 digit year
z # "1960-01-01" "1960-01-02" "1960-03-31" "1960-07-30"

z[1] - z[2] # Time difference of -1 days
as.numeric(z[1] - z[2]) # -1

# converting to Julian
weekdays(d2)
months(d2)

julian(d2)
# [1] 18552         # the number of days that have occurred since the origin date
# attr(,"origin")
# [1] "1970-01-01"

# Lubridate
library(lubridate)
# all be converted to y-m-d
ymd("20140108")   # "2014-01-08"
mdy("08/04/2014") # "2014-08-04"
dmy("03-04-2013") # "2013-04-03"


# dealing with times
ymd_hms("2011-08-03 10:15:03") # "2011-08-03 10:15:03 UTC"

ymd_hms("2011-08-03 10:15:03", tz = "Pacific/Auckland") # "2011-08-03 10:15:03 NZST"
# ?Sys.timezone
Sys.timezone(location = TRUE) # "Australia/Sydney"


# some functions
x = dmy(c("1jan2013", "2jan2013", "31mar2013", "30jul2013"))
wday(x[1]) # get weekday

wday(x[1], label = TRUE) # get weekday as label
# Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat









