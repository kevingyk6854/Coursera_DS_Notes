x<-c(88, 78, 67, 69, 62, 100, 73, 45)
sort(x, decreasing = FALSE)
# [1]  45  62 | 67  69 | 73  78 | 88 100
summary(x)

# R HDF5
# for more to use R HDF5
# https://bioconductor.org/packages/release/bioc/vignettes/rhdf5/inst/doc/rhdf5.html
install.packages("BiocManager")
BiocManager::install("rhdf5")

# source("http://biocondunctor.org/bioLite.R")
# bioLite("rhdf5")

library(rhdf5)

created <- h5createFile("example.h5")

created <- h5createGroup("example.h5", "foo")
created <- h5createGroup("example.h5", "baa")
created <- h5createGroup("example.h5", "foo/foobaa")
h5ls("example.h5")

A = matrix(1:10,nr=5,nc=2)
h5write(A, "example.h5","foo/A")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "example.h5","foo/B")
C = matrix(paste(LETTERS[1:10],LETTERS[11:20], collapse=""),nr=2,nc=5)
h5write(C, "example.h5","foo/foobaa/C")
df = data.frame(1L:5L,seq(0,1,length.out=5),
                c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
h5write(df, "example.h5","df")
h5ls("example.h5")


D = h5read("example.h5","foo/A")
E = h5read("example.h5","foo/B")
F = h5read("example.h5","foo/foobaa/C")
G = h5read("example.h5","df")

h5write(c(12,13,14), "example.h5", "foo/A", index=list(1:3, 1))
h5read("example.h5","foo/A")

# ---------------------------------------------------------------------------
# get data off webpages (html)
# https://scholar.google.com/citations?hl=en&user=HI-I6C0AAAAJ
con = url("https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode

# parsing with xml
library(XML)
library(RCurl)
library(httr) # 爬虫用

hi_url <- "https://scholar.google.com/citations?hl=en&user=HI-I6C0AAAAJ"
# html = GET(url) # httr package - GET()
# content = content(html, as='text')
# parsed_html = htmlParse(contetn, asText=TRUE)
hi_html <- htmlParse(rawToChar(GET(hi_url)$content))

xpathSApply(hi_html, "//title", xmlValue)

xpathSApply(hi_html, "//td[@class='gsc_a_c']", xmlValue)

# accessing websites with passwords
pg2 = GET("http://httpbin.org/basic-auth/user/passwd",
          authenticate("user", "passwd"))
pg2

names(pg2)

# using handles(httr)
google = handle("https://www.google.com/")
pg2 = GET(handle=google, path='/')
pg3 = GET(handle=google, path='search')
# ---------------------------------------------------------------------------

# get data from twitter API
# accessing twitter from R
myapp = oauth_app("twitter",
                  key ="your_consumer_key",
                  secret = "your_consumer_secret") # start the authorisation process for app
sig = sign_oauth1.0(myapp,
                    token = "your_token",
                    token_secret = "your_token_secret")
homeTL = GET("....json", sig)

# converting the json object
json1 = content(homeTL) # get a json object
json2 = jsonlite::fromJSON(toJSON(json1)) # fromJSON -> create a data frame
