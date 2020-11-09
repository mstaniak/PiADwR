# Getting started
## Install required packages
install.packages(
    c("readxl", "jsonlite", "readr", "dplyr", "xml2", "data.table", "httr")
)
library(readxl)
library(jsonlite)
library(readr)
library(dplyr)
library(data.table)
library(httr)
library(xml2)
library(lubridate)
## To see files in a directory
?list.files
# CSV
## Base
listings_rt <- read.csv('./data/csv/listings.csv',
                        header = TRUE, sep = ",")
listings_rt2 <- read.csv('./data/csv/listings.csv',
                        header = TRUE, sep = ",", stringsAsFactors = TRUE)
head(listings_rt)
summary(listings_rt)
summary(listings_rt2)

sapply(listings_rt, class)
listings2_rt <- read.csv('./data/csv/listings2.csv',
                         sep = ';')
class(listings2_rt[["reviews_per_month"]])
rpm <- listings2_rt[["reviews_per_month"]]
rpm_c <- as.character(rpm) # zbędne od wersji R 4.0.0
head(rpm_c)
rpm_cd <- gsub(",", ".", rpm_c)
rpm_cd
rpm_num <- as.numeric(rpm_cd)
# sapply(listings2_rt, class)
## readr
listings_rr <- read_csv("./data/csv/listings.csv")
class(listings_rr$last_review)
class(listings_rt$last_review)
listings2_rr <- read_csv2("./data/csv/listings2.csv")
head(listings2_rr$reviews_per_month)
dplyr::glimpse(listings2_rr)
## data.table
listings_dt <- fread(file = './data/csv/listings.csv')
listings_dt
sapply(listings_dt, class)
listings2_dt <- fread(file = './data/csv/listings2.csv')
# JSON
listings_js <- jsonlite::fromJSON('./data/json/listings.json')
listings_js <- mutate(listings_js,
                      last_review = as_date(last_review))
glimpse(listings_js)
# Excel
listings_excel <- read_excel('./data/excel/listings.xlsx')
glimpse(listings_excel)
# Native formats
## RDA
load("./data/native/listings.rda")
save(listings_rr, file = "listings_rr.rda",
     compression_level = 9)
load("listings_rr.rda")
## RDS
listings_rds <- readRDS("./data/native/listings.RDS")
listings_from_rds <- readRDS("./data/native/listings.RDS")
head(listings_from_rds)
?saveRDS
# # APIs
# ## To practice, create an account at https://openweathermap.org/api
# ## download.file(,
# ##     destfile = "../json/weather.json")
# json <- httr::GET(
#     "http://api.openweathermap.org/data/2.5/weather?q=London,uk&APPID=...",
#     accept_json())#,
# ##add_headers('Authorization' = 'Bearer 31232187asdsadh23187'))
# (content(json))
