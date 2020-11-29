library(jsonlite)
library(httr)
library(dplyr)
library(data.table)

get_data_from_db <- function(ticker, start_Date, end_Date){
  url_code <- paste(c(Sys.getenv("DB_URL"),
                      ticker,"&startDate=",start_Date, "&endDate=", end_Date), collapse = "")
  json <- httr::GET(url_code) 
  jsonString <- content(json, 'text', encoding = "UTF-8")
  frame <- fromJSON(jsonString)
  print(frame)
  if (frame == paste(c('No data for awmbetween dates:', start_Date, '-', end_Date), collapse = ' ')){
    return(data.table())
  } else{
    frame <- fromJSON(jsonString)
    result_dt = data.table(frame)
    setnames(result_dt, 'hight', 'high')
    return(result_dt)
  }
}


all_data <- function(tickers, start_Date, end_Date){
  prices <- lapply(tickers, get_data, start_Date, end_Date)
  rbindlist(prices)
}

start_Date <- "2006-02-02"
end_Date <- "2020-03-01"

a <- get_data_from_db("awm", start_Date, '2006-02-06')
a
class(a)
b <- get_data_from_db("abe", start_Date, end_Date)
b
all_data(tickers, start_Date, end_Date)
