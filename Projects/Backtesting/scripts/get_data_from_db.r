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
  if (class(frame) == 'character'){
    return(data.table())
  } else{
    frame <- fromJSON(jsonString)
    result_dt = data.table(frame)
    setnames(result_dt, 'hight', 'high')
    return(result_dt)
  }
}

get_all_data <- function(tickers, start_Date, end_Date){
  prices <- lapply(tickers, get_data, start_Date, end_Date)
  data.table::rbindlist(prices)
}

start_Date <- "2006-02-02"
end_Date <- "2020-03-01"

a <- get_data_from_db("AGO", '2020-10-01', '2020-10-31')
a
all_index_data[Ticker == toupper('ago')]
class(a)
b <- get_data_from_db("abe", start_Date, end_Date)
b
all_data(tickers, start_Date, end_Date)
