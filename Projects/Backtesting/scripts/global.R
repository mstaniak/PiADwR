library(data.table)
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(lubridate)
library(quantmod)
library(RQuantLib)
library(docstring)
library(jsonlite)
library(httr)
library(dplyr)

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
    result_dt[, `:=` (date = ymd(date),
                      open = as.numeric(open),
                      high = as.numeric(high),
                      low = as.numeric(low),
                      close = as.numeric(close),
                      volume = as.numeric(volume)
      
    )]
    return(result_dt)
  }
}

get_all_data <- function(tickers, start_Date, end_Date){
  prices <- lapply(tickers, get_data_from_db, start_Date, end_Date)
  data.table::rbindlist(prices)
}
