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

get_stooq_data = function(tickers, interval){
  # Downloading data for specified tickers and interval from stooq.pl and saving in
  # data folder
  # NIE PUSHOWAĆ DANYCH
  for (ticker in tickers){
    url = paste0("https://stooq.pl/q/d/l/?s=", ticker, "&i=", interval)
    print(url)
    data = fread(input = url, fill = TRUE)
    data = cbind(ticker, data)
    colnames(data) = c('Ticker', 'Date', 'Open', 'High', 'Low', 'Close', 'Volume')
    fwrite(data, file = paste0("data/", ticker, "_", interval, '.csv'))
  }
}

read_all_stocks = function(interval){
  # Reading into the one data.table, all stocks from data folder for specified interval
  all_data = list()
  for (file in list.files("data/")){
    if(substr(file, 4, nchar(file)) == paste0("_", interval, ".csv")){
      print(file)
      all_data[[substr(file, 1, 3)]] = fread(paste0("data/", file))
    }
  }
  return(rbindlist(all_data))
}

dividend_adjustment = function(stock_info_dt){
  # Koryguje o dywidendy/splity/prawa poboru obiekt data.table zawierający dane jednej spółki w oparciu
  # o dane ze stooq.pl
  all_index_data = fread('data/all_index_data.csv')
  setnames(stock_info_dt, 
           names(stock_info_dt), 
           c('Name', 'Date', 'Open', 'High', 'Low', 'Close', 'Volume'))
  print(stock_info_dt[, Name][1])
  print(nchar(stock_info_dt[, Name][1]) > 3)
  if (nchar(stock_info_dt[, Name][1]) > 3){
    name = stock_info_dt[, Name][1]
    ticker = all_index_data[Name == name, Ticker][1]
  } else if (nchar(stock_info_dt[, Name][1]) == 3){
    ticker = stock_info_dt[, Name][1]
    name = all_index_data[Ticker == ticker, Name][1]
  }
  print(c(ticker, name))
  stock_info_dt[, `:=` (Name = name,
                        Ticker = ticker,
                        Date = ymd(Date),
                        Open = as.numeric(Open),
                        High = as.numeric(High),
                        Low = as.numeric(Low),
                        Close = as.numeric(Close),
                        Volume = as.numeric(Volume))]
  
  url = paste0('https://stooq.pl/q/m/?s=', ticker)
  tabela = read_html(url) %>%
    html_nodes(xpath = '//*[@id="fth1"]') %>%
    html_table(fill=TRUE)
  tabela = tabela[[1]][, c(1, 4)]
  
  names(tabela) = c('Date', 'Dzielnik')
  tabela$Date = dmy(tabela$Date)
  
  for (row in nrow(tabela):1){
    dzielnik = tabela[row, 'Dzielnik']
    stock_info_dt[Date < tabela[row, 'Date'], 
                  `:=` (Open = Open/dzielnik,
                        High = High/dzielnik,
                        Low = Low/dzielnik,
                        Close = Close/dzielnik)]
  }
  return(stock_info_dt)
}

get_price_adjustments_info = function(ticker){
  all_index_data = fread('data/all_index_data.csv', header = T)
  name = all_index_data[Ticker == ticker, Name][1]
  url = paste0('https://stooq.pl/q/m/?s=', ticker, collapse = '')
  tabela = read_html(url) %>%
    html_nodes(xpath = '//*[@id="fth1"]') %>%
    html_table(fill=TRUE)
  if (length(tabela) > 0){
    tabela = tabela[[1]][, c(1, 2, 4)]
    tabela = data.table(tabela)
    setnames(tabela, names(tabela), c('Date', 'Zdarzenie', 'Dzielnik'))
    print(tabela[, Zdarzenie])
    if (grepl('Przekroczony', tabela[, Zdarzenie])){
      stop()
    }
    tabela[, Date := dmy(tabela$Date)]
    tabela[, `:=` (Ticker = ticker,
                   Name = name)]
    print(tabela)
  }
  Sys.sleep(300)
  return(tabela)
}

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