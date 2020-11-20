library(data.table)
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(lubridate)
library(quantmod)
library(RQuantLib)
library(docstring)

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
    print(file)
    if(substr(file, 4, nchar(file)) == paste0("_", interval, ".csv")){
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
  Sys.sleep(120)
  return(tabela)
}

indeks = 'WIG20'
wig20 = list()
for (ticker in all_index_data[Index == indeks, Ticker]){
  wig20[[ticker]] = get_price_adjustments_info(ticker)
}
wig20_dt = rbindlist(wig20)
fwrite(wig20_dt, paste0('data/', indeks, '_price_adjustment.csv'), bom = TRUE)

indeks = 'MWIG40'
mwig40 = list()
for (ticker in all_index_data[Index == indeks, Ticker]){
  mwig40[[ticker]] = get_price_adjustments_info(ticker)
}
mwig40_dt = rbindlist(mwig40)
fwrite(mwig40_dt, paste0('data/', indeks, '_price_adjustment.csv', bom = TRUE))


indeks = 'SWIG80'
swig80 = list()
for (ticker in all_index_data[Index == indeks, Ticker]){
  swig80[[ticker]] = get_price_adjustments_info(ticker)
}
swig80_dt = rbindlist(swig80)
fwrite(swig80_dt, paste0('data/', indeks, '_price_adjustment.csv', bom = TRUE))


library(timeDate)

## Example data
dates <- as.Date("2013-01-01") + 0:364
Dates <- as.timeDate(dates)

## Extract the first business day of each month
?isBizday
bizDates <- dates[isBizday(Dates, holidays=holidayLONDON())]
firsts <- tapply(bizDates, months(bizDates), min)
sapply(firsts, function(X) as.character(as.Date(X)))
