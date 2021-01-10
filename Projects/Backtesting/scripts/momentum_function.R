library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)

get_data <- function(ticker, start_Date, end_Date){
  url_code <- paste(c(Sys.getenv("DB_URL"),
                      ticker,"&startDate=",start_Date, "&endDate=", end_Date), collapse = "")
  json <- httr::GET(url_code)#, 
  jsonString <- content(json, 'text', encoding = "UTF-8")
  frame <- fromJSON(jsonString)
  result_dt = data.table(frame)
  setnames(result_dt, 'hight', 'high')
  result_dt[, `:=` (date = ymd(date), open = as.numeric(open), high = as.numeric(high), low = as.numeric(low), 
                    close = as.numeric(close), volume = as.numeric(volume))]
  return(result_dt)
}



all_data <- function(tickers, start_Date, end_Date){
  prices <- lapply(tickers, get_data, start_Date, end_Date)
  rbindlist(prices)
}

start_date <- "2010-02-02"
end_date <- "2020-12-20"
tickers <- fread("tickers.txt")$V1
tickers <- tolower(tickers)
full_data <- all_data(tickers, start_date, end_date)


is_weekend <- function(date){
  ifelse(weekdays(date) %in% c("sobota", "niedziela"), TRUE, FALSE)
}

numdays <- function(num, date){
  vec <- vector()
  while (length(vec) < num){
    if (is_weekend(as.Date(date)) == FALSE){
      vec <- c(as.Date(vec),as.Date(date))
    }
    else{
      vec <- vec
      
    }
    date = date - 1
  }
  rev(vec) 
}

 slope_of_regression <- function(data, name){
  model <- lm(log(close)~date, data = data[ticker == name, ])
  return(c(model$coefficient[2], summary(model)$r.squared))
}

score_function <- function(data, lst){
  slopevec <- c()
  rsqvec <- c()
  for (name in lst){
    slopevec <- c(slopevec,slope_of_regression(data, name)[1])
    rsqvec <- c(rsqvec, slope_of_regression(data, name)[2])
    }
  score <- ((exp(slopevec))^252 - 1) * 100 * rsqvec
  return (score)
  
}
momentum_function <- function(data, date_of_analysis, number_of_days, portfolio, min_momentum, max_stocks, min_inv_vola, ...){
  if (weekdays(date_of_analysis) == "sobota"){
    date_of_analysis <- date_of_analysis - 1
  }
  if (weekdays(date_of_analysis) == "niedziela"){
    date_of_analysis <- date_of_analysis - 2
  }
  else{
    date_of_analysis <- date_of_analysis}
  
  vec_of_days <- numdays(number_of_days, date_of_analysis)
  window <- data[date %in% vec_of_days, ]
  ticker_vec <- unique(window$ticker)
  score <- score_function(window, ticker_vec)
  momentum_tab <- data.table(ticker_vec, score)
  setnames(momentum_tab, c("ticker", "momentum"))
  momentum_tab <- momentum_tab[order(-momentum), ]
  momentum_tab <- head(momentum_tab, max_stocks)
  price <- window[ticker %in% momentum_tab$ticker & date == date_of_analysis, 
                  mean(c(open, high, low, close), na.rm = TRUE), by = ticker]
  volatility <- window[ticker %in% momentum_tab$ticker, sd((high - low)/close), by = ticker]
  tmp_table <- data.table(inner_join(price, volatility, by = "ticker"))
  setnames(tmp_table, c("ticker", "price", "volatility"))
  tmp_table[, inv_volatility := 1/tmp_table[, volatility]]
  #tmp_table[, "Inv_Volatility"] <- tmp_table[, 1/tmp_table$volatility]
  momentum_tab <- inner_join(momentum_tab, tmp_table, by = "ticker")
  momentum_tab <- momentum_tab[momentum > min_momentum & inv_volatility > min_inv_vola, ]
  weight <- momentum_tab[, inv_volatility] / sum(momentum_tab[, inv_volatility])
  value1 <- portfolio * weight
  quant <- floor(value1 / momentum_tab[, price])
  value2 <- quant * momentum_tab[, price]
  
  momentum_tab[, c("weight", "quantity", "value", "date") := 
                 .(weight, quant, value2, date_of_analysis)]

  setcolorder(momentum_tab, c("ticker", "momentum", "volatility", "inv_volatility", "weight", "price", "quantity", "value", "date"))
  critical_value <- portfolio - sum(value2)
  ind <- 1
  critical_index <- which(momentum_tab[, price] == momentum_tab[, min(price)])
  while(critical_value > momentum_tab[critical_index, "price"]){
    momentum_tab[ind, "quantity"] <- momentum_tab[ind, "quantity"] + 1
    momentum_tab[ind, "value"] <- momentum_tab[ind, "value"] + momentum_tab[ind, "price"]
    critical_value <- critical_value - momentum_tab[ind, "price"]}
    ind <- ind + 1
  
  return (momentum_tab)
}

#TEST
analysis_date <- ymd('2020-11-17')
some_date <- ymd('2014-10-13')
test1 <- momentum_function(full_data, analysis_date, 252, 20000, 40,6,12)
test2 <- momentum_function(full_data, some_date, 155, 125000, 40,6,12)


