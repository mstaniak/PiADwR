library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)


start_date <- "2010-02-02"
end_date <- "2020-12-20"
full_data <- get_all_data(tickers, start_date, end_date)


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
momentum_function <- function(data, date_of_analysis, number_of_days, 
                              cash, min_momentum, max_stocks, min_inv_vola, ...){
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
  volatility <- window[ticker %in% momentum_tab$ticker, 
                       sd((high - low)/close), by = ticker]
  tmp_table <- data.table(inner_join(price, volatility, by = "ticker"))
  setnames(tmp_table, c("ticker", "price", "volatility"))
  tmp_table[, inv_volatility := 1/volatility]
  momentum_tab <- data.table(inner_join(momentum_tab, tmp_table, by = "ticker"))
  momentum_tab[, weight := inv_volatility/sum(inv_volatility)]
  momentum_tab <- momentum_tab[momentum > min_momentum 
                               & inv_volatility > min_inv_vola, ]
  momentum_tab[, quantity := floor((cash * weight) / price)]
  momentum_tab[, value := quantity * price]
  momentum_tab[, date := date_of_analysis]
  setcolorder(momentum_tab, c("ticker", "momentum", "volatility", 
                              "inv_volatility", "weight", "price", 
                              "quantity", "value", "date"))
  return (momentum_tab)
}

#TEST
analysis_date <- ymd('2020-11-17')
some_date <- ymd('2014-10-13')
test11 <- momentum_function(full_data, analysis_date, 252, 20000, 40,6,12)
test2 <- momentum_function(full_data, some_date, 155, 125000, 40,6,12)


