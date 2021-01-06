library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)

get_data <- function(ticker, start_Date, end_Date){
  url_code <- paste(c("https://tahkr5dywl.execute-api.us-east-2.amazonaws.com/default/getStocks?ticker=",
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

start_Date <- "2010-02-02"
end_Date <- "2020-12-20"


tickers <- fread("tickers.txt")$V1
tickers <- tolower(tickers)

AllData <- all_data(tickers, start_Date, end_Date)


is_weekend <- function(date){
  ifelse(weekdays(date) %in% c("sobota", "niedziela"), TRUE, FALSE)
}


numdays <- function(n, date){
  vec <- vector()
  while (length(vec) < n){
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
MomentumFunction <- function(data, date1, nums, portfolio, min_momentum, max_stocks, min_inv_vola, ...){
  if (weekdays(date1) == "sobota"){
    date1 <- date1- 1
  }
  if (weekdays(date1) == "niedziela"){
    date1 <- date1 - 2
  }
  else{
    date1 <- date1}
  VecOfDays <- numdays(nums, date1)
  Window <- data[date %in% VecOfDays, ]
  TickerList <- unique(Window$ticker)
  Score <- score_function(Window, TickerList)
  MomentumTab <- data.table(TickerList, Score)
  setnames(MomentumTab, c("ticker", "Momentum"))
  MomentumTab <- MomentumTab[order(-Momentum), ]
  MomentumTab <- head(MomentumTab, max_stocks)
  Price_full <- Window[ticker %in% MomentumTab$ticker & date == date1, mean(c(open, high, low, close), na.rm = TRUE), by = ticker]
  Volatility <- Window[ticker %in% MomentumTab$ticker, sd((high - low)/close), by = ticker]
  tmpTable <- inner_join(Price_full, Volatility, by = "ticker")
  setnames(tmpTable, c("ticker", "Price", "Volatility"))
  tmpTable[, "Inv Volatility"] <- tmpTable[, 1/tmpTable$Volatility]
  MomentumTab <- inner_join(MomentumTab, tmpTable, by = "ticker")
  MomentumTab <- MomentumTab[Momentum > min_momentum & `Inv Volatility` > min_inv_vola, ]
  Weight <- MomentumTab$`Inv Volatility`/sum(MomentumTab$`Inv Volatility`)
  Value1 <- portfolio * Weight
  Quant <- Value1 / MomentumTab$Price
  Quant2 <- floor(Quant)
  Value2 <- Quant2 * MomentumTab$Price
  MomentumTab[, c("Weight", "Quantity", "Value", "Date") := .(Weight, Quant2, Value2, date1)]
  setcolorder(MomentumTab, c("ticker", "Momentum", "Volatility", "Inv Volatility", "Weight", "Price", "Quantity", "Value", "Date"))
  
  CriticalValue <- portfolio - sum(Value2)
  ind <- 1
  critical_index <- which(MomentumTab$Price == min(MomentumTab$Price))
  while(CriticalValue > MomentumTab[critical_index, "Price"]){
    MomentumTab[ind, "Quantity"] <- MomentumTab[ind, "Quantity"] + 1
    MomentumTab[ind, "Value"] <- MomentumTab[ind, "Value"] + MomentumTab[ind, "Price"]
    CriticalValue <- CriticalValue - MomentumTab[ind, "Price"]}
    ind <- ind + 1
  
  return (MomentumTab)
}

analysis_date <- ymd('2020-11-17')
some_date <- ymd('2014-10-13')

test1 <- MomentumFunction(AllData, analysis_date, 252, 20000, 40,6,12)
test2 <- MomentumFunction(AllData, some_date, 155, 125000, 40,6,12)
