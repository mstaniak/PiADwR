### LIBRARIES
libraries = c('data.table', 'tidyverse', 'rvest', 'stringr', 'stringi', 'lubridate',
              'docstring', 'jsonlite', 'httr', 'dplyr', 'RQuantLib',
              'TTR', "PerformanceAnalytics", "shiny", "shinyWidgets")
load_libraries = function(packages_vec){
  for (package in packages_vec){
    if (package %in% rownames(installed.packages())){
      library(package, character.only = TRUE)
    } else{
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

load_libraries(libraries)

get_data_from_db <- function(ticker, start_date, end_date){
  url_code <- paste(c(Sys.getenv("DB_URL"),
                      ticker,"&startDate=", start_date, "&endDate=", end_date), collapse = "")
  json <- httr::GET(url_code) 
  json_string <- content(json, 'text', encoding = "UTF-8")
  frame <- fromJSON(json_string)
  if (class(frame) == 'character'){
    return(data.table())
  } else{
    frame <- fromJSON(json_string)
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

get_all_data <- function(tickers, start_date, end_date){
  prices <- lapply(tickers, get_data_from_db, start_date, end_date)
  data.table::rbindlist(prices)
}

get_rebalance_dates = function(start_date, end_date, rebalance_frequency, period){
  rebalance_dates = seq(ymd(start_date), ymd(end_date), 
                        by = paste(rebalance_frequency, period))
  rebalance_dates = unique(adjust('Poland', rebalance_dates, 0L))
  return(rebalance_dates)
}

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

backtest_simulation = function(commission_rate, rebalance_dates, ...){
  #' dddd
  momentum_tables_history = list()
  cash_history = c()
  stocks_value_history = c()
  total_value_history = c()
  current_momentum_table = data.table(matrix(nrow=0, ncol=9))
  setnames(current_momentum_table, colnames(current_momentum_table),
           c("ticker", "momentum", "volatility", "inv_volatility", "weight",
             "price", "quantity", "value", "date"))
  old_momentum_table = data.table(matrix(nrow = 0, ncol = 9))
  setnames(old_momentum_table, colnames(old_momentum_table),
           c("ticker", "momentum", "volatility", "inv_volatility", "weight",
             "price", "quantity", "value", "date"))
  for (date_number in 1:length(rebalance_dates)){
    analysis_date = rebalance_dates[date_number]
    current_prices = full_data[ticker %in% old_momentum_table[, ticker] &
                                 date == analysis_date][order(ticker), close]
    current_stocks_value = sum(current_prices * old_momentum_table[order(ticker), 
                                                                   quantity])
    commission = current_stocks_value * commission_rate
    cash = cash + current_stocks_value - commission
    current_momentum_table = momentum_function(data = full_data, 
                                               date_of_analysis = analysis_date, 
                                               number_of_days = number_of_days, 
                                               min_momentum = min_momentum, 
                                               max_stocks = max_stocks, 
                                               min_inv_vola = min_inv_vola, 
                                               cash = cash)
    stocks_value = sum(current_momentum_table[, value])
    momentum_tables_history[[as.character(analysis_date)]] = current_momentum_table
    commission = commission_rate * stocks_value
    cash = cash - stocks_value - commission
    old_momentum_table = current_momentum_table
    cash_history = c(cash_history, cash)
    stocks_value_history = c(stocks_value_history, stocks_value)
    total_value_history = c(total_value_history, cash + stocks_value)
  }
  history_dt = data.table(date = rebalance_dates, 
                          cash = cash_history, 
                          stocks_value = stocks_value_history, 
                          total_value = total_value_history)
  return(list(equity_history = history_dt, portfolio_history = momentum_tables_history))
}

equity_plot = function(history_dt, plot_type){
  history_dt_long = pivot_longer(history_dt, cols = c('cash', 'stocks_value', 'total_value'))
  if (plot_type == 'point'){
    ggplot(history_dt_long) + geom_point(aes(x = date, y = value, col = name))
  }
  if (plot_type == 'col'){
    ggplot(history_dt_long) + geom_col(aes(x = date, y = value, fill = name), position = 'dodge')
  }
}

strat_summary = function(history_dt){
  returns = CalculateReturns(history_dt[, c("date", "total_value")])
  start = as.Date(history_dt[, date][1])
  end = as.Date(history_dt[, date][nrow(history_dt)])
  cagr = round(Return.cumulative(returns)[1], 3)
  sortino = round(SortinoRatio(returns)[1], 3)
  std_dev = round(StdDev(returns)[1], 3)
  max_dd = round(maxDrawdown(returns), 3)
  profitable_rebablances = sum(returns$total_value > 0, na.rm = TRUE)
  loss_rebalances = sum(returns$total_value < 0, na.rm = TRUE)
  summary_dt = data.table(list("start", "end", "cagr", "sortino", "std_dev", "max_dd", 
                               "profitable_rebalances", "loss_rebalances"), 
                          list(start, end, cagr, sortino, std_dev, max_dd, 
                               profitable_rebablances, loss_rebalances)
  )
  setnames(summary_dt, colnames(summary_dt), c("Nazwa", "Wartość"))
  return(summary_dt)
}

tickers = c('abe', 'acg', 'ago', 'alg', 'awm', 'aml', 'amb', 'apt', 'arh', 'atc', 'asb', 'abs', 
            'ast', '1at', 'atg', 'apr', 'bio', 'lwb', 'bbt', 'brs', 'bos', 'cmp', 'cpg', 'dbc', 
            'eex', 'ent', 'fro', 'fte', 'gop', 'gtn', 'gnb', 'glc', 'grn', 'hrp', 'ida', 'inc', 
            'irl', 'kgn', 'ksw', 'ltx', 'lbw', 'mci', 'mdg', 'mnc', 'mrb', 'mlg', 'mls', 'net', 
            'nwg', 'oat', 'opn', 'bkm', 'pcr', 'pep', 'psw', 'phn', 'pce', 'pxm', 'r22', 
            'rbw', 'rvu', 'snk', 'slv', 'ska', 'stx', 'stp', 'tim', 'tor', 'toa', 'trk', 'ulg', 
            'unt', 'vgo', 'vox', 'vrg', 'wwl', 'wlt', 'wse', 'zep', 'pbx')
  