rebalance_dates = get_rebalance_dates(start_date = "2020-01-01", 
                                      end_date = "2020-12-31",
                                      rebalance_frequency = 1,
                                      period = "month")
commission_rate = 0.0038
number_of_days = 125
min_momentum = 40
max_stocks = 5
min_inv_vola = 20
cash = 10**4
backtest_simulation = function(commission_rate, rebalance_dates, ...){
  #' dddd
  momentum_tables_history = list()
  cash_history = c()
  stocks_value_history = c()
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
    current_prices = full_data[ticker %in% old_momentum_table[order(ticker), ticker] & date == analysis_date, close]
    current_stocks_value = sum(current_prices * old_momentum_table[, quantity])
    cash = cash + current_stocks_value * (1 - commission_rate)
    current_momentum_table = momentum_function(data = full_data, 
                                               date_of_analysis = 
                                                 rebalance_dates[date_number], 
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
  }
  summary_dt = data.table(date = rebalance_dates,
                          cash = cash_history,
                          stocks_value = stocks_value_history)
  
}


momentum_tables_history[['2020-03-02']]
full_data[ticker %in% momentum_tables_history[["2020-03-02"]][, ticker] & date == '2020-03-02']
pricess = full_data[ticker %in% old_momentum_table[, ticker] & date == rebalance_dates[3]][, close]
sum(momentum_tables_history[['2020-01-02']][, quantity] * pricess)
