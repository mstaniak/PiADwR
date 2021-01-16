start_date <- "2020-01-02"
end_date <- "2020-12-30"
rebalance_dates = get_rebalance_dates(start_date = start_date, 
                                      end_date = end_date,
                                      rebalance_frequency = 1,
                                      period = "month")
commission_rate = 0.0038
number_of_days = 30
min_momentum = 40
max_stocks = 5
min_inv_vola = 20
cash = 10**4
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
    # full_data[ticker=='mls', 
    #           date][which(abs(analysis_date - full_data[ticker=='mls', date]) == 
    #                         min(abs(analysis_date - full_data[ticker=='mls', date])))]
    print(analysis_date)
    print(full_data[ticker %in% old_momentum_table[, ticker] &
                      date == analysis_date][order(ticker)])
    print(old_momentum_table)
    current_stocks_value = sum(current_prices * old_momentum_table[order(ticker), 
                                                                   quantity])
    cash = cash + current_stocks_value * (1 - commission_rate)
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
  return(history_dt)
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

equity_plot(history_dt, 'col')

strat_summary = function(history_dt){
 CAGR = history_dt[] 
}

full_data[ticker == 'mls']


momentum_tables_history[['2020-03-02']]
full_data[ticker %in% momentum_tables_history[["2020-03-02"]][, ticker] & date == '2020-03-02']
pricess = full_data[ticker %in% old_momentum_table[, ticker] & date == rebalance_dates[3]][, close]
sum(momentum_tables_history[['2020-01-02']][, quantity] * pricess)

momentum_tables_history[[as.character(rebalance_dates[1])]]
momentum_tables_history[[as.character(rebalance_dates[2])]]
momentum_tables_history[[as.character(rebalance_dates[3])]]
momentum_tables_history[[as.character(rebalance_dates[4])]]
momentum_tables_history[[as.character(rebalance_dates[5])]]
momentum_tables_history[[as.character(rebalance_dates[6])]]
momentum_tables_history[[as.character(rebalance_dates[7])]]
momentum_tables_history[[as.character(rebalance_dates[8])]]
momentum_tables_history[[as.character(rebalance_dates[9])]]
momentum_tables_history[[as.character(rebalance_dates[10])]]
momentum_tables_history[[as.character(rebalance_dates[11])]]
momentum_tables_history[[as.character(rebalance_dates[12])]]



sum(momentum_tables_history[[as.character(rebalance_dates[3])]][, value])
cash_history[3]
