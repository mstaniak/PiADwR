library(data.table)
library(dplyr)
all_index_data = fread(file = 'data/all_index_data.csv', header = T)
# indexes - wektor podawany w pierwszym punkcie planu
indexes = c('WIG20')
tickers = all_index_data[Index %in% indexes]$Ticker

get_stooq_data = function(tickers, interval){
  # Downloading data for specified tickers and interval from stooq.pl and saving in
  # data folder
    for (ticker in tickers){
    url = paste0("https://stooq.pl/q/d/l/?s=", ticker, "&i=", interval)
    print(url)
    data = fread(input = url, fill = TRUE)
    data = cbind(ticker, data)
    colnames(data) = c('Ticker', 'Date', 'Open', 'High', 'Low', 'Close', 'Volume')
    fwrite(data, file = paste0("data/", ticker, "_", interval, '.csv'))
  }
}

get_stooq_data(tickers, 'd')

read_all_stocks = function(interval){
  # Reading into the one DF all stocks from data folder for specified interval
  all_data = list()
  for (file in list.files("data/")){
    print(file)
    if(substr(file,4,nchar(file)) == paste0("_", interval, ".csv")){
      all_data[[substr(file,1,3)]] = fread(paste0("data/", file))
    }
  }
  return(rbindlist(all_data))
}