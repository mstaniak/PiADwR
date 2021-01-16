### LIBRARIES
libraries = c('data.table', 'tidyverse', 'rvest', 'stringr', 'stringi', 'lubridate', 
              'quantmod', 'RQuantLib', 'docstring', 'jsonlite', 'httr', 'dplyr', 
              'TTR', 'quantstrat')
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

tickers = c('abe', 'acg', 'ago', 'alg', 'awm', 'aml', 'amb', 'apt', 'arh', 'atc', 'asb', 'abs', 
            'ast', '1at', 'atg', 'apr', 'bio', 'lwb', 'bbt', 'brs', 'bos', 'cmp', 'cpg', 'dbc', 
            'eex', 'ent', 'fro', 'fte', 'gop', 'gtn', 'gnb', 'glc', 'grn', 'hrp', 'ida', 'inc', 
            'irl', 'kgn', 'ksw', 'ltx', 'lbw', 'mci', 'mdg', 'mnc', 'mrb', 'mlg', 'mls', 'net', 
            'nwg', 'oat', 'opn', 'bkm', 'pcr', 'pep', 'psw', 'phn', 'pce', 'pxm', 'r22', 
            'rbw', 'rvu', 'snk', 'slv', 'ska', 'stx', 'stp', 'tim', 'tor', 'toa', 'trk', 'ulg', 
            'unt', 'vgo', 'vox', 'vrg', 'wwl', 'wlt', 'wse', 'zep', 'pbx')
  