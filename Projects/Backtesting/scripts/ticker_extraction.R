library(stringr)
library(tidyverse)
library(tabulizer)

tickers <- extract_tables('tickery.pdf')
tickers[[2]]
tickery = data.frame(matrix(0,0,2))

for (i in 1:20){
  tickery = rbind(tickery,data.frame(tickers[[i]][3:nrow(tickers[[i]]),2],tickers[[i]][3:nrow(tickers[[i]]),3]))
}

names(tickery) = c('Name','Ticker')

write.csv(tickery,'tickers_pl.csv',fileEncoding = 'UTF-8')
