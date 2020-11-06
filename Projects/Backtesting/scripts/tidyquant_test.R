# install.packages('tidyquant')
# https://en.wikipedia.org/wiki/List_of_S%26P_500_companies
library(tidyquant)
aapl <- tq_get('AAPL',
               from = "2017-01-01",
               to = "2020-10-28",
               get = "stock.prices")
