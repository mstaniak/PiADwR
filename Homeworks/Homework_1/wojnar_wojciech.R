library(data.table)
library(tidyverse)
gas_base = read.csv('daily_42401_2019.csv')
gas_tv = read_csv('daily_42401_2019.csv')
gas_dt = fread('daily_42401_2019.csv')
NA_counts = function(vector){
  return(sum(is.na(vector))/length(vector))
}

## base
lapply(gas_base, NA_counts)
## tidyverse
summarise_all(gas_tv, NA_counts)
## data.table
gas_dt[, lapply(.SD, NA_counts)]
