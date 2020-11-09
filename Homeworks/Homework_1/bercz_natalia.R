library(tidyverse)
library(data.table)

fract = function(vec){
  return(sum(is.na(vec))/length(vec))
}

sapply(gas_base, 2, fract)

summarize_all(gas_tv, fract)

gas_dt[, lapply(.SD, fract)]