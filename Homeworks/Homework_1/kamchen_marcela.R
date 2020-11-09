fraqNA <- function(vector) {
  mean(is.na(vector))
}

#base

apply(gas_base,2,fraqNA)
#lub tak:
lapply(gas_base, fraqNA)

#tidyverse

summarise_all(gas_tv,fraqNA)

#data.table

gas_dt[, lapply(.SD, fraqNA)]