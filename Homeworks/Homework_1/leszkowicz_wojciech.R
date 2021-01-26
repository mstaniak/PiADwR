num_of_NA <- function(v) {
  sum(is.na(v))/length(v)
}

# base
lapply(gas_base, num_of_NA)

# tidyverse
summarise_all(gas_tv, num_of_NA)

# data.table
gas_dt[, lapply(.SD, num_of_NA)]