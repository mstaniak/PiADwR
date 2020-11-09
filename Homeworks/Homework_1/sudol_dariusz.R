NA_ratio <- function(input_vector) {
  sum(is.na(input_vector))/length(input_vector)*100
}
### base
lapply(gas_base, NA_ratio)
### tidyverse
summarise_all(gas_tv, NA_ratio)
### data.table
gas_dt[, lapply(.SD, NA_ratio)]