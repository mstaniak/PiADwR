# zadanie domowe ##################
# w każdym z tych trzech dialektów R-owych, napisać funkcje, która liczy proporcje brakujących wartości dla każdej kolumny
rate_NA <- function(input_vector) {
  sum(is.na(input_vector))/length(input_vector)}

### base
lapply(gas_base, rate_NA) 
### tidyverse
summarise_all(gas_tv, rate_NA)
### data.table 
gas_dt[, lapply(.SD, rate_NA)]