library(tidyverse)
library(data.table)

# setwd("~/Desktop/PiADwR-academic-year-2020")

gas_files <- list.files("./data", full.names = TRUE)
# base
gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)
# tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
# data.table
gas_dfs_td <- lapply(gas_files, fread)
gas_dt <- bind_rows(gas_dfs_td)

# ZADANIE DOMOWE
## v1
proportion_NA <- function(input_vector){
  sum(is.na(input_vector)) / length(input_vector)
}

## v2
# proportion_NA <- function(input_vector){
#   mean(is.na(input_vector))
# }

### base
lapply(gas_base, proportion_NA)
### tidyverse
summarise_all(gas_tv, proportion_NA)
### data.table
gas_dt[, lapply(.SD, proportion_NA)]
