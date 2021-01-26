library(dplyr)
library(readr)
library(stringr)
library(data.table)
library(tidyr)

###import
gas_files <- list.files("./data")
gas_base <- do.call("rbind", gas_file)

## tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <-bind_rows(gas_dfs)

## data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs)

col_na <- function(vec){
  length(vec[is.na(vec)])/length(vec)
}

### base
apply(gas_base, 2, col_na)
lapply(gas_base, col_na)

### tidyverse
summarize_all(gas_tv, col_na)

### data.table
gas_dt[, lapply(.SD, col_na)]
