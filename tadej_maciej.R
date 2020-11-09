install.packages(c("dplyr",
                   "lubridate",
                   "readr",
                   "data.table",
                   "stringr",
                   "tidyr"))
library(readr)
library(dplyr)
library(data.table)
# Import ----

gas_files <- list.files(, full.names = TRUE, pattern = ".csv")

## base

gas_dfs <- lapply(gas_files, read.csv)

gas_base <- do.call("rbind", gas_dfs)

## tidyverse

gas_dfs_tv <- lapply(gas_files, read_csv)

for (i in 1:4){
  gas_dfs_tv[[i]]$'Method Code' <- as.character(gas_dfs_tv[[i]]$'Method Code')
}

gas_tv <- bind_rows(gas_dfs_tv)


## data.table

gas_dfs_dt <- lapply(gas_files, fread)

gas_dt <- rbindlist(gas_dfs_dt)

#funkcja wyliczająca % wartości NA na danej kolumnie

countNA <- function(vec){
  sum(is.na(vec))/length(vec)
}


lapply(gas_base, countNA)
lapply(gas_dt, countNA)
summarise_all(gas_tv, countNA)
