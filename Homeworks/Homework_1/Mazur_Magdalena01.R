
setwd("D:/KRUSZYN/PiADwR/PiADwR-academic-year-2020/Materials/Lab_04")

library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(tidyr)

#base
gas_files = list.files("./data", full.names = TRUE)
gas_dfs = lapply(gas_files, read.csv)
gas_base = do.call("rbind", gas_dfs)

# tidyverse
gas_dfs_tv = lapply(gas_files, read_csv)
gas_tv = bind_rows(gas_dfs_tv)

# data.table
gas_dfs_dt = lapply(gas_files, fread)
gas_dt = rbindlist(gas_dfs_dt)

missingNA_vec = function(vector){
  sum(is.na(vector))/length(vector)
}

sapply(gas_base, missingNA_vec)
summarise_all(gas_tv, missingNA_vec)
gas_dt[, lapply(.SD, missingNA_vec)]
