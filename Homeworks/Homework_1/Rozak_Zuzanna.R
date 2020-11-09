
library(tidyverse)
library(dplyr)
library(data.table)


gas_files <- list.files("./data", full.names = TRUE)


f_prop <- function(x){
    sum(is.na(x))/length(x)
  }

#base
gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)
apply(gas_base,2, f_prop)

#tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
summarise_all(gas_tv, f_prop)


#data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt[,lapply(.SD, f_prop)]

