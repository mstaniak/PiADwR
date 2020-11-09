library(tidyverse)
library(data.table)
# Import ----
## base
setwd("C:/Users/rogow/OneDrive/Dokumenty/UWr/Programowanie i analiza danych w R/Lab_4")
gas_files <- list.files("./data", full.names = TRUE)
gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)
head(gas_base)
## tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
gas_tv
## data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt
# Homework ----
f <- function(vec){
  sum(is.na(vec))/length(vec)
}
## base
sapply(gas_base, f)
## tidyverse
summarise_all(gas_tv, f)
## data.table
gas_dt[, lapply(.SD, f)] 