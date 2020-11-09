# Homework 1 -----
install.packages(c("dplyr",
                   "lubridate",
                   "readr",
                   "data.table",
                   "stringr",
                   "tidyr"))
library(readr)
library(dplyr)
library(data.table)
setwd("C:/Users/adi24/Desktop/Programowanie i analiza danych w R/dane")
gas_files <- list.files("./")
gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
proportion <- function(input_vector){
  (sum(is.na(input_vector)) / length(input_vector))
}
### base
apply(gas_base, 2, proportion) 
### tidyverse
summarize_all(gas_tv, proportion)
### data.table
gas_dt[, lapply(.SD, proportion)] 
