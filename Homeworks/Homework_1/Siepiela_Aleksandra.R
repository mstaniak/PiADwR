library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(tidyr)
library(tidyverse)
## Import -----
# base
setwd("C:/Users/Olka/Desktop/SEMESTR 1M/Programowanie i analiza dnych w R")
gas_files <- list.files("./data", full.names = TRUE)
gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)
# tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
gas_tv
# data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt

na_proportion<-function(input_vector){
  prop<-sum(is.na(input_vector))/length(input_vector)
  prop
}

#Base 
lapply(gas_base, na_proportion)
#albo
apply(gas_base, 2, na_proportion)

#tidyverse
summarise_all(gas_tv, na_proportion)

#data.table
gas_dt[ , lapply(.SD, na_proportion)]


