library(readr)
library(dplyr)
library(tidyverse)
library(data.table)
#Import
##base
gas_files <- list.files("C:/Users/Michal/Desktop/Programowanie i analiza danych w R/lab/data", full.names = TRUE)
gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)

##tidyverse
gas_dfs_tv <- lapply(gas_files, read.csv)
gas_tv <- bind_rows(gas_dfs_tv)

##datatable
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)

proportion_Na <- function(input_vector){
  return( sum(is.na(input_vector))/length(input_vector))
}

###base
base_summarise <- function(data, fun){
  return( sapply(data, fun))
}

base_summarise(gas_base, proportion_Na)


###tidyverse
tidyverse_summarise <- function(data, fun){
  return( summarise_all(data, fun) )
}

tidyverse_summarise(gas_tv, proportion_Na)


###data_table
datatable_summarise <- function(data, fun){
  data[, lapply(.SD, fun)]
}

datatable_summarise(gas_dt, proportion_Na
