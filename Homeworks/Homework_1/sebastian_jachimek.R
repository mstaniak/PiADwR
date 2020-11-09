###HOMEWORK IS AT THE VERY END OF THIS FILE. "Detour. Functions in R" and "Column operations" ARE HERE 
###ADDITIONALLY, RUNNING THEM IS NOT NECESSARY TO CHECK IF HOMEWORK IS DONE CORRECTLY. 


library(data.table)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

gas_files <- list.files("./PiADwR/data", full.names = TRUE)

##base ----
gas_dfs <- lapply(gas_files, read.csv)
gas_base = do.call("rbind", gas_dfs)
##tidyverse ----

gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
k <- 1:4
class(gas_dfs_tv[[2]]$`Method Code`) <- "numeric"
class(gas_dfs_tv[[3]]$`Method Code`) <- "numeric"
gas_tv <- bind_rows(gas_dfs_tv)

##data.table ---- 
gas_dfs_dt <- lapply(gas_files, fread)

gas_dt <- rbindlist(gas_dfs_dt)

#Detour. Functions in R ----

mean(gas_dt$`1st Max Hour`)
myread <- read.csv

mean_noNA <- function(numeric_vector){
  mean(numeric_vector, na.rm = TRUE)
}

mean_noNA2 <- function(numeric_vector, ...){
  mean(numeric_vector, na.rm = TRUE, ...)
}

mean_noNA2(gas_dt$`1st Max Hour`, trim = 0.07)
mean_noNA(gas_dt$`1st Max Hour`)

num_unique <- function(input_vector){
  length(unique(input_vector))
}

num_unique_no_NA <- function(input_vector){
  sum(!is.na(unique(input_vector)))
}

num_unique(gas_dt$`Method Code`)
num_unique_no_NA(gas_dt$`Method Code`)

#Column operations ---- 
## Summarize all / selected columns
###base
apply(gas_base, 2, num_unique_no_NA)
###tidyverse
###datatable
gas_dt[, lapply(.SD, num_unique_no_NA)] #.SD - subset of data


##Apply to all / selected columns



##Homework ----

percent_of_na <- function(vector){
  sum(is.na(vector)) / length(vector)
}

#base

sapply(gas_base, percent_of_na)

#tidyverse

summarise_all(gas_tv, percent_of_na)


#data.table

gas_dt[, sapply(.SD, percent_of_na)]
