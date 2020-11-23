library(dplyr)
library(readr)
library(data.table)

# HOMEWORK =====

# napisz funkcje, ktora liczy proporcje brakujcych wartosci dla kazdej kolumny (%)
# i zaaplikuj ja do kazdej kolumny w kazdym z trzech dialektow R

fraction_NA_2_noNA <- function(input_vector){
  (sum(is.na(input_vector)) / length(input_vector)) * 100
}

## base
apply(gas_base, 2, fraction_NA_2_noNA) #2 here indicates that apply on columns

## tidyverse
summarize_all(gas_tv, fraction_NA_2_noNA)

## data.table
gas_dt[, lapply(.SD, fraction_NA_2_noNA)] #.SD is the subset of data

