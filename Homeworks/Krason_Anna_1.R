#1 zadanie domowe

library("dplyr")
library("data.table")

#setwd('/home/datahikerxps/Desktop/progr R')
co_19 <-read.csv('daily_42101_2019.csv')
co_20 <- read.csv('daily_42101_2020.csv')
ozone_19 <- read.csv('daily_44201_2019.csv')
ozone_20 <- read.csv('daily_44201_2020.csv')

gas <- rbind(co_19, co_20, ozone_19, ozone_20)
gas_DT <- data.table(gas)

prop <- function(dane){
  licz <- 0
  n <- length(dane)
  for(i in dane){
    if(is.na(i) == TRUE) {licz = licz + 1}
  }
  licz/n
}

lapply(gas, prop)

summarise_all(gas, prop)

gas_DT[, lapply(.SD, prop)]

