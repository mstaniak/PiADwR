library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(tidyr)
library(tidyverse)

setwd("C:/Users/Olka/Desktop/SEMESTR 1M/Programowanie i analiza dnych w R")
gas_files <- list.files("./data", full.names = TRUE)

# data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
View(gas_dt)  

setnames(gas_dt, colnames(gas_dt[, list(`State Name`, `County Name`, `City Name`, `Local Site Name`,
                                        `Date Local`, `Parameter Name`, `Sample Duration`,`Arithmetic Mean`)]),
                                         c("State", "County", "City", "Site", "Date","Pollutant", "SampleDuration",
                                        "MeasuredValue"))


#Wszystkie polecenia wykonałam najpierw na kolumnie Date, a później na kolumnie Date of Last Change.

#Dodaj do daty wylosowane godziny, minuty i sekundy. Tak powstały napis przekonwertuj z powrotem na datę.

dim_gas_dt <- dim(gas_dt)
dim_gas_dt  # 797802 x 29

datetimes <- paste(
  as.character(gas_dt$Date),
  paste(sample(0:23, 797802, replace = TRUE), sample(0:59, 797802, replace = TRUE), sample(0:59, 797802, replace = TRUE),
        sep = ":"))
datetimes <- ymd_hms(datetimes, tz = "UTC")
gas_dt[ ,Date := datetimes]


datetimes2 <- paste(
  as.character(gas_dt$`Date of Last Change`),
  paste(sample(0:23, 797802, replace = TRUE), sample(0:59, 797802, replace = TRUE), sample(0:59, 797802, replace = TRUE),
        sep = ":"))
datetimes2 <- ymd_hms(datetimes2, tz = "UTC")
gas_dt[ ,`Date of Last Change` := datetimes2]

#Zaokrąglij tak utworzoną datę i oblicz typowe statystyki opisowe (średnia, median, min, max itd) dla miesiąca i roku,
#(tzn. zaokrąglij najpierw do miesiąca, potem do roku [w dół] i oblicz statystyki według takich grup)

#Dla Date
round_to_month <- floor_date(gas_dt$Date, unit = "month")
round_to_year <- floor_date(gas_dt$Date, unit = "year")

gas_dt[ , .(Min = summary(MeasuredValue)[1], Qu_1st = summary(MeasuredValue)[2], 
            Median = summary(MeasuredValue)[3], Mean = summary(MeasuredValue)[4],
            Qu_3rd = summary(MeasuredValue)[5], Max = summary(MeasuredValue)[6]),
            by = month(round_to_month)][order(month)]


gas_dt[ , .(Min = summary(MeasuredValue)[1], Qu_1st = summary(MeasuredValue)[2], 
            Median = summary(MeasuredValue)[3], Mean = summary(MeasuredValue)[4],
            Qu_3rd = summary(MeasuredValue)[5], Max = summary(MeasuredValue)[6]),
            by = year(round_to_year)][order(year)]

#Alternatywna wersja przy użyciu aggregate
aggregate(gas_dt$MeasuredValue ~ month(round_to_month), gas_dt, FUN = summary)
aggregate(gas_dt$MeasuredValue ~ year(round_to_year), gas_dt, FUN = summary)


#Dla Date of Last Change
round_to_month2 <- floor_date(gas_dt$`Date of Last Change`, unit = "month")
round_to_year2 <- floor_date(gas_dt$`Date of Last Change`, unit = "year")

gas_dt[ , .(Min = summary(MeasuredValue)[1], Qu_1st = summary(MeasuredValue)[2], 
            Median = summary(MeasuredValue)[3], Mean = summary(MeasuredValue)[4],
            Qu_3rd = summary(MeasuredValue)[5], Max = summary(MeasuredValue)[6]),
            by = month(round_to_month2)][order(month)]


gas_dt[ , .(Min = summary(MeasuredValue)[1], Qu_1st = summary(MeasuredValue)[2], 
            Median = summary(MeasuredValue)[3], Mean = summary(MeasuredValue)[4],
            Qu_3rd = summary(MeasuredValue)[5], Max = summary(MeasuredValue)[6]), 
            by = year(round_to_year2)][order(year)]

#Alternatywna wersja przy użyciu aggregate
aggregate(gas_dt$MeasuredValue ~ month(round_to_month2), gas_dt, FUN = summary)
aggregate(gas_dt$MeasuredValue ~ year(round_to_year2), gas_dt, FUN = summary)

#Traktując datę jako 
#a) datę
#b) napis
#wyciągnij z daty rok, dzień, miesiąc, godzinę, minutę, sekundę i dodaj je jako kolumny.

#a)
data <- gas_dt$Date
Y <- year(data)
M <- month(data)
D <- day(data)
H <- hour(data)
Mi <- minute(data)
S <- second(data)

gas_dt[, `:=`(`Date: Year` = Y, `Date: Month` = M, `Date: Day` = D, 
              `Date: Hour` = H, `Date: Minute` = Mi, `Date: Second` = S)] 

data2 <- gas_dt$`Date of Last Change`
Y2 <- year(data2)
M2 <- month(data2)
D2 <- day(data2)
H2 <- hour(data2)
Mi2 <- minute(data2)
S2 <- second(data2)

gas_dt[, `:=`(`Date of Last Change: Year` = Y2, `Date of Last Change: Month` = M2,
              `Date of Last Change: Day` = D2, `Date of Last Change: Hour` = H2,
              `Date of Last Change: Minute` = Mi2, `Date of Last Change: Second` = S2)] 

#b)
dates_table<-str_match(data, "([0-9\\.]+)-([0-9\\.]+)-([0-9\\.]+) ([0-9\\.]+):([0-9\\.]+):([0-9\\.]+)")
gas_dt[, `:=`(`Str Date: Year` = dates_table[ ,2], `Str Date: Month` = dates_table[ ,3], `Str Date: Day` = dates_table[ ,4], 
              `Str Date: Hour` = dates_table[ ,5], `Str Date: Minute` = dates_table[ ,6], `Str Date: Second` = dates_table[ ,7])] 

dates_table2<-str_match(data2, "([0-9\\.]+)-([0-9\\.]+)-([0-9\\.]+) ([0-9\\.]+):([0-9\\.]+):([0-9\\.]+)")
gas_dt[, `:=`(`Str Date of Last Change: Year` = dates_table2[ ,2], `Str Date of Last Change: Month` = dates_table2[ ,3],
              `Str Date of Last Change: Day` = dates_table2[ ,4], `Str Date of Last Change: Hour` = dates_table2[ ,5],
              `Str Date of Last Change: Minute` = dates_table2[ ,6], `Str Date of Last Change: Second` = dates_table2[ ,7])] 
