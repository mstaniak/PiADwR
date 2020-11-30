# Zadanie domowe: dla danych o zanieczyszczeniu powietrza:
# * dodaj do daty wylosowane godziny, minuty i sekundy. 
#   Tak powstały napis przekonwertuj z powrotem na datę.
# * zaokrąglij tak utworzoną datę i oblicz typowe statystyki opisowe (średnia, median, min, max itd) dla miesiąca i roku,
#   (tzn. zaokrąglij najpierw do miesiąca, potem do roku [w dół] i oblicz statystyki według takich grup)
# * traktując datę jako 
#   a) datę
#   b) napis
#   wyciągnij z daty rok, dzień, miesiąc, godzinę, minutę, sekundę i dodaj je jako kolumny.

library(readr)
library(dplyr)
library(data.table)
library(lubridate)

gas_files <- list.files("./data/gas_data", full.names = TRUE)
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)

# zadanie 1
sample_hour = function(date){
  hour = paste(sample(1:24,1), sample(0:59,1), sample(0:59,1), sep = ":")
  date <- paste(as.character(date), hour, sep = " ")
  strftime(date, format = "%Y-%m-%d %H:%M:%OS")
}

gas_dt[, 'Date Local' := lapply(gas_dt[, 'Date Local'], sample_hour)] # UWAGA: losuje ciągle tą samą godzinę dla każdej daty
## i wracamy do daty
to_date = function(chr){
  as.POSIXct(chr, format="%Y-%m-%d %H:%M:%OS")
}
gas_dt[, 'Date Local' := lapply(gas_dt[, 'Date Local'], to_date)]

# zadanie 3
gas_dt[, Year := lapply(gas_dt[, 'Date Local'], year)]
gas_dt[, Month := lapply(gas_dt[, 'Date Local'], month)]
gas_dt[, Day := lapply(gas_dt[, 'Date Local'],day)]
gas_dt[, Hour := lapply(gas_dt[, 'Date Local'],hour)]
gas_dt[, Minut := lapply(gas_dt[, 'Date Local'],minute)]
gas_dt[, Second := lapply(gas_dt[, 'Date Local'],second)]


