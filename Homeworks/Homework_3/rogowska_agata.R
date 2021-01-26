# # Zadanie domowe z dzisiejszych zajęć:
# # dla danych o zanieczyszczeniu powietrza:
# #
# # dodaj do daty wylosowane godziny, minuty i sekundy. 
# # Tak powstały napis przekonwertuj z powrotem na datę.
# 
# # zaokrąglij tak utworzoną datę i oblicz typowe 
# # statystyki opisowe (średnia, median, min, max itd) dla miesiąca i roku,
# # (tzn. zaokrąglij najpierw do miesiąca, potem do roku [w dół] i 
# # oblicz statystyki według takich grup) 
# 
# # traktując datę jako 
# # a) datę
# # b) napis
# # wyciągnij z daty rok, dzień, miesiąc, godzinę, minutę, 
# # sekundę i dodaj je jako kolumny,
# 
# # W drugiej kropce chodzi o to, że mamy zaokrąglić datę do miesięcy, a potem obliczyć te statystki opisowe dla MeasuredValue grupując po miesiącu i roku?
# 
# # Import ----
# setwd("C:/Users/rogow/OneDrive/Dokumenty/UWr/Programowanie i analiza danych w R/Lab_4")
# gas_files <- list.files("./data", full.names = TRUE)
# library(data.table)
# gas_dfs_dt <- lapply(gas_files, fread)
# gas_dt <- rbindlist(gas_dfs_dt)
# library(lubridate)
# library(stringr)
# colnames(gas_dt)[c(12, 17)] <- c("Date", "MeasuredValue")

# Homework ----

# dodaj do daty wylosowane godziny, minuty i sekundy. 
gas_dt[, Date := paste(as.character(Date), 
                       paste(sample(0:23, .N, replace=TRUE), 
                             sample(0:59, .N, replace=TRUE), 
                             sample(0:59, .N, replace=TRUE), 
                             sep = ":"))]
# Tak powstały napis przekonwertuj z powrotem na datę.
gas_dt[, Date := ymd_hms(Date)]

# zaokrąglij tak utworzoną datę i oblicz typowe 
# statystyki opisowe (średnia, median, min, max itd) dla miesiąca i roku,
# (tzn. zaokrąglij najpierw do miesiąca, potem do roku [w dół] i 
# oblicz statystyki według takich grup) 

gas_dt[, Date_floor_month := floor_date(Date, unit = "month")]
gas_dt[, .(Mean = mean(MeasuredValue, na.rm = TRUE),
                 Median = median(MeasuredValue, na.rm = TRUE),
                 Min = min(MeasuredValue, na.rm = TRUE),
                 Max = max(MeasuredValue, na.rm = TRUE),
                 Sd = sd(MeasuredValue, na.rm = TRUE),
                 Var = var(MeasuredValue, na.rm = TRUE),
                 Q1 = quantile(MeasuredValue, na.rm = TRUE, probs = c(0.25)),
                 Q3 = quantile(MeasuredValue, na.rm = TRUE, probs = c(0.25))
                 ), by = c("Date_floor_month")]

gas_dt[, Date_floor_year := floor_date(Date, unit = "year")]
gas_dt[, .(Mean = mean(MeasuredValue, na.rm = TRUE),
                Median = median(MeasuredValue, na.rm = TRUE),
                Min = min(MeasuredValue, na.rm = TRUE),
                Max = max(MeasuredValue, na.rm = TRUE),
                Sd = sd(MeasuredValue, na.rm = TRUE),
                Var = var(MeasuredValue, na.rm = TRUE),
                Q1 = quantile(MeasuredValue, na.rm = TRUE, probs = c(0.25)),
                Q3 = quantile(MeasuredValue, na.rm = TRUE, probs = c(0.25))
                 ), by = c("Date_floor_year")]

# traktując datę jako 
# a) datę
# b) napis
# wyciągnij z daty rok, dzień, miesiąc, godzinę, minutę, 
# sekundę i dodaj je jako kolumny,

# data
gas_dt[, `:=`(Year = year(Date),
              Day = day(Date),
              Month = month(Date),
              Hour = hour(Date),
              Minute = minute(Date),
              Second = second(Date))]

# napis
gas_dt[, Str_Date := Date]
gas_dt[, `:=`(Str_Year = str_match(Str_Date, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")[,2], 
              Str_Day = str_match(Str_Date, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")[,4],
              Str_Month = str_match(Str_Date, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")[,3], 
              Str_Hour = str_match(Str_Date, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")[,5], 
              Str_Minute = str_match(Str_Date, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")[,6], 
              Str_Second = str_match(Str_Date, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")[,7])]


