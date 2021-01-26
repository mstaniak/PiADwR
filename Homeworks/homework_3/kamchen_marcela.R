
library('dplyr')
library('lubridate')
library('stringr')

#zadanie1
#przekonwertowanie z napisu na date
class(gas_dt[,Date])
gas_dt[, Date := paste(Date, paste(sample(0:23), sample(0:59) ,sample(0:59),sep=":"))]
class(gas_dt[,Date])

gas_dt[, Date := ymd_hms(Date)]

#zadanie2


#zaokrąglanie do miesiaca [w dol]
gas_dt_m <- gas_dt[, Date := floor_date(Date, unit = "month")]
gas_dt_m[, ':='(MinValue = min(MeasuredValue), MaxValue = max(MeasuredValue), MeanValue = mean(MeasuredValue), MedianValue = median(MeasuredValue), SDValue = sd(MeasuredValue)), by = "Date"]

#zaokraglanie do roku
gas_dt_y <- gas_dt[, Date := floor_date(Date, unit = "year")]
gas_dt_y[, ':='(MinValue = min(MeasuredValue), MaxValue = max(MeasuredValue), MeanValue = mean(MeasuredValue), MedianValue = median(MeasuredValue), SDValue = sd(MeasuredValue)), by = "Date"]


#zadanie3
#Wyciaganie roku, miesiaca, dnia, godziny, minuty, sekundy do osobnych kolumn (jako data)

gas_dt1 <- gas_dt[, ':='(Year = year(Date), Month = month(Date), Day = day(Date), 
              Hour = hour(Date), Minute = minute(Date), Second = second(Date))]

#Wyciaganie roku, miesiaca, dnia, godziny, minuty, sekundy do osobnych kolumn (jako napis)
dates <- gas_dt[,Date]
dates <- as.character(dates)
splited_dates <- str_match(dates, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")

gas_dt2 <- gas_dt[, ':='(Year = splited_dates[,2], Month = splited_dates[,3], Day = splited_dates[,4], 
                         Hour = splited_dates[,5], Minute = splited_dates[,6], Second = splited_dates[,7])]


