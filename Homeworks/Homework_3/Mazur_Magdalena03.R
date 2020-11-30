library(dplyr)
library(lubridate)
library(stringr)

#1
gas_dt[, Date := paste(Date, paste(sample(0:23), sample(0:59), sample(0:59), sep = ":"))]
gas_dt$Date = ymd_hms(gas_dt$Date)

#2
gas_dt[, `:=` (Date = floor_date(Date, unit = "month"), Mean_Value = mean(MeasuredValue, na.rm = TRUE), Median_Value = median(MeasuredValue, na.rm = TRUE), Min_Value = min(MeasuredValue, na.rm = TRUE), Max_Value = max(MeasuredValue, na.rm = TRUE)), by = c("Date")]
gas_dt[, `:=` (Date = floor_date(Date, unit = "year"), Mean_Value = mean(MeasuredValue, na.rm = TRUE), Median_Value = median(MeasuredValue, na.rm = TRUE), Min_Value = min(MeasuredValue, na.rm = TRUE), Max_Value = max(MeasuredValue, na.rm = TRUE)), by = c("Date")]

#3
gas_date_string = gas_dt
gas_date_string
Dates = str_match(gas_date_string$Date, "([0-9]{4})\\-([0-9]{2})\\-([0-9]{2}) ([0-9]{1,2})\\:([0-9]{1,2})\\:([0-9]{1,2})")
gas_date_string[, `:=` (Year = Dates[,2], Month = Dates[,3], Day = Dates[,4], Hour = Dates[,5], Minute = Dates[,6], Second = Dates[,7])]

gas_dt$Date = ymd_hms(gas_dt$Date)
gas_date_date = gas_dt
gas_date_date[, `:=` (Year = year(Date), Month = month(Date), Day = day(Date), Hour = hour(Date), Minute = minute(Date), Second = second(Date))]
