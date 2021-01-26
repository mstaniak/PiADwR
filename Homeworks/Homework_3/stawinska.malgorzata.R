

#dodaj do daty wylosowane godziny, minuty i sekundy. 
gas_dt[, Date := paste(as.character(Date), paste(sample(0:23, .N, replace=TRUE), sample(0:59, .N, replace=TRUE), sample(0:59, .N, replace=TRUE), sep = ":"))]
#Tak powstały napis przekonwertuj z powrotem na datę.
gas_dt[, Date := ymd_hms(Date)]

#zaokrąglij tak utworzoną datę
#oblicz typowe statystyki opisowe (średnia, median, min, max itd) dla miesiąca i roku,

#MIESIAC
gas_dt_m = gas_dt[, Date := floor_date(Date, unit = "month")]
gas_dt_m[, .(Mean = mean(MeasuredValue, na.rm = TRUE),
             Median = median(MeasuredValue, na.rm = TRUE),
             Min = min(MeasuredValue, na.rm = TRUE),
             Max = max(MeasuredValue, na.rm = TRUE),
             SD = sd(MeasuredValue, na.rm = TRUE)), by = c("Date")]

#ROK
gas_dt_y = gas_dt[, Date := floor_date(Date, unit = "year")]
gas_dt_y[, .(Mean = mean(MeasuredValue, na.rm = TRUE), 
             Median = median(MeasuredValue, na.rm = TRUE),
             Min = min(MeasuredValue, na.rm = TRUE),
             Max = max(MeasuredValue, na.rm = TRUE),
             SD = sd(MeasuredValue, na.rm = TRUE)), by = c("Date")]

#traktując datę jako 
#a) datę
#b) napis
#wyciągnij z daty rok, dzień, miesiąc, godzinę, minutę, sekundę i dodaj je jako kolumny,

#a
gas_dt[, `:=`(Year = year(Date),
              Month = month(Date),
              Day = day(Date),
              Hour = hour(Date),
              Minute = minute(Date),
              Second = second(Date))]

#b
dates = as.character(gas_dt[, Date])
split_dates = str_match(dates, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")
gas_dt_char = gas_dt[, `:=`(Year = split_dates[,2], 
                            Month = split_dates[,3], 
                            Day = split_dates[,4], 
                            Hour = split_dates[,5], 
                            Minute = split_dates[,6], 
                            Second = split_dates[,7])]
