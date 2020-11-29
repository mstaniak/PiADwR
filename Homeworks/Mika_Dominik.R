
#Zad.1#


n <- length(gas_dt[, Date])
gas_dt[, Date:=as.character(Date)]



datetimes <- paste(
  gas_dt[, Date],
  paste(sample(0:23, n, replace = TRUE), sample(0:59, n, replace = TRUE), sample(0:59, n, replace = TRUE),
        sep = ":"))

datetimes <- ymd_hms(datetimes, tz = "UTC")
gas_dt[, Date:=datetimes]
####


#Zad.2#
gas_dt[, Date := floor_date(Date, unit = "month")]

gas_dt[, .(
  mean_val = mean(MeasuredValue),
  median_val = median(MeasuredValue),
  min_val = min(MeasuredValue),
  max_val = max(MeasuredValue)),
  by = .(year(Date), month(Date))]
####


#Zad.3#
#a)#
gas_dt[, `:=`(date_year = year(Date),
              date_month = month(Date),
              date_day = day(Date),
              date_hour = hour(Date), 
              date_minute = minute(Date),
              date_second = second(Date))]
####

#b)#

gas_dt[, Date:=as.character(Date)]

datetimes <- paste(
  gas_dt[, Date],
  paste(sample(0:23, n, replace = TRUE), sample(0:59, n, replace = TRUE), sample(0:59, n, replace = TRUE),
        sep = ":"))


dates <- str_match(datetimes,  "([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+):([0-9]+)")

gas_dt[, `:=`(date_year = dates[, 2],
              date_month = dates[, 3],
              date_day = dates[, 4],
              date_hour = dates[, 5], 
              date_minute = dates[, 6],
              date_second = dates[, 7])]
####


