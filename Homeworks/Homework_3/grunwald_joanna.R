# zad1
date <- gas_dt[, Date]
sampledate <- paste(as.character(date),paste(sample(0:23, length(date), replace=T), sample(0:59, length(date), replace=T), sample(0:59, length(date), replace=T, sep = ":")))
gas_dt[, Date := ymd_hms(sampledate)]
                    
#zad2
statistics <- gas_dt[, .(mean(MeasuredValue), median(MeasuredValue), sd(MeasuredValue), min(MeasuredValue), max(MeasuredValue)), by = list(month(floor_date(gas_dt[, Date], unit="month")), year(floor_date(gas_dt[, Date], unit="year")))]
                    
#zad3
gas_dt[, c("year", "month", "day", "hour", "minute", "second") := list(year(gas_dt[, Date]), month(gas_dt[, Date]), day(gas_dt[, Date]), hour(gas_dt[, Date]), minute(gas_dt[, Date]), second(gas_dt[, Date]))]
                    
yearmonth <- str_split(sampledate, "-", simplify=T)[, 1:2]
days <- str_split(str_split(sampledate, "-", simplify=T)[, 3], " " , simplify=T)[, 1] 
times <- str_split(str_split(str_split(sampledate, "-", simplify=T)[, 3], " " , simplify=T)[, 2], ":", simplify=T)
                    
all <- cbind(year, month, days, times)
gas_dt[, c("year", "month", "day", "hour", "minute", "second") := list(all[, 1], all[, 2], all[, 3], all[, 4], all[, 5], all[, 6])]