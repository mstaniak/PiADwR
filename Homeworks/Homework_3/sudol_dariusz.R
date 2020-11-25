time <- paste(sample(0:23, nrow(gas_dt), replace=T), sample(0:59, nrow(gas_dt), replace=T), sample(0:59, nrow(gas_dt), replace=T), sep = ":")
gas_dt[, Date := paste(Date, time, sep=" ")]
gas_dt[, Date := ymd_hms(Date, tz = "UTC")]

gas_dt[, Date := floor_date(Date, unit = "month")]
gas_dt[, .(Mean = mean(MeasuredValue), Median = median(MeasuredValue), Min = min(MeasuredValue), Max = max(MeasuredValue)), by = Date]

gas_dt[, Date := floor_date(Date, unit = "year")]
gas_dt[, .(Mean = mean(MeasuredValue), Median = median(MeasuredValue), Min = min(MeasuredValue), Max = max(MeasuredValue)), by = Date]

# data
gas_dt[, c("Year", "Month", "Day", "Hour", "Minute", "Second") := .(year(Date), month(Date), day(Date), hour(Date), minute(Date), second(Date))]

gas_dt[, c("Year", "Month", "Day", "Hour", "Minute", "Second") := NULL]
gas_dt[, Date := as.character(Date)]

# napis

ydmhms <- str_match(gas_dt$Date, "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})")[,-1]
gas_dt[, c("Year", "Month", "Day", "Hour", "Minute", "Second")  := as.data.table(ydmhms)]