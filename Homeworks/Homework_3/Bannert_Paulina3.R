library(readr)
library(dplyr)
library(data.table)
# Import ----
gas_files <- list.files("./data", full.names = TRUE)

gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt = gas_dt[, c("State Name", "County Name", "City Name",
                    "Local Site Name", "Date Local", "Parameter Name",
                    "Sample Duration", "Arithmetic Mean")]
newnames <- c("State", "County", "City", "Site", "Date",
              "Pollutant", "SampleDuration", "MeasuredValue")
setnames(gas_dt, colnames(gas_dt), newnames)

#ZAD1
dates <- gas_dt$Date

date_times <- paste(
  as.character(dates),
  paste(sample(0:23, 4), sample(0:59, 4), sample(0:59, 4),
        sep = ":"))

date_times <- ymd_hms(date_times)
class(date_times)

gas_dt[, Date := date_times, ]

#ZAD2
gas_dt[, Date_month_round := floor_date(Date, unit = "month") ] 
gas_dt[, Date_year_round := floor_date(Date, unit = "year") ] # czy to miało być wykonane na date_month_round
gas_dt[, .(MIN = min(MeasuredValue), MAX = max(MeasuredValue), MEAN = mean(MeasuredValue),
           MEDIAN = median(MeasuredValue)), by = .(month(Date_round), year(Date_round))]
gas_dt[, .(MIN = min(MeasuredValue), MAX = max(MeasuredValue), MEAN = mean(MeasuredValue),
           MEDIAN = median(MeasuredValue)), by = month(Date_month_round)]
# to i to:
gas_dt[, .(MIN = min(MeasuredValue), MAX = max(MeasuredValue), MEAN = mean(MeasuredValue),
           MEDIAN = median(MeasuredValue)), by = year(Date_year_round)]
#lub to...
gas_dt[, .(MIN = min(MeasuredValue), MAX = max(MeasuredValue), MEAN = mean(MeasuredValue),
           MEDIAN = median(MeasuredValue)), by = .(month(Date_month_round), year(Date_year_round))]

#usuwamy te kolumny
gas_dt[, `:=`(Date_month_round = NULL, Date_year_round = NULL)]


#ZAD3
#traktując datę jako date
gas_dt[, `:=`(YEAR = year(Date), MONTH = month(Date), DAY = day(Date), HOUR =hour(Date),
          MINUTE = minute(Date), SEC = second(Date))]
# usuwamy
gas_dt[, `:=`(YEAR = NULL, MONTH = NULL, DAY = NULL, HOUR = NULL, MINUTE = NULL, SEC = NULL)]

#jeszcze raz, traktując datę jako stringa
datki <- str_match(gas_dt$Date, "([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+):([0-9]+)")
gas_dt[, `:=`(YEAR = datki[,2], MONTH = datki[,3], DAY = datki[,4], HOUR = datki[,5],
              MINUTE = datki[,6], SEC = datki[,7])]
