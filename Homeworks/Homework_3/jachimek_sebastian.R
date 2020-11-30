library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

gas_file <- list.files("./data", full.names = TRUE)
gas_dfs_dt <- lapply(gas_file, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt <- gas_dt[, c("State Name", "County Name", "City Name",
                     "Local Site Name", "Date Local", "Parameter Name",
                     "Sample Duration", "Arithmetic Mean")]


names_of_cols <- c("State", "County", "City", "Site", "Date", "Pollutant", "SampleDuration", "MeasuredValue")
setnames(gas_dt, colnames(gas_dt), names_of_cols)
head(gas_dt)

gas_dt[, NumPollutants := uniqueN(Pollutant), by = c("State", "County", "City", "Site", "Date")]


#ZADANIE 1

dates <- gas_dt[, Date]

datetimes <- paste(as.character(dates), paste(sample(0:23, length(dates), replace = TRUE), 
                    sample(0:59, length(dates), replace = TRUE), sample(0:59, length(dates), replace = TRUE), sep = ":"))


datetimes2 <- ymd_hms(datetimes)

gas_dt[, Date := datetimes2]
head(gas_dt)

#ZADANIE 2

rounded_month <- floor_date(gas_dt[, Date], unit = "month")
rounded_year <- floor_date(gas_dt[, Date], unit = "year")

gas_stats <- gas_dt[, .(mean(MeasuredValue), median(MeasuredValue), min(MeasuredValue), max(MeasuredValue)), 
       by = list(month(rounded_month), year(rounded_year))]

gas_stats_names <- c("Month", "Year", "MeanValue", "Median", "MinValue", "MaxValue")
setnames(gas_stats, gas_stats_names)
head(gas_stats)


#ZADANIE 3 

##a) jako datê

get_year <- year(gas_dt[, Date])
get_month <- month(gas_dt[, Date])
get_day <- day(gas_dt[, Date])
get_hour <- hour(gas_dt[, Date])
get_minute <- minute(gas_dt[, Date])
get_second <- second(gas_dt[, Date])


gas_dt_copy1 <- gas_dt[, -5] #tworzê kopiê (bez kolumny Date), gdzie dodam poszczególne kolumny (traktuj¹c datê jak datê)

gas_dt_copy1[, c("Year", "Month", "Day", "Hour", "Minute", "Second") := 
               list(get_year, get_month, get_day, get_hour, get_minute, get_second)]

head(gas_dt_copy1)

##b) jako napis

gas_dt_copy2 <- gas_dt[, -5] #tworzê kopiê (bez kolumny Date), gdzie dodam poszczególne kolumny (traktuj¹c datê jako napis)

dtms1 <- str_split(datetimes, "-", simplify = TRUE)
dtms1_prime <- dtms1[, 1:2] #macierz z³o¿ona z lat i miesiêcy
dtms2 <- str_split(dtms1[, 3], " ", simplify = TRUE)
dtms2_prime <- dtms2[, 1] # macierz z³o¿ona z dni
dtms3 <- str_split(dtms2[, 2], ":", simplify = TRUE) # macierz z³o¿ona z godzin, minut i sekund

dtms <- cbind(dtms1_prime, dtms2_prime, dtms3)

gas_dt_copy2[, c("Year", "Month", "Day", "Hour", "Minute", "Second") := 
               list(dtms[, 1], dtms[, 2], dtms[, 3], dtms[, 4], dtms[, 5], dtms[, 6])]

head(gas_dt_copy2)