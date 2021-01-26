library(stringr)
library(lubridate)

gas_dt

# przez referencje od razu zmien na date zmodyfikowana date z dodana godzina jako character
gas_dt[, Date := ymd_hms(paste(
  as.character(Date),
  paste(sample(0:23, 4), sample(0:59, 4), sample(0:59, 4),
        sep = ":")))]

# zaokraglij miesiac w dol
gas_dt[, Date := floor_date(Date, unit = "month")]

# liczymy
gas_dt[, list(MeanMeasured = mean(MeasuredValue, na.rm = TRUE),
              Median = median(MeasuredValue, na.rm = TRUE),
              MaxMeasured = max(MeasuredValue, na.rm = TRUE),
              MinMeasured = min(MeasuredValue, na.rm = TRUE)),
       by = c("Date")]


# zaokraglij rok w dol
gas_dt[, Date := floor_date(Date, unit = "year")]

# liczymy
gas_dt[, list(MeanMeasured = mean(MeasuredValue, na.rm = TRUE),
              Median = median(MeasuredValue, na.rm = TRUE),
              MaxMeasured = max(MeasuredValue, na.rm = TRUE),
              MinMeasured = min(MeasuredValue, na.rm = TRUE)),
       by = c("Date")]


# ekstrakcja z daty i dodaj jako kolumny
gas_dt[, `:=`(Year = year(Date),
              Month = month(Date),
              Day = day(Date),
              Hour = hour(Date),
              Minute = minute(Date),
              Second = second(Date))]


# ekstrakcja z characteru i dodaj jako kolumny
# format to : YYYY-MM-DD HH:MM:SS
char_to_vals <- function(char){
  full_date <- "([0-9][0-9]{3})[- .]([0-9]{2})[- .]([0-9]{2})[ ]([0-9]{2})[- :]([0-9]{2})[- :]([0-9]{2})"
  results = str_match(char, full_date)
  year = results[2]
  month = results[3]
  day = results[4]
  hour = results[5]
  minute = results[6]
  second = results[7]
  c(year, month, day, hour, minute, second)
} 

gas_dt[, c("Year", "Month", "Day", "Hour", "Minute", "Second")] <- transpose(gas_dt[, lapply(Date, char_to_vals)])