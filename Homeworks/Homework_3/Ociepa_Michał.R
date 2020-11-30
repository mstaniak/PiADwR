library(dplyr)
library(lubridate)
library(stringr) # stringi
library(readr)
library(dplyr)
library(data.table)

# Import ----

gas_files <- list.files("./data", full.names = TRUE)


gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)

head(gas_dt)
gas_dt_1 <- copy(gas_dt)

#zadanie 1
gas_dt_1[, datetime := ymd_hms(paste(
  as.character(`Date of Last Change`),
  paste(sample(0:23, .N, replace = TRUE), sample(0:59, .N, replace = TRUE), sample(0:59, .N, replace = TRUE),
        sep = ":")))]

#zadanie 2
unique(floor_date(gas_dt_1$datetime, unit = "month"))
gas_dt_1[, .(mean = mean(`Arithmetic Mean`), median = median(`Arithmetic Mean`), minimum = min(`Arithmetic Mean`), maximum = max(`Arithmetic Mean`), stand_dev = sd(`Arithmetic Mean`)),
       by = .(date = floor_date(datetime, unit = "month"))]


#zadanie 3
gas_dt_1[, `:=` (sec = second(datetime), min = minute(datetime), hour = hour(datetime), day = day(datetime), month = month(datetime), year = year(datetime))]


gas_dt[, datetime := paste(
  as.character(`Date of Last Change`),
  paste(sample(0:23, .N, replace = TRUE), sample(0:59, .N, replace = TRUE), sample(0:59, .N, replace = TRUE),
        sep = "-"), sep = "-")]

split <- str_split(gas_dt$datetime, "-", simplify = TRUE)
gas_dt[, `:=`(year = split[, 1], month = split[, 2], day = split[, 3], hour = split[, 4], minute = split[, 5], second = split[, 6])]
