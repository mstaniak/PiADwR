library(readr)
library(dplyr)
library(tidyverse)
library(data.table)
# Import ----
setwd("C:/Users/rogow/OneDrive/Dokumenty/UWr/Programowanie i analiza danych w R/Lab_4")
gas_files <- list.files("./data", full.names = TRUE)
## base
gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)
## tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
## data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
# Select columns ----
## base
gas_base <- gas_base[, c("State.Name", "County.Name", "City.Name",
                        "Local.Site.Name", "Date.Local", "Parameter.Name",
                        "Sample.Duration", "Arithmetic.Mean")]
## tidyverse
gas_tv <- dplyr::select(gas_tv, `Date Local`, `Arithmetic Mean`, `Sample Duration`, `State Name`,
                `County Name`, `City Name`, `Local Site Name`, `Parameter Name`)
gas_tv <- gas_tv %>%
  dplyr::select(`Date Local`, `Arithmetic Mean`, `Sample Duration`, `State Name`,
                `County Name`, `City Name`, `Local Site Name`, `Parameter Name`)
## data.table
gas_dt <- gas_dt[, c("State Name", "County Name", "City Name",
                    "Local Site Name", "Date Local", "Parameter Name",
                    "Sample Duration", "Arithmetic Mean")]
# Rename columns ----
## base
colnames(gas_base) <- c("State", "County", "City", "Site", "Date",
                        "Pollutant", "SampleDuration", "MeasuredValue")
## tidyverse
gas_tv <- gas_tv %>%
  rename(Date = `Date Local`, MeasuredValue = `Arithmetic Mean`,
         SampleDuration = `Sample Duration`, State = `State Name`,
         County = `County Name`, `City` = `City Name`) %>%
  rename(Site = `Local Site Name`, Pollutant = `Parameter Name`)
## data.table
setnames(gas_dt, colnames(gas_dt), colnames(gas_base))
# Drop columns ----
## base
gas_base <- gas_base[, -7]
## tidyverse
gas_tv <- dplyr::select(gas_tv, -SampleDuration)
## data.table
gas_dt[, SampleDuration := NULL]
# Mutate columns ----
## base
gas_base[["MeasuredValue"]] <- ifelse(gas_base[["MeasuredValue"]] < 0,
                                      0, gas_base[["MeasuredValue"]])
## tidyverse
gas_tv <- gas_tv %>%
  mutate(MeasuredValue = ifelse(MeasuredValue < 0, 0, MeasuredValue))
## data.table
gas_dt[, MeasuredValue := ifelse(MeasuredValue < 0, 0, MeasuredValue)]
# Apply to all / selected columns ----
## base
for (i in 1:ncol(gas_base)) {
  if (is.factor(gas_base[, i])) {
    gas_base[, i] <- as.character(gas_base[, i])
  }
}
### or lapply
## tidyverse
gas_tv <- gas_tv %>%
  mutate_all(function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  })
## data.table
gas_dt = gas_dt[, lapply(.SD, function(x) {
  if (is.factor(x)) {
    as.character(x)
  } else {
    x
  }
})]
# Wąska (long) -> szeroka (wide) tidyverse: tidyr ----
# State | County | City | Site | Date | CO (MeasuredValue dla Pollutant == "Carbon monoxide") | Ozone (MeasuredValue dla Pollutantt == "Ozone")
library(dplyr)
unique(gas_tv$Pollutant)
gas_tv_before_spread <- gas_tv %>%
  group_by(State, County, City, Site, Date, Pollutant) %>%
  summarize(MeasuredValue = mean(MeasuredValue, na.rm = TRUE))
gas_wide <- spread(gas_tv_before_spread, Pollutant, MeasuredValue)
gas_wide2 <- spread(gas_tv_before_spread, Pollutant, MeasuredValue, fill = -1)
filter(gas_wide2, is.na(`Sulfur dioxide`))
# Szeroka -> wąska tidyverse: tidyr ----
gas_long <- gather(gas_wide, "Pollutant", "MeasuredValue", `Sulfur dioxide`, Ozone)
filter(gas_long, !is.na(MeasuredValue))
dim(gas_long)
dim(gas_tv_before_spread)
# Wąska -> szeroka data.table ----
gas_dt_wide <- dcast(gas_dt, State + County + City + Site + Date ~ Pollutant,
                     value.var = "MeasuredValue", fill = NA_real_,
                     fun.aggregate = function(x) mean(x, na.rm = TRUE))
# Szeroka -> wąska data.table  ----
gas_dt_long <- melt(gas_dt_wide,
                    id.vars = setdiff(colnames(gas_dt_wide), c("Sulfur dioxide", "Ozone")),
                    measure.vars = c("Sulfur dioxide", "Ozone"),
                    variable.name = "Pollutant", value.name = "MeasuredValue",
                    variable.factor = FALSE)
# Przykłady ----
gas_dt[, NumPollutants := uniqueN(Pollutant),
       by = c("State", "County", "City", "Site", "Date")]

gas_two <- gas_dt[NumPollutants > 1]
gas_two_wide <- data.table::dcast(gas_two, State + County + City + Site + Date ~ Pollutant,
                      value.var = "MeasuredValue",
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))
gas_two_wide
setnames(gas_two_wide, "Sulfur dioxide", "SO")

# Zadanie domowe: -------------------------------------------------------------------------------------------------------------------------------------------
# 1. Dla ramek danych gas_two_wide i gas_two
# -> obliczyć znormalizowane wartości MeasuredValue (odjęcie średniej, podzielenie przed odch. std.)
# (dla gas_two w postaci wąskiej, dla gas_two_wide w postaci szerokiej)
# dla gas_two: przez referencję i bez referencji
# dla gas_two_wide: przy użyciu lapply(), przy użyciu referencji i lapply(), lapply() bez referencji,
# bez lapply - z referencją i bez
# 2. Dla dowolnego miejsca:
# - przekonwertować do wersji szerokiej ze względu na ROK
# - wrócić do wersji wąskiej
# - przekonwertować do wersji szerokiej ze względu na miasto (różne miasta w jednym stanie)
# - znormalizować dla każdego miasta osobno
# 3. Dla gas_dt, zrobić to co na zajęciach ze średnimi bez użycia merge/join.
# 1
gas_two_ref <- gas_two[, MeasuredValueNorm := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue), by = Pollutant]

normMV_SO <- (gas_two[Pollutant == "Sulfur dioxide", MeasuredValue] - mean(gas_two[Pollutant == "Sulfur dioxide", MeasuredValue]))/sd(gas_two[Pollutant == "Sulfur dioxide", MeasuredValue])
normMV_Ozone <- (gas_two[Pollutant == "Ozone", MeasuredValue] - mean(gas_two[Pollutant == "Ozone", MeasuredValue]))/sd(gas_two[Pollutant == "Ozone", MeasuredValue])

norm <- function(x){
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  (x-m)/s
}

pollutant <- c("Ozone", "SO")

lapply(pollutant, function(i){
  norm(gas_two_wide[[i]])
})

gas_two_wide[, lapply(.SD, norm), .SDcols = pollutant]

dt_ozone <- gas_two_wide$Ozone
norm(dt_ozone)
dt_so <- gas_two_wide$SO
norm(dt_so)

gas_two_wide[, c("OzoneNorm", "SONorm") := .(norm(Ozone), norm(SO))]
# 2
gas_dt_wide_date  <- dcast(gas_dt, State + County + City + Site + Pollutant ~ year(Date),
                           value.var = "MeasuredValue", fill = NA_real_,
                           fun.aggregate = function(x) mean(x, na.rm = TRUE))
gas_dt_long_date <- melt(gas_dt_wide_date,
                         id.vars = setdiff(colnames(gas_dt_wide_date), c("2019", "2020")),
                         measure.vars = c("2019", "2020"),
                         variable.name = "Year", value.name = "MeasuredValue",
                         variable.factor = FALSE)
gas_dt_wide_city  <- dcast(gas_dt, County + Site + Date + Pollutant ~ City,
                           value.var = "MeasuredValue", fill = NA_real_,
                           fun.aggregate = function(x) mean(x, na.rm = TRUE))
gas_dt[, .N, by = .(County, Site, Date, Pollutant, City)]
cities <- unique(gas_dt$City)
gas_dt_wide_city[, lapply(.SD, norm), .SDcols = cities]
# 3
gas_dt[, Year := year(Date)]
View(gas_dt[, AverageVal := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")])