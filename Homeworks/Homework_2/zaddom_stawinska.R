
### KOD Z ZAJÊÆ - PRZYGOTOWANIE DANYCH
library(readr)
library(dplyr)
library(data.table)
library(reshape2)

gas_files <- list.files("./Materials/Lab_04/dane", full.names = TRUE)

gas_dfs <- lapply(gas_files, read.csv)
gas_base <- do.call("rbind", gas_dfs)
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)

gas_base = gas_base[, c("State.Name", "County.Name", "City.Name",
                        "Local.Site.Name", "Date.Local", "Parameter.Name",
                        "Sample.Duration", "Arithmetic.Mean")]


gas_tv = select(gas_tv, `Date Local`, `Arithmetic Mean`, `Sample Duration`, `State Name`,
                `County Name`, `City Name`, `Local Site Name`, `Parameter Name`)
gas_tv <- gas_tv %>%
  select(`Date Local`, `Arithmetic Mean`, `Sample Duration`, `State Name`,
         `County Name`, `City Name`, `Local Site Name`, `Parameter Name`)

gas_dt = gas_dt[, c("State Name", "County Name", "City Name",
                    "Local Site Name", "Date Local", "Parameter Name",
                    "Sample Duration", "Arithmetic Mean")]

colnames(gas_base) <- c("State", "County", "City", "Site", "Date",
                        "Pollutant", "SampleDuration", "MeasuredValue")

gas_tv <- gas_tv %>%
  rename(Date = `Date Local`, MeasuredValue = `Arithmetic Mean`,
         SampleDuration = `Sample Duration`, State = `State Name`,
         County = `County Name`, `City` = `City Name`) %>%
  rename(Site = `Local Site Name`, Pollutant = `Parameter Name`)

setnames(gas_dt, colnames(gas_dt), colnames(gas_base))


gas_dt[, NumPollutants := uniqueN(Pollutant),
       by = c("State", "County", "City", "Site", "Date")]

gas_two <- gas_dt[NumPollutants > 1]
gas_two_wide <- data.table::dcast(gas_two, State + County + City + Site + Date ~ Pollutant,
                                  value.var = "MeasuredValue",
                                  fun.aggregate = function(x) mean(x, na.rm = TRUE))

class(gas_two_wide) #wczesniej byl tylko data.frame, ale juz ok



###ZADANIE DOMOWE

# 1. Dla ramek danych gas_two_wide i gas_two
# -> obliczyæ znormalizowane wartoœci MeasuredValue (odjêcie œredniej, podzielenie przed odch. std.)

normalizacja = function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

# (dla gas_two w postaci w¹skiej, dla gas_two_wide w postaci szerokiej)
# dla gas_two: przez referencjê i bez referencji
gas_two[, norm_MV_r := normalizacja(MeasuredValue), by = 'Pollutant']

#bez
gas_two[,'norm_MV'] = gas_two[,normalizacja(MeasuredValue), by = 'Pollutant'][,2]

# dla gas_two_wide: przy u¿yciu lapply() - przy u¿yciu referencji i bez referencji,
setnames(gas_two_wide, c("State", "County","City","Site" ,"Date", "CO", "NO", "OZONE"))

gas_two_wide[, c("CO", "NO", "OZONE") := lapply(.SD, normalizacja),
             .SDcols = c("CO", "NO", "OZONE")]
#bez
gas_two_wide[, c("CO", "NO", "OZONE")] = lapply(gas_two_wide[, c("CO", "NO", "OZONE")], normalizacja)

# bez lapply - z referencj¹ i bez

gas_two_wide[, `:=` (CO_ref = normalizacja(CO),
                     NO_ref = normalizacja(NO),
                     OZONE_ref = normalizacja(OZONE))]
#bez
gas_two_wide[, 'CO_bez'] = gas_two_wide[, normalizacja(CO)]
gas_two_wide[, 'NO_bez'] = gas_two_wide[, normalizacja(NO)]
gas_two_wide[, 'OZONE_bez'] = gas_two_wide[, normalizacja(OZONE)]



# 2. Dla dowolnego miejsca:
# - przekonwertowaæ do wersji szerokiej ze wzglêdu na ROK
# - wróciæ do wersji w¹skiej
# - przekonwertowaæ do wersji szerokiej ze wzglêdu na miasto (ró¿ne miasta w jednym stanie)
# - znormalizowaæ dla ka¿dego miasta osobno

#
gas_wybrany = gas_dt[State == "California" & County == "Los Angeles"]

gas_dt_wide_year  <- dcast(gas_wybrany, State + County + City + Site + Pollutant ~ year(Date),
                           value.var = "MeasuredValue", fill = NA_real_,
                           fun.aggregate = function(x) mean(x, na.rm = TRUE))
#
gas_dt_long_year <- melt(gas_dt_wide_year,
                         id.vars = setdiff(colnames(gas_dt_wide_year), c("2019")),
                         measure.vars = c("2019"),
                         variable.name = "Year", value.name = "MeasuredValue",
                         variable.factor = FALSE)
#
gas_dt_wide_city  <- dcast(gas_dt, County + Site + Date + Pollutant ~ City,
                           value.var = "MeasuredValue", fill = NA_real_,
                           fun.aggregate = function(x) mean(x, na.rm = TRUE))
#
gas_dt_norm_city = gas_dt_wide_city[, MV_norm := normalizacja(MeasuredValue), by = c('City')]




# 3. Dla gas_dt, zrobiæ to co na zajêciach ze œrednimi bez u¿ycia merge/join.

gas_dt[, MeanVal := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")]




