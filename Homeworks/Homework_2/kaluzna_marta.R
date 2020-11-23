#### Zadanie domowe nr 2

## 1. Dla ramek danych gas_two_wide i gas_two
## -> obliczyć znormalizowane wartości MeasuredValue (odjęcie średniej, podzielenie przed odch. std.)
## (dla gas_two w postaci wąskiej, dla gas_two_wide w postaci szerokiej)

standardize <- function(x){
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

# dla gas_two
# przez referencję:
gas_two[, NormalizedMeasuredValue := standardize(MeasuredValue), by = Pollutant]
# bez referencji:
gas_two[, 'NormalizedMeasuredValue'] <- gas_two[, standardize(MeasuredValue), by = Pollutant][,2]

# dla gas_two_wide
# lapply() i referencja
gas_two_wide[, c('NormalizedCO', 'NormalizedOzone') := lapply(.SD, standardize), .SDcols = c('CO', 'Ozone')]
# lapply() i bez referencji
gas_two_wide[, c('NormalizedCO', 'NormalizedOzone')] <- gas_two_wide[, lapply(.SD, standardize), .SDcols = c('CO', 'Ozone')]
# bez lapply() i z referencją
gas_two_wide[, c('NormalizedCO', 'NormalizedOzone') := list(standardize(CO), standardize(Ozone))]
# bez lapply() i bez referencji
gas_two_wide[, c('NormalizedCO', 'NormalizedOzone')] <- gas_two_wide[, list(standardize(CO), standardize(Ozone))]


## 2. Dla dowolnego miejsca:
gas_dt[, Year := year(Date)]
gas_single <- gas_dt[State == "Alabama" & County == "Jefferson" &
                       City == "Birmingham" & Site == "North Birmingham" &
                       Pollutant == "Ozone"]

# przekonwertować do wersji szerokiej ze względu na ROK
gas_single[, Date := format(Date, format = '%m-%d')]
gas_single_wide <- dcast.data.table(gas_single, State + County + City + Site + Pollutant + Date ~ Year,
                                    value.var = "MeasuredValue", fill = NA_real_)

# wrócić do wersji wąskiej
gas_single_long <- melt.data.table(gas_single_wide, id.vars = setdiff(colnames(gas_single_wide), c("2017", "2020")),
                                   measure.vars = c("2017", "2020"),
                                   variable.name = "Year", value.name = "MeasuredValue",
                                   variable.factor = FALSE)

# wersja (wąska) bez brakujących wartości 
# gas_single_long <- melt.data.table(gas_single_wide, id.vars = setdiff(colnames(gas_single_wide), c("2017", "2020")),
#                         measure.vars = c("2017", "2020"),
#                         variable.name = "Year", value.name = "MeasuredValue",
#                         variable.factor = FALSE, na.rm = TRUE)


# przekonwertować do wersji szerokiej ze względu na miasto (różne miasta w jednym stanie)
gas_single_2 <- gas_dt[State == "Alabama" & County == "Jefferson" &
                         Pollutant == "Ozone"]

gas_single_wide_2 <- dcast.data.table(gas_single_2, State + County + Site + Pollutant + Date ~ City,
                                      value.var = "MeasuredValue", fill = NA_real_)

# znormalizować dla każdego miasta osobno
# dla gas_single_2
gas_single_2[, NormalizedMeasuredValue := standardize(MeasuredValue), by = City]


## 3. Dla gas_dt, zrobić to co na zajęciach ze średnimi bez użycia merge/join.
gas_dt[, AverageVal := mean(MeasuredValue, na.rm = TRUE), 
       by = c("Pollutant", "State", "Year")]
