#ZADANIE DOMOWE 2
# 1. Dla ramek danych gas_two_wide i gas_two
# -> obliczyć znormalizowane wartości MeasuredValue (odjęcie średniej, podzielenie przed odch. std.)

normval <- function(input_vector){
  (input_vector - mean(input_vector))/ sd(input_vector)
}

# (dla gas_two w postaci wąskiej, dla gas_two_wide w postaci szerokiej)
# dla gas_two: przez referencję 

gas_two_ref <- gas_two[Pollutant == 'Ozone', NormVal := normval(MeasuredValue)]
gas_two_ref <- gas_two[Pollutant == 'Nitrogen dioxide (NO2)', NormVal := normval(MeasuredValue)]

#i bez referencji 
#coś takiego kombinowałam ale nie chciało działać:
#gas_two_noref <- gas_two[Pollutant == 'Ozone', NormVal = normval(MeasuredValue)]
#gas_two_noref <- gas_two[Pollutant == 'Nitrogen dioxide (NO2)', NormVal = normval(MeasuredValue)]

# dla gas_two_wide: przy użyciu lapply() - przy użyciu referencji
#i bez referencji
# bez lapply - z referencją
gas_two_wide[, NormVal_NO2 := normval(NO2)]
gas_two_wide[, NormVal_NO2 := normval(CO)]

#i bez
gas_two_wide$NormVal_NO2 <- normval(gas_two_wide$NO2)
gas_two_wide$NormVal_Ozone <- normval(gas_two_wide$Ozone)

# 2. Dla dowolnego miejsca:
# - przekonwertować do wersji szerokiej ze względu na ROK
gas_dt[, Year := year(Date)]
gas_dt_wide_year <- dcast(gas_dt, State + County + City + Site + Pollutant ~ Year,
                     value.var = "MeasuredValue", fill = NA_real_,
                     fun.aggregate = function(x) mean(x, na.rm = TRUE))

# - wrócić do wersji wąskiej
gas_dt_long_year <- melt(gas_dt_wide_year,
                    id.vars = setdiff(colnames(gas_dt_wide_year), c("1989", "2020")),
                    measure.vars = c("1989", "2020"),
                    variable.name = "Year", value.name = "MeasuredValue",
                    variable.factor = FALSE)


# - przekonwertować do wersji szerokiej ze względu na miasto (różne miasta w jednym stanie)
# - znormalizować dla każdego miasta osobno
# 3. Dla gas_dt, zrobić to co na zajęciach ze średnimi bez użycia merge/join.

gas_dt[, AverageVal := mean(MeasuredValue), by = c("Pollutant", "State", "Year")]
