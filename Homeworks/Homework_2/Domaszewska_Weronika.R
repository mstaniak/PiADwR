##### ZADANIE 1 #####
# Dla ramek danych gas_two_wide i gas_two
# -> obliczyć znormalizowane wartości MeasuredValue (odjęcie średniej, podzielenie przed odch. std.)
# (dla gas_two w postaci wąskiej, dla gas_two_wide w postaci szerokiej)
# dla gas_two: przez referencję i bez referencji
# dla gas_two_wide: przy użyciu lapply(), przy użyciu referencji i lapply(), lapply() bez referencji,
# bez lapply - z referencją i bez

##### gas_two #####

### referencja  ###
gas_two[,norm := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)] 
# sprawdzenie 
mean(gas_two[,MeasuredValue])
sd(gas_two[,MeasuredValue])
mean(gas_two[,norm])
sd(gas_two[,norm])

### bez referncji ###
gas_two_norm = (gas_two[,MeasuredValue] - mean(gas_two[,MeasuredValue]))/sd(gas_two[,MeasuredValue])

##### gas_two_wide #####

# przy u życiu lapply()
gas_two_wide_2 = gas_two_wide[, lappy(.SD, normalization(x)), .SDcols = c("CO","Ozone")]


##### ZADANIE 2 #####

# Dla dowolnego miejsca:
# - przekonwertować do wersji szerokiej ze względu na ROK
# - wrócić do wersji wąskiej
# - przekonwertować do wersji szerokiej ze względu na miasto (różne miasta w jednym stanie)
# - znormalizować dla każdego miasta osobno

# wybór miejsca
gas_dt_msc = gas_dt[State == "Arizona" & County == "Maricopa" & City == "Phoenix" & Site == "JLG SUPERSITE",]

### ze względu na ROK ###

# Wąska -> szeroka
gas_dt_wide_msc <- dcast(gas_dt_msc, State + County + City + Site + Pollutant ~  year(Date),
                      value.var = "MeasuredValue", fill = NA_real_,
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))

# Szeroka -> wąska
gas_dt_long_msc <- melt(gas_dt_wide_msc,
                     id.vars = setdiff(colnames(gas_dt_wide_msc), c("2019", "2020")),
                     measure.vars = c("2019", "2020"),
                     variable.name = "Year", value.name = "MeasuredValue",
                     variable.factor = FALSE)

### ze względu na miasto (różne miasta w jednym stanie) ###
gas_dt_msc2 = gas_dt[State == "Arizona" ,]
gas_dt_wide_msc2 <- dcast(gas_dt_msc2, State + County + Site + Date + Pollutant ~  City,
                      value.var = "MeasuredValue", fill = NA_real_,
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))

### normalizacja ###
gas_dt_msc2[,norm := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)] 

##### ZADANIE 3 #####
# 3. Dla gas_dt, zrobić to co na zajęciach ze średnimi bez użycia merge/join.
gas_dt[,Average_Val := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")]




