# ZADANIE 1
normalization <- function(n){
  (n-mean(n))/sd(n)
}

### gas_two ###
# referencja
gas_two[,norm := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)] 
mean(gas_two[,MeasuredValue])
sd(gas_two[,MeasuredValue])
mean(gas_two[,norm])
sd(gas_two[,norm])

# bez referncji
gas_two_norm = (gas_two[,MeasuredValue] - mean(gas_two[,MeasuredValue]))/sd(gas_two[,MeasuredValue])

### gas_two_wide ###
# lapply()
gas_two_wide_2 = gas_two_wide[, lappy(.SD, normalization(x)), .SDcols = c("CO","Ozone")]

# referencja i lapply()

# lapply() bez referencji

# bez referencji i bez lapply()

# bez lapply i z referencja




# ZADANIE 2. Tutaj wybieramy dowolne miesjce
gas_dt_ = gas_dt[State == "Oklahoma" & County == "Oklahoma" & City == "Oklahoma City" & Site == "Near Road",]
### ze względu na rok ###
## Wąska -> szeroka
gas_dt_wide2 <- dcast(gas_dt_ , State + County + City + Site + Pollutant ~  year(Date),
                      value.var = "MeasuredValue", fill = NA_real_,
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))
## Szeroka -> wąska
gas_dt_long2 <- melt(gas_dt_wide2,
                     id.vars = setdiff(colnames(gas_dt_wide2), c("2020", "2019")),
                     measure.vars = c("2020", "2019"),
                     variable.name = "Year", value.name = "MeasuredValue",
                     variable.factor = FALSE)
# - przekonwertować do wersji szerokiej ze względu na miasto (różne miasta w jednym stanie)
gas_dt_pl <- gas_dt[State == "Oklahoma",]
gas_dt_wide3 <- dcast(gas_dt_pl, State + County + Site + Date + Pollutant ~  City,
                     value.var = "MeasuredValue", fill = NA_real_,
                     fun.aggregate = function(x) mean(x, na.rm = TRUE))

# znormalizować dla każdego miasta osobno
gas_dt_pl[,normalization := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)]

# ZADANIE 3. Dla gas_dt, zrobić to co na zajęciach ze średnimi bez użycia merge/join.
gas_dt[,Average_Val := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")]
