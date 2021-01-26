#ZAD1
#GAS_TWO
# referencje
gas_two[,normalizeMeasuredV := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)]

# bez referencji
gas_two_norm = (gas_two[, MeasuredValue] - mean(gas_two[, MeasuredValue]))/sd(gas_two[, MeasuredValue])
gas_two[ ,list(normalizeMeasuredV1 = gas_two_norm),]

#GAS_TWO_WIDE
normaliz <- function(x){
  return (x-mean(x))/sd(x)
}
#lapply
gas_two_wide = gas_two_wide[, lapply(.SD, normalize(x)), .SDcols =c("CO","Ozone")]
all_cases[,.SD, .SDcols=selected_columns]

#lapply i referencje
gas_two_wide = gas_two_wide[, ,]

#lapply i bez referencji
gas_dt_wide <- gas_dt_wide[,.(normalizeCO = normaliz())]

#z referencją bez lapply
gas_two_wide <- gas_two_wide[,c("normalizeCO", "normalizeOz") := .(mean(CO), mean(Ozone)),]

#bez referencji bez lapply
gas_two_wide <- gas_two_wide[,list(normalizeCO = mean("Carbon monoxide"), normalizeOz = normaliz("Ozone")),]


#ZAD2
place_single <- gas_dt[State == "Alabama" & County == "Jefferson" & City == "Birmingham" & Site == "North Birmingham"]
gas_dt_wide1 <- dcast(place_single, State + County + City + Site + Pollutant ~ year(Date),
                      value.var = "MeasuredValue", fill = NA_real_,
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))

gas_dt_long1 <- melt(gas_dt_wide1,
                     id.vars = setdiff(colnames(gas_dt_wide1), c("2020", "2019")),
                     measure.vars = c("2020", "2019"),
                     variable.name = "Date_year", value.name = "MeasuredValue",
                     variable.factor = FALSE)

gas_dt_wide2 <- dcast(place_single, State + County + Date + Site + Pollutant ~ City,
                      value.var = "MeasuredValue", fill = NA_real_,
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))


#ZAD3
gas_dt3 <- gas_dt[, AverageVal2 := mean(MeasuredValue), by = c("Pollutant", "State", "Year")]
