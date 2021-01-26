# zadanie 1

#dla gas_two z referencja
gas_two_norm_ref <- gas_two[, NormalizedValue := (MeasuredValue-mean(MeasuredValue))/sd(MeasuredValue)]


#dla gas_two bez referencji
NormalizedValue <- gas_two[, .((MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue))]
gas_two_norm <- data.table(gas_two,NormalizedValue)

# zadanie 2

gas_dt_2 <- gas_dt[State == "Alabama" & County == "Jefferson" & City == "Birmingham" & Site == "North Birmingham",]
gas_dt_wide <- dcast(gas_dt_2, State + County + City + Site + Date ~ Year,
      value.var = "MeasuredValue", fill = NA_real_,
      fun.aggregate = function(x) mean(x, na.rm = TRUE))


gas_dt_long <- melt(gas_dt_wide,
                    id.vars = setdiff(colnames(gas_dt_wide), c("2018", "2019")),
                    measure.vars = c("2018", "2019"),
                    variable.name = "Year", value.name = "MeasuredValue",
                    variable.factor = FALSE)



# zadanie 3
gas_dt_zad3 <- gas_dt[, Year := year(Date)]
gas_dt_zad3[, AverageVal := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")]
