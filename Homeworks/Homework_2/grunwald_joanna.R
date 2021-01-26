zad_1_a <- gas_two[, Normed_MV := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)]
#zad_1_b 
gas_two[, "Normed_MV"] = gas_two[, MeasuredValue - mean(MeasuredValue)/sd(MeasuredValue), by="Pollutant"][,2]
zad_1_c <- gas_two_wide[, c("Normed_Oz", "Normed_Sd") := lapply(.SD, function(x) (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)), .SDcols = c("Ozon", "Sulfur dioxide")]

norm <- function(x){
  (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
}
zad_1_d <- gas_two_wide[, `:=` (Norm_Oz=norm(Ozone), Norm_Sd=norm(`Sulfur dioxide`))]
#zad_1_e
gas_two_wide[, c("Normed_Ozone", "Normed_Sulfur")] = lapply(gas_two_wide[, c("Ozone", "Sulfur dioxide")], norm)
#zad_1_f
gas_two_wide[, c("Norm_Sulfur")] = gas_two_wide[, norm("Sulfur dioxide")]
gas_two_wide[, c("Norm_Ozone")] = gas_two_wide[, norm("Ozone")]


gas_single_year <- gas_dt[State == "Alabama" & County == "Jefferson" &
                            City == "Birmingham" & Site == "North Birmingham"]
zad_2_a <- dcast(gas_single_year, State + County + City + Site + Pollutant ~ year(Date),
                 value.var = "MeasuredValue", fill = NA_real_,
                 fun.aggregate = function(x) mean(x, na.rm = TRUE))
zad_2_b <- melt(zad_2_a,
                id.vars = setdiff(colnames(zad_2_a), c("2018", "2019", "2020")),
                measure.vars = c("2018", "2019", "2020"),
                variable.name = "Date", value.name = "MeasuredValue",
                variable.factor = FALSE)

gas_single_city <- gas_dt[State == "Alabama" & County == "Jefferson"]
zad_2_c <- dcast(gas_single_city, State + County + Date + Site + Pollutant ~ City,
                 value.var = "MeasuredValue", fill = NA_real_,
                 fun.aggregate = function(x) mean(x, na.rm = TRUE))

zad_2_d <- gas_single_city[,  Normed_MV := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue), by=c("City")]



zad_3 <- gas_dt[, Averages := mean(MeasuredValue, na.rm=TRUE), by = c("State", "Pollutant", "Year")]  
