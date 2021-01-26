#1
#normalizacja zwykla
gas_two[, .(normalize = (MeasuredValue - mean(MeasuredValue) / sd(MeasuredValue)))]
gas_two_wide[, .(normalize_sulf = (Sulfur dioxide - mean(Sulfur dioxide) / sd(Sulfur dioxide)), normalize_co = (CO - mean(CO) / sd(CO)))]
#normalizacja z referencja
gas_two[, `:=`(normalize = (MeasuredValue - mean(MeasuredValue) / sd(MeasuredValue)))]
gas_two_wide[, `:=`(normalize_sulf = (Sulfur dioxide - mean(Sulfur dioxide) / sd(Sulfur dioxide)), normalize_co = (CO - mean(CO) / sd(CO)))]
#lapply z referencja
gas_two_wide[, c("normalize_sulf", "normalize_co") := lapply(.SD, function(x)
  {
  (x-mean(x))/sd(x)
}), .SDcols = c("Sulfur dioxide", "CO")]
#lapply bez referencji
gas_two_wide[, c("normalize_sulf", "normalize_co") = lapply(.SD, function(x)
{
  (x-mean(x))/sd(x)
}), .SDcols = c("Sulfur dioxide", "CO")]


#2
gas_dt_wide_year <-  dcast(gas_dt, State + County + City + Site + Pollutant ~ year(Date), 
                           value.var = "MeasuredValue", fill = NA_real_,
                           fun.aggregate = function(x) mean(x, na.rm = TRUE)


gas_dt_long_year <- melt(gas_dt_wide_year, 
                         id.vars = setdiff(colnames(gas_dt_wide_year), c("2019", "2020")),
                         measure.vars = c("2019", "2020"),
                         variable.name = "Year", value.name = "MeasuredValue",
                         variable.factor = FALSE)

gas_dt_wide_city <- dcast(gas_dt_lond_year, State + County + Year + Site + Pollutant ~ City, 
                          value.var = "MeasuredValue", fill = NA_real_,
                          fun.aggregate = function(x) mean(x, na.rm = FALSE))


gas_dt_wide_city[, normalized = (MeasuredValue-mean(MeasuredValue))/sd(MeasuredValue), by = City]

