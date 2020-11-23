gas_two_wide[, list(norm_ozone = (Ozone - mean(Ozone) / sd(Ozone)), (norm_sulfur = (`Sulfur dioxide` - mean(`Sulfur dioxide`)) / sd(`Sulfur dioxide`)) )]

gas_two_wide[, `:=` (norm_co = ((Ozone - mean(Ozone))/sd(Ozone)), norm_sulfur = ((`Sulfur dioxide` - mean(`Sulfur dioxide`))/sd(`Sulfur dioxide`)))]

gas_two_wide[, c("norm_sulf", "norm_ozone") := lapply(.SD, function(x)
{
  ( x-mean(x) )/sd(x)
}), .SDcols = c("Sulfur dioxide", "Ozone")]

gas_two_wide[, c("norm_sulf", "norm_co") = lapply(.SD, function(x)
{
  ( x-mean(x) ) /sd(x)
}), .SDcols = c("Sulfur dioxide", "Ozone")]

gas_dt_wide_year <-  dcast(gas_dt, State + County + City + Site + Pollutant ~ year(Date), 
      value.var = "MeasuredValue", fill = NA_real_,
      fun.aggregate = function(x) mean(x, na.rm = TRUE))

gas_dt_long_year <- melt(gas_dt_wide_year, 
                         id.vars = setdiff(colnames(gas_dt_wide_year), c("2019", "2020")),
                         measure.vars = c("2019", "2020"),
                         variable.name = "Year", value.name = "MeasuredValue",
                         variable.factor = FALSE)

gas_dt_wide_city <- dcast(gas_dt_lond_year, State + County + Year + Site + Pollutant ~ City, 
                          value.var = "MeasuredValue", fill = NA_real_,
                          fun.aggregate = function(x) mean(x, na.rm = FALSE))
