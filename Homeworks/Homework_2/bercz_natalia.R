library(tidyverse)
library(data.table)

#1
gas_two_ref = gas_two[, MeasuredValue := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)]

norm = gas_two[,(MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue)]
gas_two_noref = data.table(gas_two, norm)

gas_two_wide_lapply = dcast(gas_two_wide, State + County + City + Site + Date ~ Pollutant,
                        value.var = 'MeasuredValue',
                        fun.aggregate = function(x) lapply((x - mean(x))/sd(x)))

gas_two_wide_lapplyref = dcast(gas_two_wide, State + County + City + Site + Date ~ Pollutant,
                         fun.aggregate = function(x) lapply(MeasuredValue := (x - mean(x))/sd(x)))

gas_two_wide_nolapplyref = dcast(gas_two_wide, State + County + City + Site + Date ~ Pollutant,
                          fun.aggregate = function(x) MeasuredValue := (x - mean(x))/sd(x))

gas_two_wide_nollappynoref = dcast(gas_two_wide, State + County + City + Site + Date ~ Pollutant,
                           value.var = 'MeasuredValue',
                           fun.aggregate = function(x) (x - mean(x)/sd(x)))

#2
gas_dt_wide = dcast(gas_dt, State + County + City + Site + Pollutant ~ year(Date),
                    value.var = 'MeasuredValue', fill = NA_real_,
                    fun.aggregate = function(x) mean(x, na.rm = TRUE))

gas_dt_long = melt(gas_dt_wide,
                   id.vars = setdiff(colnames(gassingle_wide), c('2018', '2019')),
                   measure.vars = c('2018', '2019'),
                   variable.name = 'Date', value.name = 'MeasuredValue',
                   variable.factor = FALSE)

gas_dt_city = dcast(gas_dt, State + County + Date + Site + Pollutant ~ City,
                      value.var = 'MeasuredValue', fill = NA_real_,
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))

#3
gas_dt[, AveVal := mean(MeasuredValue, na.rm = TRUE), 
       by = c('Pollutant', 'State', 'Country', 'City')]