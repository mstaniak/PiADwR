gas_files <- list.files("./data", full.names = TRUE)
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt
setnames(gas_dt, colnames(gas_dt), c("State", "County", "City", "Site", "Date",
                                     "Pollutant", "SampleDuration", "MeasuredValue"))
gas_dt[, NumPollutants := uniqueN(Pollutant),
       by = c("State", "County", "City", "Site", "Date")]

gas_dt[, Year := year(Date)]

gas_two <- gas_dt[NumPollutants > 1]
gas_two_wide <- data.table::dcast(gas_two, State + County + City + Site + Date ~ Pollutant,
                                  value.var = "MeasuredValue",
                                  fun.aggregate = function(x) mean(x, na.rm = TRUE))
gas_two_wide
class(gas_two_wide)
setnames(gas_two_wide, "Carbon monoxide", "CO")

standarize = function(x){
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
}

# przez ref
gas_two[, std_MeasuredValue_ref := standarize(MeasuredValue), 
        by = 'Pollutant']
# bez ref
gas_two[,'std_MeasuredValue_noref'] = gas_two[,standarize(MeasuredValue), 
                                              by = 'Pollutant'][,2]
gas_two
# przez ref i z lapply
gas_two_wide[, c('std_CO_ref_lap', 'std_Ozone_ref_lap') := lapply(.SD, standarize),
             .SDcols = c('CO', 'Ozone')]
# przez ref bez lapply
gas_two_wide[, `:=` (std_CO_ref_nolap = standarize(CO),
                     std_Ozone_ref_nolap = standarize(Ozone))]
# bez ref i z lapply
gas_two_wide[, c('std_CO_noref_lap', 'std_Ozone_noref_lap')] = lapply(gas_two_wide[, c('CO', 'Ozone')], standarize)
# bez ref i bez lapply
gas_two_wide[, 'std_CO_noref_nolap'] = gas_two_wide[, standarize(CO)]
gas_two_wide[, 'std_Ozone_noref_nolap'] = gas_two_wide[, standarize(Ozone)]

# zad 2

gas_single = gas_dt[State == "Alabama" & County == "Jefferson" &
                       City == "Birmingham" & Site == "North Birmingham" &
                       Pollutant == "Ozone"]
gas_single
gas_single[, Date := format(Date, format = '%m-%d')]
gas_single

# zad 2a
z_2_a = data.table::dcast(gas_single, State + County + City + Site + Pollutant + Date  ~ Year, 
                           value.var = "MeasuredValue", fill = NA_real_)
# zad 2b
z_2_b = data.table::melt(z_2_a, id.vars = setdiff(colnames(z_2_a), c('2019', '2020')),
                          measure.vars = c('2019', '2020'),
                          variable.name = 'Year',
                          value.name = 'MeasuredValue', variable.factor = F, na.rm = T)

# zad 2c
gas_single2 = gas_dt[State == "Alabama" & County == 'Jefferson' & Pollutant == "Ozone"]
gas_single2
z_2_c = dcast.data.table(gas_single2, State + Date + Pollutant ~ City,
                         value.var = 'MeasuredValue', fill = NA_real_,
                         fun.aggregate = function(x) mean(x, na.rm = T))
# zad 2d
z_2_d = gas_single2[, MeasuredValue_std := standarize(MeasuredValue), by = c('City')]

# zad 3
gas_dt_with_avgs <- gas_dt[, AverageVal := mean(MeasuredValue, na.rm = TRUE), 
                           by = c("Pollutant", "State", "Year")]
