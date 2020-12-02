gas_files = list.files('./data/gas_data', full.names = TRUE)
gas_dfs_dt = lapply(gas_files, fread)
gas_dt = rbindlist(gas_dfs_dt)

sample_hour = function(date){
  hour = paste(sample(1:24,1), sample(0:59,1), sample(0:59,1), sep = ':')
  date = paste(as.character(date), hour, sep = ' ')
  strftime(date, format = '%Y-%m-%d %H:%M:%OS')
}

gas_dt[, Date := floor_date(Date, unit = 'month')]
gas_dt[, .(Mean = mean(MeasuredValue), 
           Median = median(MeasuredValue), 
           Min = min(MeasuredValue), 
           Max = max(MeasuredValue)), 
       by = Date]

gas_dt[, Date := floor_date(Date, unit = 'year')]
gas_dt[, .(Mean = mean(MeasuredValue), 
           Median = median(MeasuredValue), 
           Min = min(MeasuredValue), 
           Max = max(MeasuredValue)), 
       by = Date]

gas_dt[, 'Date Local' := lapply(gas_dt[, 'Date Local'], sample_hour)]
dat_funct = function(chr){
  as.POSIXct(chr, format = '%Y-%m-%d %H:%M:%OS')
}
gas_dt[, 'Date Local' := lapply(gas_dt[, 'Date Local'], dat_funct)]

gas_dt[, Year := lapply(gas_dt[, 'Date Local'], year)]
gas_dt[, Month := lapply(gas_dt[, 'Date Local'], month)]
gas_dt[, Day := lapply(gas_dt[, 'Date Local'], day)]
gas_dt[, Hour := lapply(gas_dt[, 'Date Local'], hour)]
gas_dt[, Minut := lapply(gas_dt[, 'Date Local'], minute)]
gas_dt[, Second := lapply(gas_dt[, 'Date Local'], second)]