library(data.table)
gas_file <- list.files("./data", full.names = TRUE)
gas_dfs_dt <- lapply(gas_file, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt <- gas_dt[, c("State Name", "County Name", "City Name",
                     "Local Site Name", "Date Local", "Parameter Name",
                     "Sample Duration", "Arithmetic Mean")]


names_of_cols <- c("State", "County", "City", "Site", "Date", "Pollutant", "SampleDuration", "MeasuredValue")
setnames(gas_dt, colnames(gas_dt), names_of_cols)
head(gas_dt)

gas_dt[, NumPollutants := uniqueN(Pollutant), by = c("State", "County", "City", "Site", "Date")]

gas_two <- gas_dt[NumPollutants > 1]
gas_two_wide <- gas_two_wide <- dcast(gas_two, State + County + City + Site + Date ~ Pollutant,
                                      value.var = "MeasuredValue",
                                      fun.aggregate = function(x) mean(x, na.rm = TRUE))

setnames(gas_two_wide, c("Nitrogen dioxide (NO2)", "Sulfur dioxide"), c("NO2", "SO2"))
head(gas_two_wide)


#ZADANIE 1
normalize <- function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

##gas_two

###przez referencje
gas_two[, NormalizedValues := normalize(MeasuredValue), by = "Pollutant"]


###bez referencji

gas_two[, 'Normalized_no_ref'] = gas_two[, normalize(MeasuredValue), by = "Pollutant"][,2]


##gas_two_wide

###przez referencje + lapply

gas_two_wide[, c("Normalized_NO2", "Normalized_SO2") := lapply(.SD, normalize), .SDcols = c("NO2", "SO2")]

###przez referencje bez lapply

gas_two_wide[, c("no_lapply_Normalized_NO2", "no_lapply_Normalized_SO2") := list(normalize(NO2), normalize(SO2))]

###bez referncji + lapply

gas_two_wide[, c("noref_Normalized_NO2", "noref_Normalized_SO2")] = lapply(gas_two_wide[,c("NO2","SO2")], normalize)

###bez referencji bez lapply

gas_two_wide[, "noref_nolap_Norm_NO2"] = gas_two_wide[, normalize(NO2)]
gas_two_wide[, "noref_nolap_Norm_SO2"] = gas_two_wide[, normalize(SO2)]


#ZADANIE 2

gas_dt[, Year := year(Date)]
loc_gas <- gas_dt[c(State == "Kansas" & County == "Wyandotte" & City == "Kansas City"), ]
loc_gas[, Date := format(Date, format = "%m-%d")]

#konwertowanie ze wzglêdu na rok

wide_wrt_year <- dcast(loc_gas, State + County + City + Pollutant + Date ~ Year, 
                                   value.var = "MeasuredValue", fill = NA_real_, fun.aggregate = mean)

#powrót to wersji w¹skiej 

narrow_loc_gas <- melt(wide_wrt_year, id.vars = setdiff(colnames(wide_wrt_year), c('2015','2019')), 
                       measure.vars = c('2015', '2019'), variable.name = "Year", value.name = "MeasuredValue", 
                       variable.factor = FALSE)

#konwertowanie do szerokiej ze wzglêdu na miasto


nevada_gas <- gas_dt[c(State == "Nevada" & County == "Clark"), ]

wide_wrt_city <- dcast(nevada_gas, State + Date + Pollutant ~ City, value.var = "MeasuredValue", 
                       fill = NA_real_, fun.aggregate = function(x) mean(x, na.rm = TRUE))


#normalizowanie dla ka¿dego miasta

nevada_gas[, NormalizedValues := normalize(MeasuredValue), by = "City"]

#ZADANIE 3

gas_dt[, AverageValue := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")]

