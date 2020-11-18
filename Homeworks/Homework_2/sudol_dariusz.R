# Zadanie domowe:
# 1. Dla ramek danych gas_two_wide i gas_two
# -> obliczyæ znormalizowane wartoœci MeasuredValue (odjêcie œredniej, podzielenie przed odch. std.)
# (dla gas_two w postaci w¹skiej, dla gas_two_wide w postaci szerokiej)
# dla gas_two: przez referencjê i bez referencji
# dla gas_two_wide: przy u¿yciu lapply(), przy u¿yciu referencji i lapply(), lapply() bez referencji,
# bez lapply - z referencj¹ i bez

# dla gas_two: przez referencjê
gas_two[,MeasuredValue := (MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue), by = Pollutant]

# dla gas_two: bez referencji
gas_two[, 'MeasuredValue'] <- gas_two[,(MeasuredValue - mean(MeasuredValue))/sd(MeasuredValue), by = Pollutant]$V1

#dla gas_two_wide: przy u¿yciu referencji i lapply()
gas_two_wide[, c('Ozone','SO2') := lapply(.SD, function(x) {(x-mean(x))/sd(x)}), .SDcols = c('Ozone', 'SO2')]

# dla gas_two_wide: przy u¿yciu lapply() bez referencji
gas_two_wide[, c('Ozone','SO2')] <- gas_two_wide[, lapply(.SD, function(x) {(x-mean(x))/sd(x)}), .SDcols = c('Ozone', 'SO2')]

#bez lapply - z referencj¹ i bez
standard <- function(x) {(x-mean(x))/sd(x)}
gas_two_wide[, `:=`(Ozone = standard(Ozone), SO2 = standard(SO2))]

#bez lapply i referencji
# ?

# 2. Dla dowolnego miejsca:
# - przekonwertowaæ do wersji szerokiej ze wzglêdu na ROK
# - wróciæ do wersji w¹skiej
# - przekonwertowaæ do wersji szerokiej ze wzglêdu na miasto (ró¿ne miasta w jednym stanie)
# - znormalizowaæ dla ka¿dego miasta osobno

gas_dt[, Date := format(Date, format="%m-%d")]

gas_single <- gas_dt[State == "Alabama" & County == "Jefferson" &
                       City == "Birmingham" & Site == "North Birmingham" &
                       Pollutant == "Ozone"]

gas_single[,NumPollutants := NULL]

gas_single_wide <- dcast(gas_single, State + County + City + Site + Pollutant + Date ~ Year,
                         value.var = "MeasuredValue", fill = NA_real_,
                         fun.aggregate = function(x) mean(x, na.rm = TRUE))

gas_single_long <- melt(gas_single_wide,
                        id.vars = setdiff(colnames(gas_single_wide), c("2018", "2019")),
                        measure.vars = c("2018", "2019"),
                        variable.name = "Year", value.name = "MeasuredValue",
                        variable.factor = FALSE)

gas_single_long <- as.data.table(filter(gas_single_long, !is.na(MeasuredValue)))

gas_single_2 <- as.data.table(gas_dt[State == "Alabama" & Pollutant == "Ozone"])
gas_single_2[,NumPollutants := NULL]
gas_single_2[,County := NULL]
gas_single_2[,Site := NULL]
gas_single_2
gas_single_2 <- dcast(gas_single_2, State + Date + Year + Pollutant ~ City,
                      value.var = "MeasuredValue", fill = NA_real_,
                      fun.aggregate = function(x) mean(x, na.rm = TRUE))
gas_single_2 <- as.data.table(gas_single_2)

cols <- setdiff(colnames(gas_single_2), c('State', 'Date', 'Year', 'Pollutant'))
gas_single_2[,setdiff(colnames(gas_single_2), c('State', 'Date', 'Year', 'Pollutant')):= lapply(.SD, function(x) {(x-mean(x,na.rm = T))/sd(x,na.rm = T)}), .SDcols = cols]

# 3. Dla gas_dt, zrobiæ to co na zajêciach ze œrednimi bez u¿ycia merge/join.

gas_dt[, AverageVal := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")]



















