library(data.table)
library(plyr)
library(lubridate)
library(readr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(leaflet)
library(RColorBrewer)

load("./global_terrorism.rda")
# nazwy kolumn do wyjęcia z tabeli
selected_cols = c(
  "eventid", "iyear", "imonth", "iday", "country_txt", 
  "region_txt","city","crit1", "crit2", "crit3","success", 
  "suicide","attacktype1_txt", "attacktype2_txt", 
  "attacktype3_txt", "nperps", "nperpcap", "weaptype1_txt",
  "weapsubtype1_txt", "weaptype2_txt", "weapsubtype2", 
  "weapsubtype2_txt", "weaptype3", "weaptype3_txt", 
  "weapsubtype3_txt",  "weaptype4_txt", "weapsubtype4_txt", 
  "nkill", "nkillter", "nwound", "property", "propvalue",
  "latitude","longitude","summary"
  )
dt = dt[, ..selected_cols]

### kod Arka ----
correlated_variables = list(
  "successful attack" = "success",
  "suicide attack" = "suicide",
  'number of perpetrators' = "nperps", 
  "number of perpetrators captured" = "nperpcap",
  "total number of fatalities" = "nkill", 
  "number of perpetrator fatalities" = "nkillter",
  "total number of injured" = "nwound",
  "value of property damage" =  "propvalue",
  "year" = "iyear",
  "month" = "imonth",
  "day" = "iday"
  )
correlated_variables_titles = list(
  "success" = "successful attack",
  "suicide" = "suicide attack",
  "nperps" = 'number of perpetrators', 
  "nperpcap" = "number of perpetrators captured",
  "nkill" = "total number of fatalities", 
  "nkillter" = "number of perpetrator fatalities",
  "nwound" = "total number of injured",
  "propvalue" = "value of property damage",
  "iyear" = "year" ,
  "imonth" =  "month",
  "iday" = "day"
)
# zmiana na factory
variables_to_do_factor <- c("country_txt",
                            "region_txt",
                            "city",
                            "crit1",
                            "crit2",
                            "crit3",
                            "success",
                            "suicide",
                            "attacktype1_txt",
                            "attacktype2_txt",
                            "attacktype3_txt",
                            "weaptype1_txt",
                            "weaptype2_txt",
                            "weaptype3_txt")
for (var_name in variables_to_do_factor) {
  eval(parse(text = paste0("dt[,", var_name, " := as.factor(", var_name,")]")))
}

# zmiana wartości w kilku zmiennych 
dt[, nperps := ifelse(is.na(nperps) | nperps < 0, 1, nperps)] # zakładamy sobie, że minumum zawsze jest jeden sprawca
dt[, nwound := as.integer(nwound)]
dt[, propvalue := ifelse(propvalue < 0, NA, propvalue)]
dt[, propvalue := as.numeric(propvalue)]
dt[, nperpcap := as.integer(nperpcap)]
dt[, nperpcap := ifelse(nperpcap < 0, NA, nperpcap)]


# posumowanie po kraju paru statystyk: ncases, nkill, nkillter, nwound, propvalue
sum_of_var_Dt <- dt[, .(
  ncases = .N, 
  nkill = sum(nkill, na.rm = TRUE),
  nkillter = sum(nkillter, na.rm = TRUE) ,
  nwound = sum(nwound, na.rm = TRUE), 
  propvalue = sum(propvalue, na.rm = TRUE)),
  by = "country_txt"]

# zmiana nazwy kolumny
setnames(sum_of_var_Dt, "country_txt", "country")

# posortowany lista krajów
countries_list <- sort(as.character(sum_of_var_Dt$country))

# zwraca tabelę do zakładki National Statisticks 
get_national_statisticks_table <- function(data = sum_of_var_Dt, filter, sort, countries){
  data[data$country %in% countries][order(eval(parse(text = filter)), decreasing = sort)]
}

## attack type ----
sorting <- function(x, n){
  if(x == 'TRUE') n
  else -n
}

# maps ----

#zamiana wspolrzednych geograficznych na numeric
dt[,longitude := as.numeric(str_replace(longitude,",","."))]
dt[,latitude := as.numeric(str_replace(latitude,",","."))]

#nowa kolumna data
dt[, 
   date := ifelse(imonth == 0, 
                  iyear, 
                  ifelse(iday == 0, 
                         paste(imonth,iyear,sep="/"), 
                         paste(iday,imonth,iyear,sep="/")))]

#jesli summary jest puste to wpisz tam date, kraj, miasto i typ ataku
dt[summary == "", 
   summary := paste(date, 
                    country_txt, 
                    city, 
                    paste("attack type:",attacktype1_txt), 
                    sep = ", ")]

#poprawa blednego sklasyfikowania zdarzenia do kraju oraz nadanie Andorze wspolrzednych geograficznych
dt[latitude == 45.420943, `:=`(country_txt = "Canada", region_txt = "North America")]
dt[country_txt == "Andorra", `:=`(latitude = 42.504588, longitude = 1.521744)]

#uzupelnienie brakujacych wspolrzednych przez srednie dla krajow
dt[, `:=`(mean_country_lat = mean(latitude, na.rm = TRUE),
          mean_country_long = mean(longitude, na.rm=TRUE)), by = country_txt]
dt[is.na(latitude), latitude := mean_country_lat]
dt[is.na(longitude), longitude := mean_country_long]

#dodanie koloru markera wzgledem typu ataku
circle_colors <- c("red","navy", "orange", "yellow",
                   "forestgreen","plum","darkmagenta",
                   "darkred","grey50")
attacktypes <- levels(dt[,attacktype1_txt])
attacktype_palette <- colorFactor(palette = circle_colors, levels = attacktypes)

##### KASIA ----
plyrFxCount <- function(x, name="count") {
  df <- data.frame( nrow(x) )
  colnames(df)[1] <- name
  return(df)
}

plyrFxSum <- function(x, toSum, name="sum") {
  df <- data.frame( sum(x[toSum]) )
  colnames(df)[1] <- name
  return(df)
}

# rename some columns
dat <- copy(dt)
setnames(dat, 
         c("iyear","imonth","iday","attacktype1_txt"),
         c("year","month","day", "attacktype"))

# reorder region levels by total number of attacks in each region
regionAttackOrder = order(table(dt$region), decreasing=TRUE)
regionAttackLevels = names(table(dt$region))[regionAttackOrder]
dt$region <- factor(dt$region, levels = regionAttackLevels)
regions_list <- levels(dt$region)

#MY COLOR PALETTE
regionCol <- c(brewer.pal(9, name="Set1")[c(-6, -9)], 
               '#EEC900', '#00CED1','#7FFF00','#E9967A', '#2F4F4F')

# reorder attack types by total number of attacks per type
attackTypeOrder = order(table(dt$attacktype), decreasing=TRUE)
attackTypeLevels = names(table(dt$attacktype))[attackTypeOrder]
dt$attacktype <- factor(dt$attacktype, levels = attackTypeLevels)
dat <- within(dat, 
              attacktype <- revalue(attacktype,
                                    c("Hostage Taking (Kidnapping)" = "Hostage (Kidnapping)",
                                      "Facility/Infrastructure Attack" = "Facility Attack",
                                      "Hostage Taking (Barricade Incident)" = "Hostage (Barricade)"
                                         )))
dat <- within(dat, 
              region <- revalue(region_txt,
                                c("Australasia & Oceania" = "Oceania",
                                  "Central America & Caribbean" = "Central America",
                                  "Middle East & North Africa" = "Middle East")))

dat$nkill[is.na(dat$nkill)] <- 0
dat$nwound[is.na(dat$nwound)] <- 0

#write.table(dat, "globalterrorismdb_clean.csv", sep=",", col.names=TRUE, row.names=FALSE,quote=which(colnames(dat) == 'city'))

regionTotal <- ddply(dat, ~region, plyrFxCount)
regionTotal <- as.data.table(regionTotal)

regionCol <- c(brewer.pal(9, name="Set1")[c(-6, -9)], 
               '#EEC900', '#00CED1','#7FFF00','#E9967A', '#2F4F4F')

regions = levels(dat$region)
regionYear <- ddply(dat, region ~ year, plyrFxCount, "nattacks")
regionYearPossibilities <- merge(regions, unique(dat$year))
regionYear <- merge(regionYear, regionYearPossibilities,
                    by.x = c('region','year'), by.y = c("x","y"), all.y = TRUE)
regionYear$nattacks[is.na(regionYear$nattacks)] <- 0
str(regionYear)
regionYear <- as.data.table(regionYear)