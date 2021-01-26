library(lubridate)
library(dplyr)

dane1 = read.csv("ścieżka do pliku")
datetimes <- paste(
  as.character(dane1$Date.Local),
  paste(sample(0:23, 4), sample(0:59, 4), sample(0:59, 4),
        sep = ":"))
datetimes <-ymd_hms(datetimes, tz = "UTC") #konwertowanie na datę

dane1$Date.Local=datetimes #zastępowanie kolumny date.local datą z wylosowaną godziną

msc = floor_date(dane1$Date.Local, unit = "month") #zaokrąglania miesiąca w dół
rok = floor_date(msc, unit = "year") #zaokrąglania roku w dół

#średnie
mean(month(msc))
mean(year(rok))

#mediany
median(month(msc))
median(year(rok))

#min
min(month(msc))
min(year(rok))

#max
max(month(msc))
max(year(rok))

#data
r = year(dane1$Date.Local)
m = month(dane1$Date.Local)
d = day(dane1$Date.Local)
h = hour(dane1$Date.Local)
mi = minute(dane1$Date.Local)
s = second(dane1$Date.Local)

#dodawanie kolumn do danych
dane1 <- mutate(dane1, rok = year(dane1$Date.Local))
dane1 <- mutate(dane1, m = month(dane1$Date.Local))
dane1 <- mutate(dane1, d = day(dane1$Date.Local))
dane1 <- mutate(dane1, h = hour(dane1$Date.Local))
dane1 <- mutate(dane1, mi = minute(dane1$Date.Local))
dane1 <- mutate(dane1, s = second(dane1$Date.Local))

#napis
data_napis = as.character(dane1$Date.Local)
y2 = substr(dane1$Date.Local, 1,4)
m2 = substr(dane1$Date.Local, 6,7)
d2 = substr(dane1$Date.Local, 9,10)
h2 = substr(dane1$Date.Local, 12,13)
mi2 = substr(dane1$Date.Local, 15,16)
s2 = substr(dane1$Date.Local, 18,19)
