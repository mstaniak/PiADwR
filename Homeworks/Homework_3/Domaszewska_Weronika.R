###### ZADANIE 3 #####

# wczytanie danych
dane = read.csv("ścieżka")

# Dodaj do daty wylosowane godziny, minuty i sekundy. 
# Tak powstały napis przekonwertuj z powrotem na datę.

datetimes <- paste(
  as.character(dane$Date.Local),
  paste(sample(0:23, 4), sample(0:59, 4), sample(0:59, 4),
        sep = ":"))
dane$Date.Local = ymd_hms(datetimes)   # library(lubridate)

# Zaokrąglij tak utworzoną datę. 
# Oblicz typowe statystyki opisowe (średnia, median, min, max itd) dla miesiąca i roku,
# (tzn. zaokrąglij najpierw do miesiąca, potem do roku [w dół] i oblicz statystyki według takich grup)

do_miesiaca = floor_date(dane$Date.Local, unit = "month")
do_roku = floor_date(do_miesiaca, unit = "year")

# dla miesięcy
miesiace = month(do_miesiaca)
mean(miesiace)
median(miesiace)
min(miesiace)
max(miesiace)

# dla roku
rok = month(do_roku)
mean(rok)
median(rok)
min(rok)
max(rok)

# Traktując datę jako 
# a) datę
# b) napis
# wyciągnij z daty rok, dzień, miesiąc, godzinę, minutę, sekundę.
# Dodaj je jako kolumny.

# a) jako data
rok = year(dane$Date.Local) 
miesiac = month(dane$Date.Local)
dzien = day(dane$Date.Local)
godzina = hour(dane$Date.Local)
minuta = minute(dane$Date.Local)
sekunda = second(dane$Date.Local)

# b) jako napis
data_napis = as.character(dane$Date.Local)
rok2 = substr(dane$Date.Local, 1,4)
miesiac2 = substr(dane$Date.Local, 6,7)
dzien2 = substr(dane$Date.Local, 9,10)
godzina2 = substr(dane$Date.Local, 12,13)
minuta2 = substr(dane$Date.Local, 15,16)
sekunda2 = substr(dane$Date.Local, 18,19)

# dodawanie jako kolumny osobne
dane = mutate(dane, rok = year(dane$Date.Local) )
dane = mutate(dane, miesiac = month(dane$Date.Local))
dane = mutate(dane, dzien = day(dane$Date.Local))
dane = mutate(dane, godzina = hour(dane$Date.Local))
dane = mutate(dane, minuta = minute(dane$Date.Local))
dane = mutate(dane, sekunda = second(dane$Date.Local))



