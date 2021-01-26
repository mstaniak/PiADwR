data <- read.csv("wig20_d.csv")
library(ggplot2)
rysuj <- function(dane, kolumna_x, kolumna_y, typ_wykresu, ...)
{
  ggplot(dane, aes_string(x=kolumna_x, y=kolumna_y)) + eval(parse(text=typ_wykresu)) + theme(...)
}

rysuj(dane = data, kolumna_x = "Otwarcie", kolumna_y = "Zamkniecie", typ_wykresu = "geom_line()", plot.background = element_rect(fill = "green"))
