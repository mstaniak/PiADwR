library(data.table)
library(ggplot2)

## wczytanie danych

dt <- fread("Joined_Data_mod.csv", drop = 1)

## funkcja

gg_plot_function <- function(Data, X, Y, plot_type = "geom_point", ...){
  if(plot_type == "geom_point" | plot_type == "geom_jitter" | plot_type == "geom_boxplot")
    ggplot(Data, aes(x = X, y = Y)) +
      get(plot_type)() +
      theme(...)
  else return("Wrong plot_type")
}

## dzialanie

gg_plot_function(dt, as.factor(dt[, FTAG]), dt[, AwayTeam], "geom_point")
gg_plot_function(dt, (dt[, FTAG]), dt[, AwayTeam], "geom_boxplot")
gg_plot_function(dt, as.factor(dt[, FTAG]), dt[, AwayTeam], "geom_jitter", panel.background = element_rect(fill = "white", colour = "grey50"))
gg_plot_function(dt, (dt[, FTAG]), dt[, AwayTeam], "geom_violin")
