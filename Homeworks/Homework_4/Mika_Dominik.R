library(ggplot2)
library(data.table)



ggdt <- function(data, x, y, type = geom_point, theme = theme()){

ggplot(data, aes(x = x, y = y)) +
    type() +
    theme_bw() +
    theme
}



ggdt(gas_dt, gas_dt[,Pollutant], gas_dt[, MeasuredValue], geom_point, theme())

