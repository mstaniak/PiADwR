library(data.table)

gold <- data.table(read.csv("data/xauusd_d.csv"))
usdpln <- data.table(read.csv("data/usdpln_d.csv"))

library(ggplot2)
pplot <- function(data1, data2, column_x, column_y, plot_type, ...){
  ggplot(data = data, aes_string(x = column_x, y = column_y)) + plot_type() + theme(...)
}

gold$Data <- as.Date(gold$Data)
pplot(gold, "Data", "Zamkniecie", geom_line, axis.text = element_text(colour = "red"), 
      axis.line = element_line(size = 2) )
