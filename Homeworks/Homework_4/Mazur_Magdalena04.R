
library(readr)
library(data.table)
library(lubridate)
library(plotly)

dane = read_csv("D:/KRUSZYN/PiADwR/bitstampUSD_1-min_data_2012-01-01_to_2020-09-14.csv")
dane = as.data.table(dane)
dane = na.omit(dane)
dane_v2 = dane[1:500, ]

dane_v2[, Date := as.POSIXct(as.numeric(dane_v2$Timestamp), origin = '1970-01-01', tz = 'GMT')]
dane_v2[, YMD := ymd(as.Date(Date))]

diff_plot = function(plot_type, data, variable_x, variable_y, ...){
  if(plot_type == "hist"){
    ggplot(data, aes(x = variable_x, y = variable_y)) +
      geom_boxplot()+
      theme(...)
  }
  else if(plot_type == "line"){
    ggplot(data, aes(x = variable_x, y = variable_y)) +
      geom_line()+
      theme(...)
  }
  else if(plot_type == "point"){
    ggplot(data, aes(x = variable_x, y = variable_y)) +
      geom_point()+
      theme(...)
  }
}

diff_plot("hist", dane_v2, dane_v2$YMD, dane_v2$Weighted_Price, panel.grid = element_blank())
diff_plot("line", dane_v2, dane_v2$YMD, dane_v2$High, axis.ticks.length = unit(2, "lines"))
diff_plot("point", dane_v2, dane_v2$YMD, dane_v2$Close, axis.text = element_text(color = "grey25"))
