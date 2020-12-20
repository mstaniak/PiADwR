library(data.table)
library(ggplot2)
library(lubridate)
data = fread('ccc.txt')
setnames(data, 
         colnames(data), 
         c('Ticker', 'Period', 'Date', 'Time', 'Open', 'High', 'Low', 'Close', 'Volume', 'OpenInt'))

data[, Date := ymd(Date)]

plot_data = function(data, x, y, plot_type, ...){
  ggplot(data, aes_string(x = x, y = y)) + plot_type + theme(...)
}
  
plot_data(data, x='Date', y='Open', plot_type = geom_line(),
          title = element_text(size = rel(2)))