#hw4
library(ggplot2)
library(HRW)
data(WarsawApts)

function_plot <- function(data, x, y, type, arguments)
{
  ggplot(data, aes(x = x, y = y)) + type + theme(arguments)
}

x <- WarsawApts$construction.date
y <- WarsawApts$areaPerMzloty

plot_data(data, x, y, geom_hist(),axis.title.y = element_blank()) 