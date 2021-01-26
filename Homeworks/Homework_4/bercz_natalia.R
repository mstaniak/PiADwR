library(ggplot2)
library(data.table)

data = read.csv('test_data.csv', sep = ';')

plot = function(data, x_variable, y_variable, title, xlabname, ylabname, type, theme, ...){
    ggplot(data, aes(x=x_variable, y=y_variable)) +
    labs(title, x=xlabname, y=ylabname) + type() + theme(...)
}

# types: geom_point, geom_violin, geom_polygon
# examples:
#plot(data, data$G1, data$age, 'plot1', 'G1', 'age', geom_point, theme_bw)
#plot(data, data$G2, data$traveltime, 'plot2', 'G2', 'traveltime', geom_violin, theme_dark)
#plot(data, data$G3, data$studytime, 'plot3', 'G3', 'studytime', geom_polygon, theme_minimal)