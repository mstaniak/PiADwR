#tworzy wykresy dwóch zmiennych, przykladowe typy: geom_point, geom_line, geom_jitter
createPlot <- function(data, type, x, y, ...){
library('ggplot2')
plot <- ggplot(data,aes(x = eval(parse(text = x)), y = eval(parse(text = y)))) + type() + theme(...)
return(plot)
}

#przyklad wywolania
createPlot(iris,geom_point,'Sepal.Width','Sepal.Length',rect = element_rect(fill="green"), axis.line.x = element_line(color = "red", linetype = "dashed"))
createPlot(iris,geom_line,'Sepal.Width','Sepal.Length',text = element_text(size=15,color = "blue"))
createPlot(iris,geom_jitter,'Sepal.Width','Sepal.Length',axis.title.x = element_blank(), axis.title.y = element_blank())

