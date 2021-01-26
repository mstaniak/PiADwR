library(data.table)
library(ggplot2)

awm <- fread("awm.csv")
colnames(awm)

plot_function <- function(data, x, y, plot_type, ...){
  if (plot_type %in% c("geom_line()", "geom_point()", "geom_step()")){
    ggplot(data = data, aes_string(x = x, y = y)) + eval(parse(text = plot_type)) + theme(...)
  }else {
    return("Choose one of the following types of plot: geom_line(), geom_point(), geom_step().")
  }
}

plot_function(data = awm, x = "open", y = "close", plot_type = "geom_point()")
plot_function(data = awm, x = "open", y = "close", plot_type = "geom_line()")
plot_function(data = awm, x = "open", y = "close", plot_type = "geom_step()")
plot_function(data = awm, x = "open", y = "close", plot_type = "geom_line()", axis.title.x = element_blank(), axis.title.y = element_blank())
plot_function(data = awm, x = "open", y = "close", plot_type = "geom_area()")
