
library(ggplot2)

draw_plot <- function(data, arguments = c(NULL, NULL), plot_type, ...) {
  match.arg(plot_type, c("geom_point", "geom_histogram", "geom_boxplot"))
  switch(plot_type,
         geom_point = {
           ggplot(data, aes(x = get(arguments[1]), y = get(arguments[2]))) +
             geom_point() +
             xlab(arguments[1]) +
             ylab(arguments[2]) +
             theme(...)
         },
         geom_histogram = {
           ggplot(data, aes(x = get(arguments[1]))) +
             geom_histogram() +
             xlab(arguments[1]) +
             theme(...)
         },
         geom_boxplot = {
           ggplot(data, aes(x = as.character(get(arguments[1])), y = get(arguments[2]))) +
             geom_boxplot() +
             xlab(arguments[1]) +
             ylab(arguments[2]) +
             theme(...)
         })
}

draw_plot(airquality, c("Solar.R", "Temp"), "geom_point")
draw_plot(airquality, c("Temp"), "geom_histogram")
draw_plot(airquality, c("Day", "Temp"), "geom_boxplot")


airquality
