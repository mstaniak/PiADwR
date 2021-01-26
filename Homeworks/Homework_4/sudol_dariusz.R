library(data.table)
library(ggplot2)

# dane:
# https://archive.ics.uci.edu/ml/datasets/Student+Performance

d1 <- fread("student-mat.csv")
d2 <- fread("student-por.csv")

custom_plot <- function(data, type, var_x, var_y, jitter = 0, ...) {
  if (type == "bar") {
    ggplot(data, aes_string(x = var_x)) +
      geom_bar() +
      theme(...)
  } else if (type == "box") {
    ggplot(data, aes_string(x = var_x, y = var_y)) +
      geom_boxplot() +
      theme(...)
  } else if (type == "point") {
    ggplot(data, aes_string(x = var_x, y = var_y)) +
      geom_point() +
      geom_jitter(width = jitter) +
      theme(...)
  } else {
    stop("wrong plot type")
  }
}

custom_plot(d2, "bar", "guardian")
custom_plot(d2, "box", "as.factor(school)", "G1")
custom_plot(d2, "box", var_y = "G1")
custom_plot(d2, "point", "famrel", "G1", jitter = 0.1,
        panel.background = element_rect(fill = "grey80", colour = "red"),
        panel.grid.major.x = element_line(size = .5, linetype = "dashed", colour = "red"))
custom_plot(d2, "abc", "guardian")
