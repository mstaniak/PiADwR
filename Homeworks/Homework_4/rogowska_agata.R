library(data.table)
library(ggplot2)

PlotVariable <- function(data, type, variable, ...){
  if (as.character(substitute(type)) == "geom_histogram"){
    ggplot(data, aes_string(x = variable)) + 
      type() + 
      theme(...)
  }
  else if (as.character(substitute(type)) == "geom_boxplot"){
    ggplot(data, aes_string(y = variable)) + 
      type() +
      theme(...)
  }
  else {
    tryCatch({
      Sys.sleep(0.5)
      message("Error: choose a different type")
      })
  }
}

n <- 1000
dist <- data.table(N = rnorm(n),
                   U = runif(n),
                   t = rt(n, 10))

PlotVariable(dist, geom_histogram, "N")
PlotVariable(dist, geom_boxplot, "U", panel.background = element_rect(fill = "white", colour = "grey50"))
PlotVariable(dist, geom_point, "t")




