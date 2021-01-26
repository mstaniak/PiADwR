library(ggplot2)
library(data.table)

# import danych
test_data <- fread("../Homework_4/grabias_test_data.csv")

# dwa mozliwe typy wykresu w mojej funkcji to "bar" i "pie"

wykres_homework <- function(df, plot_type, feature, ...){
  
  base <- ggplot(df[, n := .N, by = feature], 
                 aes(fill = eval(parse(text = feature)), 
                 x = "n"))
  
  # usuwamy "dummy column"
  df[, n := NULL]
  
  if (plot_type == "pie"){
    
      base + 
      
      geom_bar() + 
      
      xlab(feature) + 
      
      coord_polar("y", start=0) +
      
      scale_fill_discrete(name = feature) +
      
      theme(...)
  }
  
  else if (plot_type == "bar"){
    
      base +
      
      geom_bar(position = "dodge") +
      
      xlab(feature) +
      
      scale_fill_discrete(name = feature) +
      
      theme(...)
  }
} 

# przykladowe wywolanie
wykres_homework(df = test_data,  plot_type = "pie",
                feature = "victory_status", legend.text = element_text(size = 20))

