library(ggplot2)

# dwa mozliwe typy wykresu w mojej funkcji to "bar" i "pie"

wykres_homework <- function(df, plot_type, feature, ...){
  
  base <- ggplot(df, aes(fill = eval(parse(text = feature)), x = "n"))
 #                                                             ^^^
 #                                                  df[, n := .N, by = feature]
  
  if (plot_type == "pie"){
    
    base + 
      
      geom_bar() + 
      
      xlab(feature) + 
      
      coord_polar("y", start=0) +
      
      scale_fill_discrete(name = feature) + # get proper legend name
      
      theme(...)
  }
  
  else if (plot_type == "bar"){
    
    base +
      
      geom_bar(position = "dodge") +
      
      xlab(feature) +
      
      scale_fill_discrete(name = feature) + # get proper legend name
      
      theme(...)
  }
} 

# przykladowe wywolanie
wykres_homework(df = szachy_duration,  plot_type = "bar",
                feature = "victory_status", legend.text = element_text(size = 20))

