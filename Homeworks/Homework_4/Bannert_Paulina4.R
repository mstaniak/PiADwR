library(ggplot2)
#zadanie zostanie wykonane na kolumnie MeasuredValue z tabeli gas_dt 
data <- gas_dt


plott <- function(dane, typ)
{
  if (typ == "histogram")
  {
    p = ggplot(dane, aes(MeasuredValue)) + 
      geom_histogram() + 
      labs(title = "Histogram", 
           y =  "liczba obserwacji")
  }
  else if (typ == "box_plot")
  {
    p = ggplot(dane, aes(MeasuredValue)) + 
      geom_boxplot()
  }
  else if (typ == "rozrzut")
  {
    p = ggplot(dane, aes(State,MeasuredValue)) + 
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90))
  }
  return(p)
}


plott(data, "histogram")
plott(data, "box_plot")
plott(data, "rozrzut")
