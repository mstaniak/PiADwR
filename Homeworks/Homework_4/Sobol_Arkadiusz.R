library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(ggplot2)

# zadanie domowe
# zadanie domowe: napisać funkcję, która dla danych z Waszego projektu 
# (ew. danych z zajęć) narysuje wykres zadanego typu (parametr) dla 
# zadanej zmiennej (parametr, napis) i pozwoli dodać dowolne parametry 
# graficzne dla theme()

# używam tabel z ostatnich zajęć z ggplot'a
just_carbon
alameda

plotVariables <- function(data, geom_type = geom_point(), variables, ...){
  if (length(variables) == 2) {
    ggplot(data, aes(x = eval(parse(text = variables[1])),
                     y = eval(parse(text = variables[2])))) +
      geom_type + 
      xlab(variables[1]) +
      ylab(variables[2]) +
      theme(...)
  }
  else if(length(variables) == 1){
    ggplot(data, aes(x = eval(parse(text = variables[1])))) +
      geom_type + 
      xlab(variables[1]) +
      theme(...)
  }
}

plotVariables(data =alameda[Pollutant == "Carbon monoxide"],
              geom_point(),
              variables = c("Date", "MeasuredValue"))

plotVariables(data =alameda[Pollutant == "Carbon monoxide"],
              geom_point(),
              variables = c("Date", "MeasuredValue"),
              axis.title.x = element_text(angle = 90))

plotVariables(just_carbon,
              geom_histogram(),
              variables = c("MeasuredValue"))




