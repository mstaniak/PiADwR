##### ZADANIE 4 #####

formatowanie = function(param, zm_gg)
{
  # param ma długość n a kazdy element dlugość 3
  # param = list(c(x1,x2,x3),c(y1,y2,y3),...)
  # element_text() -> title, axis.title, axis.text : size, colour
  # element_rect() -> panel.background, panel.border (fill=NA)
  
  n = length(param)

  for (i in 1:n)
  {
    # title / axis.title
    if (param[[i]][1] == "title")
    {
      if (param[[i]][2] == "size")
      {
        rozm = as.numeric(param[[i]][3])
        zm_gg = zm_gg + theme(title = element_text(size = rozm))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(title = element_text(colour = param[[i]][3]))
      }
    }
    if (param[[i]][1] == "axis.title.x")
    {
      if (param[[i]][2] == "size")
      {
        rozm = as.numeric(param[[i]][3])
        zm_gg = zm_gg + theme(axis.title.x = element_text(size = rozm))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(axis.title.y = element_text(colour = param[[i]][3]))
      }
    }
    if (param[[i]][1] == "axis.title.y")
    {
      if (param[[i]][2] == "size")
      {
        rozm = as.numeric(param[[i]][3])
        zm_gg = zm_gg + theme(axis.title.y = element_text(size = rozm))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(axis.title.y = element_text(colour = param[[i]][3]))
      }
    }
    # axis.tekst
    if (param[[i]][1] == "axis.text")
    {
      if (param[[i]][2] == "size")
      {
        rozm = as.numeric(param[[i]][3])
        zm_gg = zm_gg + theme(axis.text = element_text(size = rozm))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(axis.text = element_text(colour = param[[i]][3]))
      }
    }
    if (param[[i]][1] == "axis.text.x")
    {
      if (param[[i]][2] == "size")
      {
        rozm = as.numeric(param[[i]][3])
        zm_gg = zm_gg + theme(axis.text.x = element_text(size = rozm))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(axis.text.x = element_text(colour = param[[i]][3]))
      }
    }
    if (param[[i]][1] == "axis.text.y")
    {
      if (param[[i]][2] == "size")
      {
        rozm = as.numeric(param[[i]][3])
        zm_gg = zm_gg + theme(axis.text.y = element_text(size = rozm))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(axis.text.y = element_text(colour = param[[i]][3]))
      }
    }
    # panel.background
    if (param[[i]][1] == "panel.background")
    {
      if (param[[i]][2] == "fill")
      {
        zm_gg = zm_gg + theme(panel.background = element_rect(fill = param[[i]][3]))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(panel.background = element_rect(colour = param[[i]][3]))
      }
    }
    # panel.border
    if (param[[i]][1] == "panel.border")
    {
      zm_gg = zm_gg + theme(panel.border = element_rect(fill = NA))
      if (param[[i]][2] == "linetype")
      {
        zm_gg = zm_gg + theme(panel.border = element_rect(linetype = param[[i]][3]))
      }
      if (param[[i]][2] == "colour")
      {
        zm_gg = zm_gg + theme(panel.border = element_rect(colour = param[[i]][3]))
      }
    }
  }
  zm_gg
}



wykresy = function(dane, typ_wykresu, parametry_graficzne)
{
  # wartości to kolumna Arithmetic.Mean z tabeli dane
  # typ_wykresu: histogram, box_plot, liniowy, kolowy 
  # parametry_graficzne - lista złożona z trzech konkretnych parametrów 
  # czyli list(c(typ_parametru_graficznego, co_ustawiamy, ustalona_wartość),...) 
  
  if (typ_wykresu == "histogram")
  {
    gg = ggplot(dane, aes(Arithmetic.Mean)) + 
      geom_histogram() + 
      labs(title = "Histogram", 
           x = "średnia", 
           y=  "ilość")
  }
  else if (typ_wykresu == "box_plot")
  {
    gg = ggplot(dane, aes(,Arithmetic.Mean)) + 
      geom_boxplot() + 
      labs(title = "BoxPlot", 
           x = "", 
           y=  "średnia")
  }
  else if (typ_wykresu == "kwantylkwantyl")
  {
    gg = ggplot(d, aes(sample = Arithmetic.Mean)) + 
      stat_qq()  + 
      stat_qq_line()
  }
  else if (typ_wykresu == "rozrzutu")
  {
    gg = ggplot(d, aes(State.Name,Arithmetic.Mean)) + 
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90))
  }
  else
  {
    print(c("Nie ma wykresu podanego typu!"))
    return()
  }
  formatowanie(parametry_graficzne, gg)
}

### wywyołanie ###
# d = daily_42101_2019
wek = list(c("axis.title.x","size",10),c("panel.border","colour","red"),c("panel.border","linetype","dashed"),c("panel.background","fill","pink"))
wykresy(d, "histogram", wek)
wykresy(d, "box_plot", wek)
wykresy(d, "kwantylkwantyl", wek)
wykresy(d, "rozrzutu", wek)
wykresy(d, "costam", wek)
