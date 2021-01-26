zmiana_theme <- function(parametr, zm_gg)
{
  for (i in 1:length(parametr))
  {
    
    if (parametr[[i]][1] == "axis.text")
    {
      if (parametr[[i]][2] == "size")
      {
        rozm <- as.numeric(parametr[[i]][3])
        zm_gg <- zm_gg + theme(axis.text <- element_text(size <- rozm))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(axis.text <- element_text(colour <- parametr[[i]][3]))
      }
    }
    
    if (parametr[[i]][1] == "axis.text.x")
    {
      if (parametr[[i]][2] == "size")
      {
        rozm <- as.numeric(parametr[[i]][3])
        zm_gg <- zm_gg + theme(axis.text.x <- element_text(size <- rozm))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(axis.text.x <- element_text(colour <- parametr[[i]][3]))
      }
    }
    
    if (parametr[[i]][1] == "axis.text.y")
    {
      if (parametr[[i]][2] == "size")
      {
        rozm <- as.numeric(parametr[[i]][3])
        zm_gg <- zm_gg + theme(axis.text.y <- element_text(size <- rozm))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(axis.text.y <- element_text(colour <- parametr[[i]][3]))
      }
    }
    
    if (parametr[[i]][1] == "title")
    {
      if (parametr[[i]][2] == "size")
      {
        rozm <- as.numeric(parametr[[i]][3])
        zm_gg <- zm_gg + theme(title <- element_text(size <- rozm))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(title <- element_text(colour <- parametr[[i]][3]))
      }
    }
    
    if (parametr[[i]][1] == "axis.title.x")
    {
      if (parametr[[i]][2] == "size")
      {
        rozm <- as.numeric(parametr[[i]][3])
        zm_gg <- zm_gg + theme(axis.title.x <- element_text(size <- rozm))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(axis.title.y <- element_text(colour <- parametr[[i]][3]))
      }
    }
    if (parametr[[i]][1] == "axis.title.y")
    {
      if (parametr[[i]][2] == "size")
      {
        rozm <- as.numeric(parametr[[i]][3])
        zm_gg <- zm_gg + theme(axis.title.y <- element_text(size <- rozm))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(axis.title.y <- element_text(colour <- parametr[[i]][3]))
      }
    }
    
    if (parametr[[i]][1] == "panel.border")
    {
      zm_gg <- zm_gg + theme(panel.border <- element_rect(fill <- NA))
      if (parametr[[i]][2] == "linetype")
      {
        zm_gg <- zm_gg + theme(panel.border <- element_rect(linetype <- parametr[[i]][3]))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(panel.border <- element_rect(colour <- parametr[[i]][3]))
      }
    }
    
    if (parametr[[i]][1] == "panel.background")
    {
      if (parametr[[i]][2] == "fill")
      {
        zm_gg <- zm_gg + theme(panel.background <- element_rect(fill <- parametr[[i]][3]))
      }
      if (parametr[[i]][2] == "colour")
      {
        zm_gg <- zm_gg + theme(panel.background <- element_rect(colour <- parametr[[i]][3]))
      }
    }
    
  }
  zm_gg
}


wykres <- function(dane, typ, parametr, wartosc)
{
  if (typ == "boxplot")
  {
    gg_plot <- ggplot(dane, aes(,wartosc)) + 
      geom_boxplot() + 
      labs(title <- "BoxPlot", 
           x <- "", 
           y=  "średnia")
  }

  else if (typ == "rozrzutu")
  {
    gg_plot <- ggplot(d, aes(State.Name,wartosc)) + 
      geom_point() + 
      theme(axis.text.x <- element_text(angle <- 90))
  } 
  
  else if (typ == "hist")
  {
    gg_plot <- ggplot(dane, aes(wartosc)) + 
      geom_histogram() + 
      labs(title <- "Histogram", 
           x <- "średnia", 
           y=  "ilość")
  }

  else if (typ == "kwantylowokwantylowy")
  {
    gg_plot <- ggplot(d, aes(sample <- wartosc)) + 
      stat_qq()  + 
      stat_qq_line()
  }
  else
  {
    print(c("Podano niepoprawny typ wykresu!"))
    return()
  }
  zmiana_theme(parametr, gg_plot)
}
