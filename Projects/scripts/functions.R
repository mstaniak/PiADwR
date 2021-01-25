
ZoomDataRange <- function(d, input1, input2){
  if (input1 == "3D"){
    c(as.Date(max(d$Date))-2, as.Date(max(d$Date)))
  }
  else if (input1 == "7D"){
    c(as.Date(max(d$Date))-6, as.Date(max(d$Date)))
  }
  else if (input1 == "1M"){
    c(as.Date(max(d$Date)) %m-% months(1), as.Date(max(d$Date)))
  }
  else if (input1 == "3M"){
    c(as.Date(max(d$Date)) %m-% months(3), as.Date(max(d$Date)))
  }
  else if (input1 == "6M"){
    c(as.Date(max(d$Date)) %m-% months(6), as.Date(max(d$Date)))
  }
  else if (input1 == "1Y"){
    c(as.Date(max(d$Date)) %m-% years(1), as.Date(max(d$Date)))
  }
  else if (input1 == "Range"){
    input2
  }
  else if (input1 == "All"){
    c(as.Date(min(d$Date)), as.Date(max(d$Date)))
  }
}

PrepareZoomDataOHLC <- function(d, data_range){
  d[Date <= data_range[2] & Date >= data_range[1], .(Date, Open, High, Low, Close)]
}

GetZoomTableOHLC <- function(d){
  DT::datatable(d[, .(Date, Open, High, Low, Close)][order(Date, decreasing = TRUE)], 
                options = list(paging = FALSE, searching = FALSE))
}

PlotLineChart <- function(d, input1){
  lineChart(d[, .(Date, get(input1))], line.type = 'h', name = "Price", theme = chartTheme("white", up.col = "#f2a900"))
}

PlotCandlestickChart <- function(d, avg, periods){
  chartSeries(d, type = "candlesticks", name = "Candlestick chart", theme = chartTheme('white', up.col = "green", dn.col = "red"))
  if (avg == 1){
    plot(addSMA(n = as.numeric(periods), overlay = TRUE, col = "blue"))
  }
  else if (avg == 2){
    plot(addWMA(n = as.numeric(periods), overlay = TRUE, col = "orange"))
  }
  else if (avg == 3){
    plot(addEMA(n = as.numeric(periods),  overlay = TRUE, col = "green"))
  }
}

GetRateData <- function(d, type, period, data_range){
  if (period != "all"){
    periodReturn(d[Date <= data_range[2] & Date >= data_range[1], .(Date, Open, High, Low, Close)],
                 type = type,
                 period = period)
  } 
  else {
    allReturns(d[Date <= data_range[2] & Date >= data_range[1], .(Date, Open, High, Low, Close)],
               type = type)
  }
}

PlotRate <- function(d){
  lineChart(d, line.type = 'h', name = "Rates of return", 
            theme = chartTheme("white", up.col = "#f2a900"))
}

changeColor <- function(d, price){
  diffrence <- abs(d[which.max(as.Date(Date)), round(get(price), 2)] - d[which.max(as.Date(Date)) - 1, round(get(price), 2)])
  if(d[which.max(as.Date(Date)), round(get(price), 2)] > d[which.max(as.Date(Date)) - 1, round(get(price), 2)]){
    infoBox(
      title = paste(price, " price"), 
      value = tags$p(d[which.max(as.Date(Date)), round(get(price), 2)], "$", style = "font-size: 150%;"), 
      icon = icon("arrow-up"),
      subtitle = tags$a(icon("arrow-up"), paste(round((diffrence/d[which.max(as.Date(Date)) - 1, round(get(price), 2)])*100,2), "%"), style = "color: white"),
      color = "green", fill = TRUE
    )
  }
  else{
    infoBox(
      title = paste(price, " price"), 
      value = tags$p(d[which.max(as.Date(Date)), round(get(price), 2)], "$", style = "font-size: 150%;"), 
      icon = icon("arrow-down"),
      subtitle = tags$a(icon("arrow-down"), paste(round((diffrence/d[which.max(as.Date(Date)) - 1, round(get(price), 2)])*100,2), "%"), style = "color: white"),
      color = "red", fill = TRUE
    )
  }
}


