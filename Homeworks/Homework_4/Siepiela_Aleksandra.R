library(data.table)
library(readr)
library(ggplot2)
d<-read_csv("C:/Users/Olka/Desktop/SEMESTR 1M/Programowanie i analiza dnych w R/Projekt/archive/bitstampUSD_1-min_data_2012-01-01_to_2020-09-14.csv")
d<-as.data.table(d)
d2<-na.omit(d)
DT<- d2[1:200, ]

choose_plot<-function(data, plot_type, X, Y, plot_title, p, p2, p3, p4, p5){
  if(plot_type == "point"){
    ggplot(data, aes(x = X, y = Y)) +
    geom_point() +
    labs(title = plot_title) + 
    theme(plot.background = p, 
          panel.background = p2,
          plot.title = p3, 
          axis.line = p4,
          axis.text = p5)
  }else if(plot_type == "line"){
    ggplot(data, aes(x = X, y = Y)) +
    geom_line() +
    labs(title = plot_title) + 
    theme(plot.background = p, 
          panel.background = p2,
          plot.title = p3, 
          axis.line = p4,
          axis.text = p5)
  }else{
    ggplot(data, aes(x = X, y = Y)) +
    geom_step() +
    labs(title = plot_title) + 
    theme(plot.background = p, 
          panel.background = p2,
          plot.title = p3, 
          axis.line = p4,
          axis.text = p5)
  }
}

choose_plot(DT, "line", DT$`Volume_(BTC)`, DT$`Volume_(Currency)`, "Wykres",
            element_rect(colour = "red"),
            element_rect(fill = "#BFD5E3", colour = "#6D9EC1", size = 2, linetype = "solid"),
            element_text(size = rel(2)),
            element_line(size = 2, linetype = "dotted"),
            element_text(colour = "blue"))

choose_plot(DT, "point", DT$Weighted_Price, DT$`Volume_(BTC)`, "Wykres 2",
           element_rect(fill = "deeppink"),
           element_rect(fill = "pink", colour = "green", size = 5, linetype = "solid"),
           element_text(color="green", size=14, face="bold.italic"),
           NULL,
           element_text(colour = "green"))


choose_plot(DT, "step", DT$Weighted_Price, DT$Open, "Wykres 3",
            element_rect(color = "darkblue", size = 4, linetype = "solid"),
            element_rect(fill = "lightblue", colour = "darkblue", size = 4, linetype = "dotted"),
            element_text(hjust = 0.5, color="darkblue", size=20, face="bold"),
            NULL,
            element_text(colour = "darkblue", size=10))

