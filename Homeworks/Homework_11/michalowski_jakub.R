# microbenchmark:
# - ramka danych (data.frame, data.table, tibble) o liczbie wierszy N,
#  liczbie grup G w zmiennej X i wartościach liczbowych w zmiennych Y, W, Z.
# - operacja: obliczanie statystyk dla zmiennych Y, W, Z według grup zmiennej X
# - trzy alternatywne implementacje: bazowy, data.table, tidyverse

# - zadanie: porównać czas wykonania tej operacji dla tych trzech implementacji,
# zmieniając liczbę wierszy ramki danych N oraz liczbę grup G.
# Narysować wykres zależności czasu (box plot, wartość typowa + zmienność)
# od N i G według implementacji.

library(microbenchmark)
library(pryr)
library(profvis)

library(ggpubr)
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(data.table)
library(ggplot2)

alphabet <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
              'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
              'R', 'S', 'T', 'U', 'W', 'X', 'Y', 'Z')

create_agg_data_frame <- function(N, G){
  dframe <- data.frame(X = alphabet[1:G],
                       Y = runif(N),
                       W = runif(N),
                       Z = runif(N)
  )
  aggregate(by=list(dframe$X), x=dframe[2:4], FUN = function(m){mean(m)/(N*G)})
}

create_agg_data_table <- function(N, G){
  dtable <- data.table(X = alphabet[1:G],
                       Y = runif(N),
                       W = runif(N),
                       Z = runif(N)
  )
  dtable[ , .(stat_Y = mean(Y)/(N*G), stat_W = mean(W)/(N*G), stat_Z = mean(Z)/(N*G)), by = X]
}

create_agg_tibble <- function(N, G){
  #Only values of size one are recycled. :/
  dframe <- data.frame(X = alphabet[1:G],
                       Y = runif(N),
                       W = runif(N),
                       Z = runif(N)
  )
  tib <- as_tibble(dframe)
  tib2 <- tib %>%
    group_by(X) %>%
    summarise(stat_Y = mean(Y)/(N*G), stat_W = mean(W)/(N*G), stat_Z = mean(Z)/(N*G))
  tib2
}

#autoplot(bench)
#boxplot(bench)

Nn <- c(100, 1000, 10000)
lista_plotow <- myplots <- vector('list', length(Nn))

for(i in length(Nn)){
  bench <- microbenchmark(
      dframe = create_agg_data_frame(Nn[i], 4),
      dtable = create_agg_data_table(Nn[i], 4),
      dtibble = create_agg_tibble(Nn[i], 4),
      times = 50
  )
  #p <- boxplot(bench)
  #lista_plotow[[i]] <- p

  lista_plotow[[i]] <- boxplot(bench)
}

#ggarrange(lista_plotow[[1]], lista_plotow[[2]], lista_plotow[[3]], nrow = 1)
#ggarrange(boxplot(bench), boxplot(bench), boxplot(bench), nrow = 1)