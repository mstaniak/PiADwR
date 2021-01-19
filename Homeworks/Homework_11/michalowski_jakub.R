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

library(cowplot)
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

Nn <- c(100, 1000, 10000, 100000)
lista_plotow <- myplots <- vector('list', length(Nn))

for(i in 1:length(Nn)){
  bench <- data.table(microbenchmark(
      dframe = create_agg_data_frame(Nn[i], 4),
      dtable = create_agg_data_table(Nn[i], 4),
      dtibble = create_agg_tibble(Nn[i], 4),
      times = 40
  ))
  #lista_plotow[[i]] <- bench[, .(mean = mean(time), min = min(time), max = max(time)), by=expr]
  p <- ggplot(bench, aes(y = time, x = expr, group=expr)) + geom_boxplot() + ylim(0, 1.4*10^8)
  lista_plotow[[i]] <- p
}

ggarrange(lista_plotow[[1]], lista_plotow[[2]], lista_plotow[[3]], lista_plotow[[4]],
          nrow = 1, ncol = 4,
          align = 'hv', labels = c('N=10^2', 'N=10^3', 'N=10^4', 'N=10^5'))


# Y axis scale not udjusted ----
lista_plotow22 <- myplots <- vector('list', length(Nn))

for(i in 1:length(Nn)){
  bench <- data.table(microbenchmark(
      dframe = create_agg_data_frame(Nn[i], 4),
      dtable = create_agg_data_table(Nn[i], 4),
      dtibble = create_agg_tibble(Nn[i], 4),
      times = 40
  ))
  #lista_plotow[[i]] <- bench[, .(mean = mean(time), min = min(time), max = max(time)), by=expr]
  p <- ggplot(bench, aes(y = time, x = expr, group=expr)) + geom_boxplot()
  lista_plotow22[[i]] <- p
}
cowplot::plot_grid(lista_plotow22[[1]], lista_plotow22[[2]],
                   lista_plotow22[[3]], lista_plotow22[[4]], align = "v", ncol = 2, nrow = 2)

## all combined ----
ggarrange(ggarrange(lista_plotow[[1]], lista_plotow[[2]], lista_plotow[[3]], lista_plotow[[4]],
                    nrow = 1, ncol = 4,
                    align = 'hv', labels = c('N=10^2', 'N=10^3', 'N=10^4', 'N=10^5')),
          ggarrange(lista_plotow22[[1]], lista_plotow22[[2]],
                   lista_plotow22[[3]], lista_plotow22[[4]], align = "v", ncol = 2, nrow = 2))