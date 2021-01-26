# Zadanie domowe. ----
# microbenchmark:
# - ramka danych (data.frame, data.table, tibble) o liczbie wierszy N,
#   liczbie grup G w zmiennej X i wartościach liczbowych w zmiennych Y, W, Z.
# - operacja: obliczanie statystyk dla zmiennych Y, W, Z według grup zmiennej X
# - trzy alternatywne implementacje: bazowy, tidyverse, data.table
# - zadanie: porównać czas wykonania tej operacji dla tych trzech implementacji,
#   zmieniając liczbę wierszy ramki danych N oraz liczbę grup G.
#   Narysować wykres zależności czasu (box plot, wartość typowa + zmienność)
#   od N i G według implementacji.

library(data.table)
library(tibble)
library(dplyr)
library(ggplot2)
library(microbenchmark)

# funkcje do tworzenia tabel, według implementacji ----
create_data_base = function(N, G){
  data.frame(
    X = sample(1:G, N, replace = TRUE),
    Y = runif(N),
    W = runif(N, 0, 100),
    Z = runif(N, -1000, 1000))
}

create_data_tibble = function(dt){
  as_tibble(dt)
}

create_data_table = function(dt){
  as.data.table(dt)
}

# funkcje stat. agregujące, według implementacji----
#base
fun_stat_base = function(data_base, fun_stat){
  aggregate(cbind(Y, W, Z) ~ X,
            data = data_base,
            FUN = fun_stat)}

### tidyverse
fun_stat_tibble = function(data_tibble, fun_stat){
  data_tibble %>%
    group_by(X) %>%
    summarize_all(fun_stat)}

### data.table
fun_stat_dt = function(data_table, fun_stat){
  data_table[, lapply(.SD, fun_stat), by = X]}

# microbenchmark dla tych tabel , obiekt i boxplot----
microbenchmark_obj = function(G, N, fun_stat, t){
  data_base = create_data_base(G, N)
  data_tibble = create_data_tibble(data_base)
  data_table = create_data_table(data_base)
  
  microbenchmark(
    base = fun_stat_base(data_base, fun_stat),
    tibble = fun_stat_tibble(data_tibble, fun_stat),
    dt = fun_stat_dt(data_table, fun_stat),
    times = t)
}


microbenchmark_boxplot = function(mb, G, N){
  boxplot(mb)
  title(paste0("G = ", G,", N = ", N))
}

# tabela i wykresy boxplot----
microbenchmark_obj(G = 5, N = 100, mean, t = 100)

# wykresy boxplot
par(mfrow=c(3,4))
microbenchmark_boxplot(microbenchmark_obj(G = 50, N = 100, mean, t = 100),G = 5, N = 100)
microbenchmark_boxplot(microbenchmark_obj(G = 50, N = 1000, mean, t = 100),G = 5, N = 1000)
microbenchmark_boxplot(microbenchmark_obj(G = 50, N = 10000, mean, t = 100),G = 5, N = 10000)
microbenchmark_boxplot(microbenchmark_obj(G = 50, N = 100000, mean, t = 100),G = 5, N = 10000)
# G = 50
microbenchmark_boxplot(microbenchmark_obj(G = 500, N = 100, mean, t = 100),G = 50, N = 100)
microbenchmark_boxplot(microbenchmark_obj(G = 500, N = 1000, mean, t = 100),G = 50, N = 1000)
microbenchmark_boxplot(microbenchmark_obj(G = 500, N = 10000, mean, t = 100),G = 50, N = 10000)
microbenchmark_boxplot(microbenchmark_obj(G = 500, N = 100000, mean, t = 100),G = 50, N = 10000)
# G = 500
microbenchmark_boxplot(microbenchmark_obj(G = 5000, N = 100, mean, t = 100),G = 500, N = 100)
microbenchmark_boxplot(microbenchmark_obj(G = 5000, N = 1000, mean, t = 100),G = 500, N = 1000)
microbenchmark_boxplot(microbenchmark_obj(G = 5000, N = 10000, mean, t = 100),G = 500, N = 10000)
microbenchmark_boxplot(microbenchmark_obj(G = 5000, N = 100000, mean, t = 100),G = 500, N = 10000)

# widzimy, że największa zmienność jest po G

# wykresy zmiennosci, po G i po N ----
plot_mb_by_G = function(n, t){
  # do macieczy wpisujemy średnie 
  dt = matrix(c(50, summary(microbenchmark_obj(G = 50, N = n, mean, t))$mean,
                250, summary(microbenchmark_obj(G = 250, N = n, mean, t))$mean,
                1250, summary(microbenchmark_obj(G = 1250, N = n, mean, t))$mean,
                6000, summary(microbenchmark_obj(G = 6000, N = n, mean, t))$mean),
              nrow = 4, ncol = 4, byrow = TRUE)
  # i macierz konwertujemy do tabeli wąskiej
  dt = as.data.table(dt)
  setnames(dt, colnames(dt), c("G", "base", "tibble", "dt"))
  dt <-melt(dt,
            id.vars = "G",
            measure.vars = c("base", "tibble", "dt"),
            variable.name = "package", value.name = "mean",
            variable.factor = FALSE)
  
  ggplot(dt, aes(x = G, y = mean, color = package, group= package)) +
    geom_line() +
    coord_trans(y = "log")
}

plot_mb_by_G(n = 10000, t = 30)

# zmienność po N
plot_mb_by_N = function(g, t){
  dt = matrix(c(100, summary(microbenchmark_obj(G = g, N = 100, mean, t))$mean,
                500, summary(microbenchmark_obj(G = g, N = 500, mean, t))$mean,
                2500, summary(microbenchmark_obj(G = g, N = 2500, mean, t))$mean,
                12500, summary(microbenchmark_obj(G = g, N = 12500, mean, t))$mean),
              nrow = 4, ncol = 4, byrow = TRUE)
  
  dt = as.data.table(dt)
  setnames(dt, colnames(dt), c("N", "base", "tibble", "dt"))
  dt <-melt(dt,
            id.vars = "N",
            measure.vars = c("base", "tibble", "dt"),
            variable.name = "package", value.name = "mean",
            variable.factor = FALSE)
  
  ggplot(dt, aes(x = N, y = mean, color = package, group= package)) +
    geom_line() +
    coord_trans(y = "log")
}
plot_mb_by_N(g = 500, t = 50)

