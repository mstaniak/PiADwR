# Zadanie domowe.
# microbenchmark:
# - ramka danych (data.frame, data.table, tibble) o liczbie wierszy N,
#   liczbie grup G w zmiennej X i wartościach liczbowych w zmiennych Y, W, Z.
# - operacja: obliczanie statystyk dla zmiennych Y, W, Z według grup zmiennej X
# - trzy alternatywne implementacje: bazowy, tidyverse, data.table
# - zadanie: porównać czas wykonania tej operacji dla tych trzech implementacji,
#   zmieniając liczbę wierszy ramki danych N oraz liczbę grup G.
#   Narysować wykres zależności czasu (box plot, wartość typowa + zmienność)
#   od N i G według implementacji.
#

library(data.table)
library(microbenchmark)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)

f <- function(x){c(min(x), max(x), mean(x), sd(x))}
times <- data.frame(base = c(), data = c(), tiny = c(), N = c(), G = c())
times2 <- data.frame(base = c(), data = c(), tiny = c(), N = c(), G = c())
date()
for (i in 1:3){
  for (j in 1:3){
    N <- 100^i
    G <- 2^(2*j)
    xb = data.frame(
      X = sample(LETTERS[1:G], N, replace = T),
      Y = runif(N, min = 0, max = 10),
      W = rnorm(N, mean = 5, sd = 1),
      Z = rexp(N))
    xd = as.data.table(xb)
    xt = as_tibble(xb)
    x <- microbenchmark(
      base = aggregate(.~X, xb, f),
      data = xd[, as.list(unlist(lapply(.SD, f))), by = X, .SDcols = c('Y','W','Z')],
      tiny = xt %>% group_by(X) %>% summarise_all(c(min, max, mean, sd)),
      times = 100)
    times <- rbind(times, as.data.frame(cbind(x,N,G)))
    y <- rbind(
      x[x$expr == 'base' & x$time < quantile(x[x$expr == 'base', 2], probs = 0.9),],
      x[x$expr == 'data' & x$time < quantile(x[x$expr == 'data', 2], probs = 0.9),],
      x[x$expr == 'tiny' & x$time < quantile(x[x$expr == 'tiny', 2], probs = 0.9),])
    times2 <- rbind(times2, as.data.frame(cbind(y,N,G)))
  }
}
date()

times$time <- times$time/1e6
times2$time <- times2$time/1e6

ggplot(times, aes(x = expr, y = time)) +
  facet_grid(rows = vars(N), cols = vars(G), scales = "free") +
  geom_boxplot() +
  ylab("time in ms")

# wykres po odcieciu 10% najwiekszych obserw
ggplot(times2, aes(x = expr, y = time)) +
  facet_grid(rows = vars(N), cols = vars(G), scales = "free") +
  geom_boxplot() +
  ylab("time in ms")
