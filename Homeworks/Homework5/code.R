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

library(microbenchmark)
library(ggplot2)
library(dplyr)
library(data.table)

G <- 10
N <- 100

generate_data <- function(N, G) {
  set.seed(17)
  data_df <- data.frame(X = sample(1:G, N, replace = TRUE),
                        Y = rnorm(N),
                        W = rexp(N),
                        Z = runif(N))
  data_tibble <- tibble(data_df)
  data_dt <- data.table(data_df)
  
  list(data_df,
       data_tibble,
       data_dt)
}

df_mean <- function(df) aggregate(.~ X, df, mean)

dt_mean <- function(df)  setDT(df)[order(X), lapply(.SD, mean), by = .(X)] 

tibble_mean <- function(df) {
  df %>% 
    group_by(X) %>%
    summarise_each(funs(mean))
}

df_mean(data_df)
tibble_mean(data_tibble)
dt_mean(data_dt)


### simulation

example_G <- c(10, 20, 50)
example_N <- c(100, 200, 500)

result <- do.call(rbind, lapply(example_G, function(G) {
  do.call(rbind, lapply(example_N, function(N) {
    data <- generate_data(N, G)
    data_df <- data[[1]]
    data_tibble <- data[[2]]
    data_dt <- data[[3]]
    
    res <- microbenchmark(df_mean(data_df), 
                          tibble_mean(data_tibble),
                          dt_mean(data_dt))
    data.frame(do.call(cbind, res),
               G = G,
               N = N)
  }))
}))

result$expr[result$expr == 1] <- "base" 
result$expr[result$expr == 2] <- "dplyr" 
result$expr[result$expr == 3] <- "data table" 


ggplot(result, aes(x = expr, y = time, col = expr, group = expr)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(result$time, c(0.1, 0.9))) +
  facet_wrap(G~N)
