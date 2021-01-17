library(data.table)
library(tidyverse)
library(microbenchmark)

create_df_for_benchmark = function(G, N){
  df = data.frame(X = sample(x = 1:G, size = N, replace = T),
                  Y = runif(N),
                  W = rnorm(N),
                  Z = rexp(N))
  return(df)
}

G = 10*2
N = 10**6

benchmark_base = create_df_for_benchmark(G, N)
benchmark_dt = data.table(benchmark_base)
benchmark_tibble = as_tibble(benchmark_base)

microbenchmark(
  benchmark_dt[, lapply(.SD, mean), by = "X", .SDcols = c("Y", "W", "Z")],
  benchmark_tibble %>% group_by(X) %>% summarise(mean)
)

benchmark_tibble %>% group_by(X) %>% summarise(mean)
