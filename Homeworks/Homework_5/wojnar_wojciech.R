library(data.table)
library(tidyverse)
library(microbenchmark)

create_df_for_benchmark = function(G, N){
  df = data.frame(X = as.factor(sample(x = 1:G, size = N, replace = T)),
                  Y = runif(N),
                  W = rnorm(N),
                  Z = rexp(N))
  return(df)
}

G = 10**2
N = 10**6

benchmark_base = create_df_for_benchmark(G, N)
benchmark_dt = data.table(benchmark_base)
benchmark_tibble = as_tibble(benchmark_base)

bench_1 = microbenchmark(
  aggregate(. ~ X, data = benchmark_base, FUN = mean),
  benchmark_dt[, lapply(.SD, mean), by = "X", .SDcols = c("Y", "W", "Z")],
  benchmark_tibble %>% group_by(X) %>% summarise_each(mean)
)

bench_11 = cbind(G = as.factor(G), N = as.factor(N), bench_1)

G = 10**3
N = 10**6

benchmark_base = create_df_for_benchmark(G, N)
benchmark_dt = data.table(benchmark_base)
benchmark_tibble = as_tibble(benchmark_base)

bench_2 = microbenchmark(
  aggregate(. ~ X, data = benchmark_base, FUN = mean),
  benchmark_dt[, lapply(.SD, mean), by = "X", .SDcols = c("Y", "W", "Z")],
  benchmark_tibble %>% group_by(X) %>% summarise_each(mean)
)

bench_21 = cbind(G = as.factor(G), N = as.factor(N), bench_2)

G = 10**2
N = 10**7

benchmark_base = create_df_for_benchmark(G, N)
benchmark_dt = data.table(benchmark_base)
benchmark_tibble = as_tibble(benchmark_base)

bench_3 = microbenchmark(
  aggregate(. ~ X, data = benchmark_base, FUN = mean),
  benchmark_dt[, lapply(.SD, mean), by = "X", .SDcols = c("Y", "W", "Z")],
  benchmark_tibble %>% group_by(X) %>% summarise_each(mean)
)

bench_31 = cbind(G = as.factor(G), N = as.factor(N), bench_3)

bench = rbindlist(list(bench_11, bench_21, bench_31))

ggplot(bench) + 
  geom_boxplot(aes(x = expr, y = time, col = G, fill = N)) + 
  scale_x_discrete(labels = c("aggregate(. ~ X, data = benchmark_base, FUN = mean)" = "base",
                              'benchmark_dt[, lapply(.SD, mean), by = "X", .SDcols = c("Y",      "W", "Z")]' =
                                "data.table", 
                              'benchmark_tibble %>% group_by(X) %>% summarise_each(mean)' =
                                "tidyverse")) + 
  theme(axis.text.x = element_text(angle = 270))
