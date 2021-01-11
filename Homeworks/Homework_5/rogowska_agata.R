library(microbenchmark)
library(tibble)
library(dplyr)
library(data.table)

options(dplyr.summarise.inform = F)

get_times = function(N, G, t = 100L){
  groups = sample(1:G, N, replace = TRUE)
  variable_1 = runif(N)
  variable_2 = rnorm(N)
  variable_3 = rt(N, df = 10)
  
  d_base = data.frame(X = groups, Y = variable_1, W = variable_2, Z = variable_3)
  d_tv = tibble(X = groups, Y = variable_1, W = variable_2, Z = variable_3)
  d_dt = data.table(X = groups, Y = variable_1, W = variable_2, Z = variable_3)
  
  m = microbenchmark(
    summary_base = aggregate(list(meanY = d_base$Y, 
                                  meanW = d_base$W, 
                                  meanZ = d_base$Z), 
                             by = list(X = d_base$X), 
                             FUN = mean),
    summary_tv = group_by(d_tv, X) %>% 
      summarise(meanY = mean(Y),
                meanW = mean(W),
                meanZ = mean(Z)),
    summary_dt = d_dt[, .(meanY = mean(Y),
                          meanW = mean(W),
                          meanZ = mean(Z)), 
                      by = X],
    times = t
  )
  cbind(m, data.frame(N = N, G = G))
}


times = rbind(get_times(100, 5),
              get_times(1000, 5),
              get_times(10000, 5),
              get_times(100, 10),
              get_times(1000, 10),
              get_times(10000, 10))
times = as.data.table(times)

library(ggplot2)
windows()
ggplot(times, aes(x = expr, y = time)) +
  geom_boxplot() +
  facet_grid(G ~ N) + 
  theme_bw()

windows()
ggplot(times, aes(x = expr, y = time)) +
  geom_boxplot() +
  facet_wrap(G ~ N, scales = "free_y") + 
  theme_bw()

times[, id := 1:(.N), by = .(N, G, expr)]
windows()
ggplot(times, aes(x = id, y = time, color = as.factor(expr))) +
  geom_line() +
  facet_grid(G ~ N) + 
  theme_bw()

windows()
ggplot(times, aes(x = id, y = time, color = as.factor(expr))) +
  geom_line() +
  facet_wrap(G ~ N, scales = "free_y") + 
  theme_bw()

