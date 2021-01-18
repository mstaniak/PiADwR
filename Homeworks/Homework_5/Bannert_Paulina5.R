library(data.table)
library(dplyr)
library(microbenchmark)
library(pryr)
library(profis)

bulder <- function (N, G){
  groups <- 1:G
  Y <- runif(N)
  Z <- runif(N)
  W <- runif(N)
  X <- rep(groups,length.out=N)
  fr = data.frame(x = X, y = Y, z = Z, w = W)
  return(fr)
}

frames <- function(n,g){
  fr <- bulder(n, g)
  dt <- as.data.table(fr)
  tbb <- as_tibble(fr)
}

stat_fr <- function(fr_data){
  aggregate(fr[,2:4], list(fr$x), data = fr, mean)
}

stat_dt <- function(dt_data){
  dt_data[, list(meanY = mean(y), meanZ = mean(z), meanW = mean(w)), by = x]
}

stat_tbb <- function(tbb_data){
  tbb_data %>% group_by(x) %>% summarize(meanY = mean(y), meanZ = mean(z), meanW = mean(w))
}

#przykład
fr <- bulder(500, 30)
dt <- as.data.table(fr)
tbb <- as_tibble(fr)
microbenchmark(
  dt_frame = stat_fr(fr),
  dt_table = stat_dt(dt),
  tbb = stat_tbb(tbb),
  times = 100)

# to nie działa...(ale wiem dlaczego)
vec1 <- seq(10^2, 10^6, 100)
w <- 1
p <- matrix(0, 10^6 - 10^2/100 + 1, 2)

for (i in vec1){
  fr <- bulder(i, 30)
  dt <- as.data.table(fr)
  tbb <- as_tibble(fr)
  p[w, 2] <- microbenchmark(dt_frame = stat_fr(fr), dt_table = stat_dt(dt), tbb = stat_tbb(tbb), times = 100)
  i = i + 1
}
