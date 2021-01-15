benchmark_show <- function(n, G)
{
  library(data.table)
  library(tidyverse)
  df <- data.frame(Y = sample(1:n, n), W = sample(1:n, n), Z = sample(1:n, n), X = sample(LETTERS[1:G], n, replace = T))
  tb <- as_tibble(df)
  dt <- as.data.table(df)
  print("Data frame:")
  print(system.time(aggregate(df[,1:3], list(df$X), mean)))
  print("Tibble:")
  print(system.time(tb %>% group_by(X) %>% summarise(mean(Y), mean(W), mean(Z))))
  print("Data table")
  print(system.time(dt[,.(mean(Y), mean(W), mean(Z)), by = X]))
}

benchmark(1e7, 20)


benchmark_plot <- function()
{
  library(data.table)
  library(tidyverse)
  library(ggplot2)
  #data <- data.frame(time = 0, n = rep(c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7), 12), group = rep(c(3, 5, 10, 20), rep(18, 4)), typ = rep(c("base", "tidyverse", "data.table"), 72))
  data <- data.frame(time = 0, n = 0, group = 0, typ = "a")
  
  for(N in c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7))
  {
    print(N)
    for(G in c(3, 5, 10, 20))
    {
      df <- data.frame(Y = sample(1:n, n), W = sample(1:n, n), Z = sample(1:n, n), X = sample(LETTERS[1:G], n, replace = T))
      tb <- as_tibble(df)
      dt <- as.data.table(df)
      # data %>% 
      #   mutate(time=replace(time , n == N & group == G & typ == "base", system.time(aggregate(df[,1:3], list(df$X), mean))[3])) %>%
      #   as.data.frame()
      data <- rbind(data, data.frame(time = system.time(aggregate(df[,1:3], list(df$X), mean))[3], n = N, group = G, typ ="base"))
      
      # data %>% 
      #   mutate(time=replace(time , n == N & group == G & typ == "tidyverse",system.time(tb %>% group_by(X) %>% summarise(mean(Y), mean(W), mean(Z)))[3])) %>%
      #   as.data.frame()
      data <- rbind(data, data.frame(time = system.time(tb %>% group_by(X) %>% summarise(mean(Y), mean(W), mean(Z)))[3], n = N, group = G, typ ="tidyverse"))
      # data %>% 
      #   mutate(time=replace(time , n == N & group == G & typ == "data.table", system.time(dt[,.(mean(Y), mean(W), mean(Z)), by = X])[3])) %>%
      #   as.data.frame()
      data <- rbind(data, data.frame(time = system.time(dt[,.(mean(Y), mean(W), mean(Z)), by = X])[3], n = N, group = G, typ ="data.table"))
    }
  }

  return(data)
}
data <- benchmark_plot()
data <- data[-1,]
row.names(data) <- NULL

ggplot(data %>% filter(group == 3)) + geom_boxplot(aes(x=n, y=time, col=typ)) + ggtitle(paste0("Ilosc grup:", 3))
ggplot(data %>% filter(group == 5)) + geom_boxplot(aes(x=n, y=time, col=typ)) + ggtitle(paste0("Ilosc grup:", 5))
ggplot(data %>% filter(group == 10)) + geom_boxplot(aes(x=n, y=time, col=typ)) + ggtitle(paste0("Ilosc grup:", 10))
ggplot(data %>% filter(group == 20)) + geom_boxplot(aes(x=n, y=time, col=typ)) + ggtitle(paste0("Ilosc grup:", 20))
