library(microbenchmark)
library(data.table)
library(tidyverse)

N = c(100,1000,10000,100000)
executionTimesdf = data.frame(baseR = c(), dataTable = c(), tidyVerse = c(), N = c(), G = c())
for (G in c(2,4,6,8)){
  for(n in N){
    df = data.frame(X = rep(1:G,length = n), Y = rnorm(n), W = rexp(n), Z = runif(n))
    dt = data.table(X = rep(1:G, length = n), Y = rnorm(n), W = rexp(n), Z = runif(n))
    tv = tibble(X = rep(1:G,length = n), Y = rnorm(n), W = rexp(n), Z = runif(n))
    
    executionTimes = microbenchmark::microbenchmark(
      baseR = aggregate(. ~ X, data = df, FUN = function(x) {c(mean(x),median(x),sd(x))}),
      dataTable = dt[, lapply(.SD,FUN = function(x) {c(mean(x),median(x),sd(x))}),by = X],
      tidyVerse = tv %>% group_by(X) %>% summarize_all(c(mean,median,sd)),
      times = 50) 
    
    executionTimesdf = rbind(executionTimesdf, as.data.frame(cbind(executionTimes,n,G)))
  }
}

ggplot(executionTimesdf,aes(expr,time)) + 
  geom_boxplot() + 
  facet_grid(vars(n),vars(G)) +
  ggtitle("Czas obliczeń statystyk dla ramek danych w bazowym R, tidyverse i data table\nw zależności od liczby grup G i liczby wierszy N") +
  xlab("liczba grup G") +
  ylab("liczba wierszy N") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"), 
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10), 
        axis.text.y =element_text(size = 10))

