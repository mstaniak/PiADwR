dt_table = function(dfr)
{
  dtab = as.data.table(dfr)
  dtab
}
tibb = function(dfr)
{
  t = as_tibble(dfr)
  t
}
time__ = function(dframe)
{
  # zwraca c(czas dla data.frame, czas dla data.table, czas dla tibble)
  
  dtable = dt_table(dframe)
  tib = tibb(dframe)
  
  time_dframe = system.time(aggregate(dframe[,2:4],list(dframe$X), data = dframe, mean))[3]
  time_dtable = system.time(dtable[,.(mean(Y), mean(W), mean(Z)), by = X])[3]
  time_tib = system.time(tib %>% group_by(X) %>% summarise(mean(Y), mean(W), mean(Z)))[3]
  time_ = c(time_dframe, time_dtable, time_tib)
  time_
}

Nvec = c(100,1000,10000,100000)
Gvec = c(5,10,20)

plot_benchmark = function(Nvec,Gvec)
{
  data = data.frame(time = 0, N = 0, G = 0, typ = "typ")
  for(N in Nvec)
  {
    for(G in Gvec)
    {
      dframe = dt_frame(N,G)

      times = time__(dframe)
      
      data = rbind(data, data.frame(time = times[1], N = N, G = G, typ = "base"))
      data = rbind(data, data.frame(time = times[2], N = N, G = G, typ = "data.table"))
      data = rbind(data, data.frame(time = times[3], N = N, G = G, typ = "tidyverse"))
      
    }
  }
  row.names(data) = NULL
  data = data[-1,]
  data
}

tab = plot_benchmark(Nvec,Gvec)

ggplot(tab %>% filter(G == 5)) + geom_boxplot(aes(x=typ, y=time, col=typ)) + ggtitle(paste("Liczba grup:", 5))
ggplot(tab %>% filter(G == 10)) + geom_boxplot(aes(x=typ, y=time, col=typ)) + ggtitle(paste("Liczba grup:", 10))
ggplot(tab %>% filter(G == 20)) + geom_boxplot(aes(x=typ, y=time, col=typ)) + ggtitle(paste("Liczba grup:", 20))
