##### ZADANIE 5 #####

### funkcje ###

dt_frame = function(N,G)
{
  dfr = data.frame(X = rep(1:G, length = N), Y = sample(1:N, N), W = sample(1:N, N), Z = sample(1:N, N)) 
  dfr
}

czas = function(dfr)
{
  c_razem = c()
  # c(czas dfr, czas dtab, czas tibb)
  dtab = as.data.table(dfr)
  tibb = as_tibble(dfr)
  
  c_dfr = system.time(aggregate(dfr[,2:4],list(dfr$X), data = dfr, mean))[3]
  c_dtab = system.time(dtab[,.(mean(Y), mean(W), mean(Z)), by = X])[3]
  c_tibb = system.time(tibb %>% group_by(X) %>% summarise(mean(Y), mean(W), mean(Z)))[3]
  c_razem = c(c_dfr, c_dtab, c_tibb)
  c_razem
}

plot_b = function(N_wek,G_wek)
{
  dane = data.frame(czas = 0, N = 0, G = 0, rodzaj = "rodzaj")
  for(N in N_wek)
  {
    for(G in G_wek)
    {
      dfr = dt_frame(N,G)
      czasy = czas(dfr)
      
      dane = rbind(dane, data.frame(czas = czasy[1], N = N, G = G, rodzaj = "base"))
      dane = rbind(dane, data.frame(czas = czasy[2], N = N, G = G, rodzaj = "data.table"))
      dane = rbind(dane, data.frame(czas = czasy[3], N = N, G = G, rodzaj = "tidyverse"))
      
    }
  }
  row.names(dane) = NULL
  dane = dane[-1,]
  dane
}

### na konkretnych danych ###

N_wek = c(100,1000,10000,100000)
G_wek = c(10,25,50)

tab = plot_b(N_wek,G_wek)

ggplot(tab %>% filter(G == 10)) + geom_boxplot(aes(x=rodzaj, y=czas, col=rodzaj)) + ggtitle(paste("Liczba grup:", 10))
ggplot(tab %>% filter(G == 25)) + geom_boxplot(aes(x=rodzaj, y=czas, col=rodzaj)) + ggtitle(paste("Liczba grup:", 25))
ggplot(tab %>% filter(G == 50)) + geom_boxplot(aes(x=rodzaj, y=czas, col=rodzaj)) + ggtitle(paste("Liczba grup:", 50))
