#####       ZADANIE 1      #####

brak_danych = function(tabela)
{
  kolumny = length(tabela[1,])
  wiersze = length(tabela[,1])
  na = c()
  k=0
  for (k in 1:kolumny)
  {
    czy_na = is.na(tabela[,k])
    na[k] = length(which(czy_na==TRUE))
  }
  gdzie = which(na!=0)
  brak_w = colnames(tabela)[gdzie]
  print(c("Brak danych w kolumnach: ", brak_w))
  proporcje = na[gdzie]/wiersze
  print(c("Proporcje:", proporcje))
}

# dane = read.csv("ścieżka")
# brak_danych(dane)
