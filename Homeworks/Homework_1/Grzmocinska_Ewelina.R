#obliczanie proporcji brakujących danych w kolumnach
data = read.csv("scieżka dostępu do pliku")

proportion = function(data)
{
  na = c()
  i = 0
  for (i in 1:(length(data[1,])))
  {
    is_na = is.na(data[,i])
    na[i] = length(which(is_na == TRUE))
  }
  column_na = which(na!=0) #w której kolumnie są braki
  column_name = colnames(data)[column_na] #jak nazywa się ta kolumna
  print(c("Column name: ", column_name))
  prop = na[column_na]/(length(data[,1]))
  print(c("Proportion:", prop))
}

proportion(data)
