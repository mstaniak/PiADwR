library(Quandl)
Quandl.api_key()
tsgames = Quandl('WSE/TSGAMES')
wirtualna = Quandl('WSE/WIRTUALNA')
xtb = Quandl('WSE/XTB')
creepyjar = Quandl('WSE/CREEPYJAR')
creepyjar

total_indeks = read.csv('total_indeks.csv')

all_data = data.frame(matrix(0,0,10))
total_indeks = total_indeks[total_indeks$Indeks=='WIG20',]

for (stock in total_indeks$Name){
  tmp = cbind(stock,Quandl(paste0('WSE/',stock,collapse = '')))
  all_data = rbind(all_data,tmp)
}

try(Quandl(paste0('WSE/',stock,collapse = ''))) == TRUE

domdev = Quandl('WSE/DOMDEV')
