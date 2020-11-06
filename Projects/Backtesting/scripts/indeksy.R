library(rvest)
library(stringr)
library(stringi)

dir.create(paste0(c(getwd(),'indeksy'),collapse='\\'))

### BIEŻĄCE DANE O INDEKSACH ZE STOCKWATCH.PL ###
indeksy = read_html('https://www.stockwatch.pl/gpw/indeksy') %>%
  html_nodes(xpath = '//*[@id="StockIndexes"]') %>%
  html_table(fill=TRUE)
indeksy = indeksy[[1]]

res = c()
for (i in indeksy$Indeksy){
  tmp = str_split(i,pattern = '', simplify = T)
  result = c()
  for (j in 1:length(tmp)){
    result = c(result,tmp[j])
    if (paste0(result,collapse='') == paste0(tmp[(j+1):length(tmp)],collapse = '')){
      break
    }
  }
  res[match(i,indeksy$Indeksy)] = paste0(result,collapse='')
}

indeksy$Indeksy = res

write.csv(indeksy,
          file = paste0(getwd(),'/info_indeksy.csv'),
          fileEncoding = 'UTF-8')

### BIEŻĄCE DANE O INDEKSACH SEKTOROWYCH ZE STOCKWATCH.PL ###
lista_indeksow = list()

for (indeks in indeksy$Indeksy){
  lista_indeksow[[str_to_upper(indeks)]] = paste0(c('https://www.stockwatch.pl/gpw/indeks/',indeks,',sklad.aspx'),collapse='')
}

lista_tabel = list()

for (url in names(lista_indeksow)){
  print(c(url, lista_indeksow[[url]]))
  tbl.page <- lista_indeksow[[url]] %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="StockIndexFull"]') %>%
    html_table()
  
  lista_tabel[[url]] = tbl.page[[1]]
}

for (url in names(lista_indeksow)){
  dobre_nazwy = gsub("\\r.*","",lista_tabel[[url]]$Spółka)
  lista_tabel[[url]]$Spółka = dobre_nazwy
}

for (tabela in names(lista_tabel)){
  write.csv(lista_tabel[[tabela]],
            file = paste0(c(getwd(),'/indeksy/',tabela,'.csv'),collapse=''),
            fileEncoding = 'UTF-8')
}

nazwy_spolek = c()
for (tabela in lista_tabel){
  nazwy_spolek = c(nazwy_spolek,tabela$Spółka)
}

nazwy_spolek = unique(nazwy_spolek)

tickery_spolek = c()
for (spolka in nazwy_spolek){
  ticker = paste0(c('https://www.stockwatch.pl/gpw/',spolka,',notowania,wskazniki.aspx'),collapse='') %>%
    read_html() %>%
    html_node(xpath='//*[@id="SWCnt"]/div[6]/table') %>%
    html_table()
  print(ticker$X1)
  tickery_spolek = c(tickery_spolek,ticker$X1)
}

new = data.frame(tickery_spolek,nazwy_spolek)
names(new) = c('Ticker','Name')
write.csv(new, 'tickers_pl.csv')