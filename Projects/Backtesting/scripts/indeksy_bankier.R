library(rvest)
library(stringr)
library(stringi)

pages = c('akcje', 'indeksy-gpw', 'new-connect', 'futures', 'opcje', 'obligacje')
url = 'https://www.bankier.pl/gielda/notowania/'

### POBRANIE INFORMACJI O AKCJACH-GPW, INDEKSACH GPW, AKCJACH-NEWCONNECT, FUTURESACH, OPCJACH I OBLIGACJACH

lista_tabel = list()

for (page in pages){
  print(page)
  tmp_url = paste0(url,page,collapse = '')
  
  tabela = read_html(tmp_url) %>%
    html_nodes(xpath = '//*[@id="boxQuotes"]/div[2]/table[1]') %>%
    html_table(fill=TRUE)
  tabela = tabela[[1]]
  tabela = rbind(tabela[1:9,1:ncol(tabela)],tabela[11:nrow(tabela),1:ncol(tabela)])
  lista_tabel[[page]] = tabela
}


### POBRANIE INFORMACJI O SPÓŁKACH Z INTERESUJĄCYCH NAS WIGÓW

indeksy = lista_tabel[['indeksy-gpw']]$`Walor AD`[1:28]

url = 'https://www.bankier.pl/inwestowanie/profile/quote.html?symbol='

lista_indeksow = list()

for (indeks in indeksy){
  print(indeks)
  tmp_url = paste0(url,indeks,collapse = '')
  
  tabela = read_html(tmp_url) %>%
    html_nodes(xpath = '//*[@id="boxProfil"]/div[2]/div[5]/div[2]/table[1]') %>%
    html_table(fill=TRUE)
  
  tabela = tabela[[1]]
  lista_indeksow[[indeks]] = tabela
}

### ZAPISANIE INFORMACJI (Indeks, Nazwa, Ticker) DO RAMKI DANYCH

result = data.frame()

for (indeks in names(lista_indeksow)){
  result = rbind(result, cbind(indeks, lista_indeksow[[indeks]]$Nazwa, lista_indeksow[[indeks]]$Ticker))
}

result
names(result) = c('Indeks', 'Name', 'Ticker')

write.csv(result, file = 'all_index_data.csv')
