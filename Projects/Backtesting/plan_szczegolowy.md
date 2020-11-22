# Funkcje

## Funkcja pobierająca dane do aplikacji (Asia):
INPUT: start_date, end_date, tickers
* funkcja pobiera informacje o spółkach z indeksów, które zaznaczył użytkownik (I)
* pobiera je z okresu (data_analizy - long_term, data_analizy) (I)
* byłbym wniebowzięty gdyby udało się zrobić 'przycisk' który updateuje dane w bazie (bonus)
OUTPUT:
data.table(Date, Ticker, Open, High, Low, Close, Volume)

## Strategie i ich parametry:

## Funkcja implementująca strategię momentum: (I)

INPUT:

* data_analizy

* ticker/nazwa z indeksów które podał użytkownik
    
* short_term - krótsze okienko czasowe dla którego liczymy momentum (bonus)
    
* long_term - dłuższe okienko czasowe dla którego liczymy momentum
    
* max_stocks - maksymalna liczba spółek w portfelu
    
* min_inv_vola - tolerancja na zmienność
    
* min_momentum - minimalne momentum
    
* max_gain - maksymalny wzrost w poniższym okresie
    
* period_of_max_gain - okres w którym jeśli spółka wzrośnie > max_gain to nie wybieramy jej do portfela
    
* cash_value - gotówka przy rebalansacji


OUTPUT:

? Lista: list(cash_value, stocks_value, tabela_jak_niżej) ?

Tabela z wybranymi spółkami, a w niej kolumny: data_analizy, ticker/nazwa, cena z dnia analizy, liczba kupionych akcji, kwota zakupionych akcji, kwota prowizji

Trzeba przedyskutować co tu jeszcze może się znaleźć, pomysły!!!

## Backtesting: (I)

INPUT:

* transaction_fee - prowizja jaką płaci użytkownik, z reguły jest to max(3, 0.0038 x kwota_zlecenia), lub max(5, 0.0038 x kwota zlecenia)

* min_transaction_fee - minimalna prowizja

* kiedy i jak często portfel jest rebalansowany (przynajmniej: raz w miesiącu: początek/koniec)

INSIDE:

* przechodzimy po datach rebalansacji z okresu który podał użytkownik i w każdym dniu rebalansacji zapuszczamy funkcję ze strategią

* z listy którą otrzymujemy z funkcji ze strategią zapisujemy, wartość gotówki, spółek, całego portfela, spółki, które się znalazły w danym okresie w portfelu

* tu trzeba się zdecydować czy podejście jest takie, że:

1. przy każdej rebalansacji wszystko 'sprzedajemy' i 'kupujemy' na nowo (łatwe i przyjemne)

1. śledzimy zmiany w portfelu i czasem 'kupujemy' część i sprzedajemy 'część' (trudniejsze i nie mam lepszego pomysłu niż forloop :()

OUTPUT:

* Krzywa kapitału (cash_value, stocks_value, cash_value_stocks)

* Tabelka podsumowująca:

2. CAGR 

2. Liczba zyskownych/stratnych miesięcy

2. Liczba zyskownych/stratnych transakcji

2. Wskaźnik Sharpe'a

2. itd co nam jeszcze do głowy przyjdzie

2. pomysły!


## Aplikacja (I)

### Menu po lewej

* Wybór początku i końca symulacji - takie kalendarze jak mieli w zeszłym roku byłyby super??

* Checklista z indeksami

* Lista ze strategiami

* Tabelka do wypełnienia z parametrami zależnymi od strategii

### Output backtestingu po prawej

* Tabelka z podsumowaniem

* Krzywa kapitału

* Być może pełna historia transkacji, a jeśli nie to np. może lista dat w których był rebalansowany portfel i klikam date -> wyświetlają mi się wówczas wybrane spółki itd. (bonus?)


