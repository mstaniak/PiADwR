## Schemat działania aplikacji

1. Wybór przez użytkownika interesujących go indeksów (regionu) (checkboxy?lista?)

2. Pobranie danych (przycisk do pobrania) / załadowanie danych przez użytkownika

    Obróbka (mało konkretne określenie) i uzupełnienie braków (jeśli danego dnia na danej spółce był zerowy obrót to brakuje wiersza z tą datą w danych dlatego myślałem o uzupełnianiu NOCB/LOCF)
    
    
3. Wybór strategii (lista, zaimplementujemy tylko jedną chyba że będziemy mieli
więcej czasu, jeśli nie to chciałbym dalej sam sobie później w tym grzebać jak nie
będziecie mieli nic przeciwko)

4. Parametry strategii:
    PRZYKŁADOWO (parametry zależą od strategii):
    
    tickery/nazwy ktore nas interesuja (wybrane w punkcie 1)
    
    data analizy
    
    short_term - krótsze okienko czasowe dla którego liczymy momentum
    
    long_term - dłuższe okienko czasowe dla którego liczymy momentum
    
    max_stocks - maksymalna liczba spółek w portfelu
    
    min_inv_vola - tolerancja na zmienność
    
    min_momentum - minimalne momentum
    
    max_gain - maksymalny wzrost w poniższym okresie
    
    period_of_max_gain - okres w którym jeśli spółka wzrośnie > max_gain to nie wybieramy jej do portfela
    
    cash_value - gotówka przy rebalansacji

5. Backtesting:
    Czas rebalansacji portfela (początek/koniec tygodnia/miesiąca/kwartału)
    
    Prowizja od transakcji
    
    Inflacja???

6. Wyniki i podsumowanie symulacji (jakaś ładna tabelka, wykresik)
    
    Tabela z historią transakcji
    
    Wykres kapitału w czasie
    
    Tabela z miesiącznymi stopami zwrotu??
    
7. ???

8. ???
