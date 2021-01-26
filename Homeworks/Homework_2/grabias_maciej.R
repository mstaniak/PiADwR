### Zad Dom
#1. Dla ramek danych gas_two_wide i gas_two
# -> obliczyc znormalizowane wartosci MeasuredValue (odjecie sredniej, podzielenie przez odch. stand.)
# (dla gas_two w postaci waskiej, dla gas_two_wide w postaci szerokiej)
# dla gas_two: przez referencje i bez referencji
# dla gas_two_wide: przy uzyciu lapply(), przy uzyciu referencji i lapply(),
# lapply() bez referencji, bez lapply() - z referencja i bez
# 2. Dla dowolnego miejsca: 
# - przekonwertowac do wersji szerokiej ze wzgledu na ROK
# - wrocic do wersji waskiej
# - przekonwertowac do wersji szerokiej ze wzgledu na miejsce
# - znormalizowac dla kazdego miasta osobno
# # 3. Dla gas_dt, zrobic to co na zajeciach ze srednimi bez uzycia merge/join.

### Rozwiazania:
### 1.

# zdefiniuj funkcje, zeby nie pisac tego samego 100 razy
normalization <- function(x){
  ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

## a) gas_two

# (i) przez referencje:
gas_two[, Normalized := .(normalization(MeasuredValue)),
        by = .(Pollutant)]


# (ii) bez referencji:
gas_two[, c("Normalized")] <- gas_two[, .(Normalized = normalization(MeasuredValue)),
                                      by = .(Pollutant)][, Normalized]


## b) gas_two_wide

# (i) lapply()
gas_two_wide[, c("Norm_CO", "Norm_O3")] <- gas_two_wide[, lapply(.SD, normalization), 
                                                        .SDcols = c("CO", "Ozone")]

# (ii) lapply() + referencja
gas_two_wide[, c("Norm_CO", "Norm_O3") := lapply(.SD, normalization),
             .SDcols = c("CO", "Ozone")]


# (iii) referencja bez lapply()
gas_two_wide[, `:=`(Norm_CO = normalization(CO),
                    Norm_O3 =  normalization(Ozone))]


# (iv) bez referencji i bez lapply()
gas_two_wide[, c("Norm_CO", "Norm_O3")] <- gas_two_wide[, list(Norm_CO = normalization(CO),
                                                               Norm_O3 =normalization(Ozone))]

### 2. Dla dowolnego miejsca: 

# - przekonwertowac do wersji szerokiej ze wzgledu na ROK

# zrobmy dla stanu Iowa
gas_state <- gas_two[State == "Iowa"]
gas_state[, `:=`(Month = month(Date),
                 Year = year(Date),
                 Date = mday(Date))]

# nie do konca wiem jak ogarnac ten format IDate? wiec zrobilem osobne 
# kolumny dla dnia i miesiaca - nie jest to moze najpiekniejsze rozwiazanie, ale dziala (mam nadzieje)

gas_state_wide_year <- dcast(gas_state, County + City + Site + Pollutant + Month + Day ~ Year,
                             value.var = "MeasuredValue",
                             fun.aggregate = function(x) mean(x, na.rm = TRUE))

# mamy o wiele wiecej pomiarow dla roku 2017 niz 2020

# - wrocic do wersji waskiej

gas_alabama_long_year <- melt(gas_state_wide_year, id.vars = setdiff(colnames(gas_state_wide_year), c("2017", "2020")),
                              measure.vars = c("2017", "2020"),
                              variable.name = "Year", value.name = "MeasuredValue",
                              variable.factor = FALSE)

# - przekonwertowac do wersji szerokiej ze wzgledu na miejsce

gas_iowa <- gas_two[State == "Iowa"]
gas_iowa

gas_iowa_wide_place <- dcast(gas_iowa, County  + Pollutant + Date ~ City,
                             value.var = "MeasuredValue", fill = NA_real_, fun.aggregate = function(x) mean(x, na.rm = TRUE))

gas_iowa_wide_place

# - znormalizowac dla kazdego miasta osobno

gas_iowa_wide_place[, unique(gas_iowa$City) := lapply(.SD, normalization),
                    .SDcols = unique(gas_iowa$City)]


### 3. Dla gas_dt, zrobic to co na zajeciach ze srednimi bez uzycia merge/join.

# dodaj kolumne przez referencje z obliczona srednia zaobserwowana emisje 
# danego gazu w danym roku w kazdym ze stanow
gas_dt[, Average_Val := mean(MeasuredValue, na.rm = TRUE), by = c("Pollutant", "State", "Year")]

