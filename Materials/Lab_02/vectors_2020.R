# library()


# Podstawowy typ danych: vector (wektor)
# Jak tworzyć wektory?
# Operatory przypisania:
# <-
x <- 1
x
print(x)
# ->
1 -> y
y
# =
z = 1
# Tworzenie wektorów
longer_vec <- c(1, 2, 3)
# Wektory liczbowe
0:10
seq_vec_1 <- seq()
seq_vec_2 <- seq(1, 20, by = 0.5)
seq_vec_2

seq_vec_3 <- seq(0, 1, length.out = 100)
seq_vec_3
length(seq_vec)

class(0:10)
class(seq_vec_3)

vector()
vector("integer", 10)
vector("numeric", 10)

repeated_vec = rep(1:10, each = 5)
repeated_vec
length(repeated_vec)

repeated_vec_2 = rep(1:10, times = 2)
repeated_vec_3 = rep(1:10, times = 1:10)
repeated_vec_3

# Wektory napisów

str_vec = 'a'
str_vec_2 = "a"
class(str_vec)
str_vec_3 = '"a"'
cat(str_vec_3)
str_vec_3
# str_vec_err = ""a""

x <- c("a", "b", "c")
letters
LETTERS

# "a" + "b"
paste(x)
paste("a", "b", "c")
length(paste("a", "b", "c", sep = "xD"))
paste(x, collapse = "")
length(paste(x, collapse = ""))

toupper(x)
tolower(x)
tolower(toupper(x))
# 1. ... : dowolna liczba argumentów
# 2. paste działa element po elemencie
paste(c("a", "b"), c("c", "d"), sep = "")
# Działanie element po elemencie jest domyślnym i naturalnym
# zachowaniem funkcji w R

1:10 + seq(0, 1, length.out = 10)
1:10 - seq(0, 1, length.out = 10)
# itd.

# Ważne: recykling
1:10 + 1:3 + 1:2 + 1:5

1:10 + 1:2
1:10 + rep(1:2, 5)

# Zastosowanie wektorów napisów: nazwy
num_vec = c("a" = 1, "b" = 2, "c" = 3)
num_vec
# num_vec = c(a = 1, b = 2, c = 3)
str_vec_nam = c("a" = "A", "b" = "B", "c" = "C")
str_vec_nam

names(str_vec_nam)
unname(str_vec_nam)

# Wektory logiczne

TRUE
# Skrót: T
FALSE
# Skrót: F

`TRUE` <- "xD"
TRUE
`T` <- "xD"
T
`T` <- FALSE
T

log_vec = c(TRUE, FALSE, TRUE)
log_vec

# Operacje logiczne

x == c("a", "b", "c")
x != c("a", "b", "c")
x == c("a", "B", "c")

all(x == c("a", "b", "c"))
any(x == c("a", "b", "c"))

# UWAGA : liczby zmiennoprzecinkowe!!
1.1 == 1.1
# identical(1.1, 1.1)
all.equal(1.1, 1.1)
abs(1.1 - 1.10000001) < 1e-7
all(abs(1.1 - 1.10000001) < 1e-7)

1:10 == 1

# Factor
fac_vec = factor(c("a", "b", "c"))
fac_vec
num_fac = factor(c("10", "20", "30"))
as.numeric(num_fac)
as.numeric(as.character(num_fac))
nlevels(fac_vec)
levels(fac_vec)
as.numeric(levels(num_fac))
#

# INDEKSOWANIE
# 1. Dodatnie liczby całkowite
repeated_vec_2[1:10]
repeated_vec_2[c(1, 15)]
repeated_vec_2[seq(1, 20, by = 2)]
# 2. Ujemne liczby całkowite
repeated_vec_2[-(1:10)]
repeated_vec_2[-c(1, 15)]
repeated_vec_2[-seq(1, 20, by = 2)]

new_vec = repeated_vec_2[-seq(1, 20, by = 2)]
new_vec[1:5] = 1
new_vec
# 3. Napisy - nazwy
str_vec_nam["a"]
str_vec_nam[c("a", "c")]
str_vec_nam[c("c", "a")]

old = sample(c("a", "b", "c"), size = 20, replace = TRUE)
dict = c("a" = "A", "b" = "B", "c" = "C")

unname(dict[old])

# 4. Wektory logiczne
log_ind = x == "a"
log_ind
x[log_ind]

new_seq_vec = seq_vec_3[seq_vec_3 < 0.5]
length(new_seq_vec)

# Kolejne operacje logiczne:
# i: &
# lub: |

ints = seq(1, 100, by = 3)
ints[(ints %% 2 == 0) & ints > 10]
ints[(ints %% 2 == 0) | ints > 10]

# KONWERSJE
class(ints)
weird_vec = c("a", 1, 1.1)
weird_vec

class(c(TRUE, 1L))
class(c(1L, 2L, 1.11))
class(c(1.1, "a", "b"))
# as.numeric, as.character itd

with_missing = c(1, 2, NA)
with_missing == NA
with_missing[with_missing > 1 & !is.na(with_missing)]
c("A", NA)

sum(with_missing)
sum(with_missing, na.rm = TRUE)
sum(na.omit(with_missing))
mean(with_missing)
mean(with_missing, na.rm = TRUE)
# itd.

# NA_real_
# NA_character_


# Dwuwymiarowe typy danych

## Macierz

m = matrix(1:10, nrow = 5, ncol = 2)
m[m[, 1] > 2, 1]
m[m[, 1] > 2, 1, drop = F]

m[matrix(c(1, 2, 3, 1, 4, 2), byrow = TRUE,
         nrow = 3, ncol = 2)]
# Mnożenie macierzowe: %*%

# LISTA
short_list = vector('list', 10)
# is.na(NULL)
# is.null(NULL)
short_list
short_list[[10]]

new_list = list(a = 1, b = 2, c = 3)
new_list
new_list[[1]]
new_list[["a"]]
new_list[[TRUE]]

# Błąd:
# new_list[[c(1, 2)]]
# new_list[[c("a", "b")]]

new_list[[1]]
class(new_list[[1]])

new_list[1]
class(new_list)
new_list[c(TRUE, FALSE, TRUE)]
new_list[c(TRUE, FALSE)]
new_list[c(FALSE, TRUE)]

# UWAGA: pod pojedynczym nawiasem kwadratowym
# indeksujemy dokładnie tak jak wektor
# Różnica: żeby dostać element listy, a nie pod-listą
# należy użyć [[]]

list(a = 1, b = 2) == 1

# mean(list(a = 1, b = 2)) # error
mean(unlist(list(a = 1, b = 2)))
class(unlist(list(a = 1, b = 2)))

list(a = 1:10, b = c("a", "b"))

meas_list = list(
    a = 1:10,
    b = 1:5,
    c = rev(1:20),
    d = letters
)
meas_list

meas_list$"a"
meas_list$a
class(meas_list$a)
# Nie R-owe rozwiązanie:
# new_vec = vector("numeric", 3)
# for (i in 1:3) {
#     print(meas_list[[i]][1])
#     new_vec[i] = meas_list[[i]][1]
# }

# RODZINA FUNKCJI APPLY
# apply, tapply, sapply, lapply, vapply itd
lapply(meas_list, function(x) x[1])
sapply(meas_list, function(x) x[1])

first_df = data.frame(
    Letter = letters,
    Id = 1:length(letters)
)
first_df
first_df[1:5, c("Letter"), drop = FALSE]
first_df[first_df$Id %% 2 == 0, ]
first_df[["Letter"]]
first_df[, "Letter"]

# list(a = 1, b = 2) == c(a = 1, b = 2)

# BONUS: przydatne funkcje
sort(x)
rev(x)
rev(sort(x))
paste()
seq()
sample()
