# Funkcje ----

x <- 1:10

some_function = function(some_vector) {
    some_vector[10] = -Inf
    some_vector
    # return(some_vector)
}

some_function(x)
x

some_function2 = function(some_vector) {
    some_vector[10] = -Inf
}


y = some_function(x) # OK
y
y2 = some_function2(x) # X
y2

data = data.frame(x = 1:10,
                  y = "y",
                  z = runif(10))
?data

library(ggplot2)
plot_function = function(var_1, var_2) {
    ggplot(data, aes_string(x = var_1, y = var_2)) +
        geom_point() +
        theme_bw()
}
plot_function("x", "z")


plot_function2 = function(data, var_1, var_2) {
    ggplot(data, aes_string(x = var_1, y = var_2)) +
        geom_point() +
        theme_bw()
}

plot_function2 = function(data, var_1, var_2) {
    ggplot(data, aes_string(x = var_1, y = var_2)) +
        geom_point() +
        theme_bw()
}

plot_function2(data.frame(x = 1:10,
                          y = "y",
                          z = runif(10)), "x", "z")


# Pętle ----
library(microbenchmark)
library(pryr)
library(profvis)

system.time({mean(1:1000000)})

microbenchmark(
    mean_1 = mean(1:10000000),
    mean_2 = sum(1:10000000)/length(1:10000000),
    times = 30
)

# Operacja: tworzenie listy wektorów liczbowych o różnej długości i różnej długości
# wektorów składowych
# wektor liczbowy: runif(...)
# Sposób 1: lapply
make_list_lapply = function(list_length, vector_length) {
    lapply(1:list_length, function(x) data.frame(matrix(runif(10*vector_length),
                                                        ncol = 10)) )
}
# Sposób 2: zaczynamy z pustą listą i dodajemy kolejne elementy
make_list_empty = function(list_length, vector_length) {
    output_list = list()
    for (i in 1:list_length) {
        output_list = c(output_list, list(data.frame(matrix(runif(10*vector_length),
                                                            ncol = 10)) ))
    }
    output_list
}
# Sposób 3: tworzymy listę długości N i ustawiamy kolejne elementy
make_list_assign = function(list_length, vector_length) {
    output_list = vector("list", list_length)
    for (i in 1:list_length) {
        output_list[[i]] = data.frame(matrix(runif(10*vector_length),
                                             ncol = 10))
    }
    output_list
}

microbenchmark(
    lapply = make_list_lapply(100, 1000),
    empty = make_list_empty(100, 1000),
    assign = make_list_assign(100, 1000),
    times = 100
)

microbenchmark(
    lapply = make_list_lapply(1000, 1000),
    empty = make_list_empty(1000, 1000),
    assign = make_list_assign(1000, 1000),
    times = 10
)


library(pryr)

x = data.frame(x = 1:10000,
               y = sample(letters, 10000, replace = TRUE))

# pryr::object_size()
pryr::address(x)

colnames(x) = c("col_1", "col_2")
address(x)

x$col_3 = "xD"
address(x)

x[1, 1] = 2
address(x)

library(data.table)
u = data.table(x = 1:10000,
               y = sample(letters, 10000, replace = TRUE))

# pryr::object_size()
pryr::address(u)

setnames(u, c("x", "y"), c("col_1", "col_2"))
address(u)

u[, col_3 := "xD"]
address(u)


set(u, 1L, 1L, 2)
u[1, 1]
address(u)



# Zadanie domowe.
# microbenchmark:
# - ramka danych (data.frame, data.table, tibble) o liczbie wierszy N,
#   liczbie grup G w zmiennej X i wartościach liczbowych w zmiennych Y, W, Z.
# - operacja: obliczanie statystyk dla zmiennych Y, W, Z według grup zmiennej X
# - trzy alternatywne implementacje: bazowy, tidyverse, data.table
# - zadanie: porównać czas wykonania tej operacji dla tych trzech implementacji,
#   zmieniając liczbę wierszy ramki danych N oraz liczbę grup G.
#   Narysować wykres zależności czasu (box plot, wartość typowa + zmienność)
#   od N i G według implementacji.
#



