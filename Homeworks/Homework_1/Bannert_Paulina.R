#Zadanie domowe
# funkcja, która oblicza proporcje brakujących wartości dla każdej kolumny

prop_NA <- function(input_vector){
  num_na = length(input_vector[is.na(input_vector)])
  num_all = length(input_vector)
  return (num_na/num_all)
}
apply(gas_base, 2, prop_NA)