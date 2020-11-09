il_NA <- function(x){
  sum(is.na(x))/length(x)
}


#Base R
prop_clas <- function(x){
  apply(x, 2, il_NA)
}


#data.frame
prop_dt <- function(x){
  
  x <-  as.data.table(x)
  x[, lapply(.SD, il_NA)]
}

x[, lapply(.SD, il_NA)]


#tidyverse
prop_tv <- function(x){
  
  summarise_all(x, il_NA)
}
