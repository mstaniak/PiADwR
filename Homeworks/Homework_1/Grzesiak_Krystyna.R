library(dplyr)
library(data.table)

compute_NA_frac_base <- function(data) {
  colMeans(is.na(data))
}

compute_NA_frac_dplyr <- function(data) {
  data %>% 
    summarise_all(funs(mean(is.na(.))))
}

compute_NA_frac_dt <- function(data) {
  data[, lapply(.SD, function(col) mean(is.na(col))), ]
}

compute_NA_frac_base(airquality)
compute_NA_frac_dplyr(data)
compute_NA_frac_dt(as.data.table(airquality))
