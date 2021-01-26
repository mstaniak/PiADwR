library(tidyverse)
library(data.table)

gas_files <- list.files("./Materials/Lab_04/dane", full.names = TRUE)


gas_dfs <- lapply(gas_files, read.csv)
gas_base = do.call("rbind", gas_dfs)

gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv = bind_rows(gas_dfs_tv)

gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)

##### zadanie domowe

#funkcja, ktora zlicza frakcje NA

count_NA <- function(vector) {
  sum(is.na(vector))/length(vector)
}

##base
lapply(gas_base, count_NA)

##tidyverse
summarise_all(gas_tv, count_NA)

##data.table
gas_dt[, lapply(.SD, count_NA)]