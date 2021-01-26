#install.packages(c("dplyr", "lubridate", "readr", "data.table", "stringr", "tidyr"))
library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(tidyr)

gas_files <- list.files(, full.names = TRUE, pattern = ".csv")


gas_dfs <- lapply(gas_files, read.csv)
gas_dfs[[1]]
gas_base <- do.call("rbind", gas_dfs)
head(gas_base)
ncol(gas_base)
nrow(gas_base)
str(gas_base)
summary(gas_base)

## tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
# Naprawienie danych
for(i in 1:4)
{
  gas_dfs_tv[[i]]$`Method Code` <- as.character(gas_dfs_tv[[i]]$`Method Code`)
  gas_dfs_tv[[i]]
}
gas_tv <- bind_rows(gas_dfs_tv)
gas_tv


## data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt
## Operacje kolumnowe:
# W kazdym z trzech dialektow R'owych napisac
unique_na <- function(vec)
  {
  round(sum(is.na(vec))/length(vec), 2)
  }
### base
lapply(gas_base, unique_na)

### tidyverse
View(summarise_all(gas_tv, unique_na))

### data.table
View(gas_dt[, lapply(.SD, unique_na)])




