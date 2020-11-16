library(datasets)

cars[1:8,1] = NA
cars[1:16,2] = NA

klasyk <- function(data)
{
mean(is.na(data))
}

library(dplyr)

dplyrr <- function(data)
{
  data %>%
    summarise(x = mean(is.na(.)))
}

library(data.table)

dejtatejbyl<- function(data)
{
  tab <- as.data.table(cars)
  tab <- table(is.na(tab))
  tab[2]/(tab[1]+tab[2])
}

klasyk(cars)
dplyrr(cars)
dejtatejbyl(cars)
