freq_NA <- function(x, method)
{
  require(tidyverse)
  require(data.table)
  if(! (method %in% c("tidyverse", "data.table", "base")))
    stop('Method has to be tidyverse, data.table or base')
  fun <- function(x)
  {
    sum(is.na(x)) / length(x)
  }
  if(method == "base")
    result <- lapply(x, fun)
  if(method == "tidyverse")
    result <- summarise_all(x, fun)
  if(method == "data.table")
    result <- data.table(x)[, lapply(.SD, fun)]
  return(result)
}
freq_NA(airquality, "base")
freq_NA(airquality, "tidyverse")
freq_NA(airquality, "data.table")
