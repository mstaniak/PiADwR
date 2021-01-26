library(data.table)
library(ggplot2)

gas_file <- list.files("./data", full.names = TRUE)
gas_dfs_dt <- lapply(gas_file, fread)
gas_dt <- rbindlist(gas_dfs_dt)

new_gas_dt <- gas_dt[, c("State Code", "Arithmetic Mean")] 

new_gas_dt <- new_gas_dt[, MeanValue := mean(`Arithmetic Mean`, na.rm = TRUE), by = "State Code"]
gas_dt2 <- data.table(unique(new_gas_dt$`State Code`), unique(new_gas_dt$MeanValue))

setnames(gas_dt2, c("Code", "Mean"))

plot_maker <- function(data, x, y, type, ...){
  if (type %in% c("geom_point()", "geom_col()")){
    ggplot(data, aes(x, y)) + eval(parse(text = type)) + theme(...)
  }
  else{
    print("You need to choose one of two types: geom_point(), geom_col()")
  }
}


plot_maker(gas_dt2, gas_dt2$Code, gas_dt2$Mean, "geom_point()")
plot_maker(gas_dt2, gas_dt2$Code, gas_dt2$Mean, "geom_col()")
plot_maker(gas_dt2, gas_dt2$Code, gas_dt2$Mean, "geom_line()") #to show that "else" works

