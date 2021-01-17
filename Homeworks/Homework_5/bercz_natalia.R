library(microbenchmark)
library(tidyverse)
library(data.table)

# microbenchmark
micbmark = function(N, G) {
  lst = list_of_data(N, G)
  data_frm = lst[[1]]
  data_tbl = lst[[2]]
  data_tib = lst[[3]]
  
  result = microbenchmark(mean_frm(data_frm), 
                          mean_tbl(data_tbl), 
                          mean_tib(data_tib))
  data.frame(do.call(cbind, result), G=G, N=N)
}

# data generation
list_of_data = function(N, G) {
  set.seed(1)
  data_frm = data.frame(X = sample(1:G, N, replace = TRUE), 
                        Y = runif(N), W = rnorm(N), Z = rexp(N))
  data_tbl = data.table(data_frm)
  data_tib = tibble(data_frm)
  
  list(data_frm,
       data_tbl,
       data_tib)
  }

# data.frame
mean_frm = function(dataframe) {
  aggregate(.~ X, dataframe, mean)
}

# data.table
mean_tbl = function(dataframe) {
  setDT(dataframe)[order(X), lapply(.SD, mean), by = .(X)]
}

# tibble
mean_tib = function(dataframe) {
  dataframe %>% 
    group_by(X) %>%
    summarise_each(funs(mean))
}

to_plot = function(N, G, rows, cols) {
  iter = 1
  bind = data.frame()
  for (row in 0:rows) {
    for (group in 0:cols) {
      micbmark = data.frame(micbmark(N-row, G-group), iter)
      iter = iter + 1
      bind = bind_rows(bind, micbmark)
    }
  }
  return(bind)
}

result = do.call(cbind.data.frame, to_plot(60, 6, 1, 1))

result$expr[result$expr == 1] = "base"
result$expr[result$expr == 2] = "data.table" 
result$expr[result$expr == 3] = "tidyverse"

# plot
ggplot(result, aes(x = expr, y = time)) +
  geom_boxplot() + ggtitle('Comparision') + 
  xlab('') + ylab('Time') + facet_wrap(~iter) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))