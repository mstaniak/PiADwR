library(microbenchmark)
library(data.table)
library(tidyverse)

create_df <- function(N, G){
  data.frame(X = sample(LETTERS[1:G], N, replace = TRUE), Y = sample(1:N, N), W = rnorm(N), Z = rexp(N))
}

N <- c(10**2, 10**4, 10**6)
G <- c(10, 50, 100)
i <- 1
mbm_list <- list()

for (n in N){
  for (g in G){
    df <- create_df(n, g)
    dt <- as.data.table(df)
    tb <- as_tibble(df)
    
    mbm <- microbenchmark(
      base_stat = aggregate(. ~ X, FUN = median, data = df),
      tidy_stat = tb %>% group_by(X) %>% summarize_all(median),
      dt_stat = dt[ , lapply(.SD, median), by = X],
      times = 50
    )

    mbm_list[[i]] <- cbind(N = as.factor(n), G = as.factor(g), mbm)
    i <- i + 1
  }
}

mbm_all <- rbindlist(mbm_list)

ggplot(mbm_all, aes(expr, time)) +
  facet_grid(vars(N), vars(G), labeller = label_both) +
  geom_boxplot(aes(col=N, fill=G)) +
  xlab('Implementacja') +
  ylab('Czas') +
  theme(axis.text.x = element_text(angle = 90))
