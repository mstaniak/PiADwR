library(ggplot2)
library(data.table)
library(microbenchmark)
library(tibble)
library(tidyverse)

# rozne ilosci wierszy (N) i rozne ilosci grup w kolumnie X (G)
N <- c(100, 100000, 1000000)
G <- c(10, 20, 50)

# proste kategorie
groups <- union(letters, LETTERS)

# grid dla wykresow
par(mfrow = c(3, 3))

# robimy wszystko w podwojnej petli
for (n in N){
  
  for (g in G){
    
    X_col <- sample(x = groups[1:G], replace = TRUE, size = N)
    Y_col <- 1:N
    W_col <- N:(2*N-1)
    Z_col <- (2*N):(3*N-1)
    
    df_dt <- data.table(
      X = X_col,
      Y = Y_col,
      W = W_col,
      Z = Z_col 
    )
    
    df_base <- data.frame(
      X = X_col,
      Y = Y_col,
      W = W_col,
      Z = Z_col 
    )
    
    df_tibble <- as_tibble(df_base)
    
    sample <- microbenchmark(
      
      DT = df_dt[, sum(Y + W + Z), by = c("X")],
      
      DF = aggregate(Y + W + Z ~ X, data = df_base,
                        FUN = sum),
      
      TV = func_tv <- df_tibble %>%
                        group_by(X) %>%
                        summarize(sum(Y + W + Z)),
      times = 30
    )
    
    boxplot(x = sample, 
            main = paste("N =", as.character(n),"G = ", as.character(g)),
            xlab = "Implementation"
    )
  }
}