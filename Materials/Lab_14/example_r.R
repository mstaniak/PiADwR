library(dplyr)
library(ggplot2)
library(MASS)

n_nonzero <- c(10, 100, 200)
n_obs <- c(10000, 1000, 500)
size <- c(1, 2, 5)

single_experiment <- function(non_zero, obs, s) {
    runif(1)
}

results_raw <- lapply(n_nonzero, function(non_zero){
    lapply(n_obs, function(obs) {
        lapply(size, function(s) {
            data.frame(
                n_nonzero = non_zero,
                n_obs = obs,
                size = s,
                stat = single_experiment(non_zero, obs, s)
            )
        }
        )}
    )}
)

bind_rows(unlist(unlist(results_raw, recursive = FALSE), recursive = FALSE)) %>%
    ggplot(aes(x = n_obs, y = stat, color = as.character(n_nonzero))) +
        geom_point() +
        theme_bw() +
        facet_wrap(~size)

