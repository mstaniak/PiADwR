install.packages("datasauRus")
library(datasauRus)
library(data.table)


?datasauRus::datasaurus_dozen

ds = as.data.table(datasauRus::datasaurus_dozen)
head(ds)


uniqueN(ds$dataset)
unique(ds$dataset)

ds[ , list(mean_x = mean(x),
           mean_y = mean(y),
           cor_xy = cor(x, y, method = "pearson")),
   by = "dataset"]

library(ggplot2)

ggplot(ds[dataset == "circle"], aes(x = x, y = y)) +
    geom_point() +
    theme_bw()

ggplot(ds, aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~dataset, nrow = 5) +
    theme_bw()

ggplot(ds, aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~dataset, scales = "free") + # scales = "free_x", scales = "free_y"
    theme_bw()

ggplot(ds, aes(x = x, y = y)) +
    geom_point() +
    facet_grid(~dataset) +
    theme_bw()

ggplot(ds, aes(y = y)) +
    geom_boxplot() +
    theme_bw()
ggplot(ds, aes(y = x)) +
    geom_boxplot() +
    theme_bw()

ggplot(ds, aes(x = dataset, y = y)) +
    geom_boxplot() +
    theme_bw()

ggplot(ds, aes(x = dataset, y = x)) +
    geom_boxplot() +
    theme_bw()


ggplot(ds, aes(x = dataset, y = x)) +
    geom_boxplot() +
    coord_flip() +
    theme_bw()


ggplot(ds, aes(x = dataset, y = x)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270)) # np. , size = 12 itd
