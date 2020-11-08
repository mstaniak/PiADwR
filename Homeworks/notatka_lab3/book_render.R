library(bookdown)
# ?render_book
render_book(input = c('formaty_danych.Rmd','wczytywanie_plikow_notatka.rmd',
              'locales.RMD', 'rds_rda.rmd'), 'bookdown::gitbook')
