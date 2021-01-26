library(bookdown)
# ?render_book
render_book(input = c('01-formaty_danych.Rmd','02-wczytywanie_plikow_notatka.rmd',
              '03-locales.Rmd', '04-rds_rda.rmd'), 'bookdown::gitbook')
