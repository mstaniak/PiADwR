library(dplyr)
library(lubridate)
library(readr)

file_paths <- list.files('.', ".csv")
read_files <- lapply(file_paths, read_csv)
read_files_corrected <- lapply(
    read_files, 
    function(df) 
        mutate(df, `Method Code` = as.character(`Method Code`)))
pollution <- bind_rows(read_files_corrected)

library(stringr)

contrast <- "KO_early-WT_late"

people <- "Mateusz Staniak, 24.01.1994, Wrocław"
str_split(people, ",")
str_match(people, "([A-Z][a-z]+) ([A-Z][a-z]+), ([0-9\\.]+), ([A-Z][a-zł]+)")
str_match(people, "([A-Za-z ]+), ([0-9\\.]+), ([A-Z][a-zł]+)")

str_match(people, "([0-9\\.]+)")

people2 <- "Mateusz Staniak"
str_match_all(people, "[A-Z][a-zł]+")


dates <- c("7-2017", "2016-01", "12-2018", "2019-2")

# I 
dates_separated <- str_split(dates, "-")
dates_with_zero <- lapply(dates_separated,
                          function(strvec) str_pad(strvec, width = 2, pad = "0",
                                                   side = "left"))
fix_date_order <- function(date_vec) {
    if(str_length(date_vec[1]) < 4) {
      date_vec <- rev(date_vec)  
    }
    paste(c(date_vec, "01"), sep = "-", collapse = "-")
}
dates <- lapply(dates_with_zero, fix_date_order)
dates <- ymd(unlist(dates))
class(dates)


dates_with_zero <- lapply(
    dates_separated,
    fix_padding
)

fix_padding <- function(strvec) str_pad(strvec, width = 2, pad = "0",
                                        side = "left")

# Funkcja jako parametr
calculate_norm <- function(vec, norm) {
    norm(vec)
}
calculate_norm(c(1, 2), function(x) sqrt(sum(x^2)))
calculate_norm(c(1, 2), function(x) sum(abs(x)))

power <- function(power) {
    function(x) x^power
}

power_1 <- power(1)
power_2 <- power(2)
power_3 <- power(3)


power_1(1:10)
power_2(1:10)
power_3(1:10)
