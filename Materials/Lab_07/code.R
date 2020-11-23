x_str <- letters[1:10]
x_fac <- as.factor(x_str)

x_fac[1:5]

df = data.frame(y = runif(100),
                x = factor(sample(letters[1:10], 100, replace = TRUE)))

aggregate(y ~  x, df, FUN = mean)

df2 = df[df$x %in% c("a", "b", "c"), ]
levels(df2$x)

aggregate(y ~  x, df[df$x %in% c("a", "b", "c"), ], FUN = mean)
aggregate(y ~  x, df[df$x %in% c("a", "b", "c"), ], FUN = mean, drop = F)


model.matrix(y ~ 0 + x, df2)

df3 = df2
df3$x = factor(df3$x)
levels(df3$x)

model.matrix(y ~ 0 + x, df3)

df4 = df3
df4$x = factor(df4$x, ordered = TRUE, levels = c("c", "b", "a"))

model.matrix(y ~ 0 + x, df4)
aggregate(y ~  x, df4, FUN = mean)

# df5 = df3
# df5$x = factor(df5$x, ordered = TRUE, levels = c("c", "b", "a"),
#                labels = c("cc", "bb", "aa"))
# df5
# # levels(df5$x)
# # model.matrix(y ~ 0 + x, df5)

y_fac <- as.factor(as.character(10:1))
as.numeric(y_fac)


x = sample(11:20, 5)
x
as.factor(x)
as.numeric(as.factor(x))
as.numeric(as.character(as.factor(x)))

library(dplyr)
library(lubridate)
library(stringr) # stringi

people <- "Mateusz Staniak, 24.01.1994, Wrocław"
str_split(people, ",")
str_match(people, "[A-Z][a-z]+ [A-Z][a-z]+, [0-9\\.]+, [A-Z][a-zł]+")
str_match(people, "([A-Z][a-z]+) ([A-Z][a-z]+), ([0-9\\.]+), ([A-Z][a-zł]+)")
str_match(people, "([A-Za-z ]+), ([0-9\\.]+), ([A-Z][a-zł]+)")

str_match(people, "([0-9\\.]+)")
str_match(people, "[0-9\\.]+")

dates <- c("7-2017", "2016-01", "12-2018", "2019-2")

# I
dates_separated <- str_split(dates, "-")
dates_with_zero <- lapply(dates_separated,
                          function(strvec) str_pad(strvec, width = 2, pad = "0",
                                                   side = "left"))

dates_reordered <- lapply(
  dates_with_zero, function(x) {
    if (str_length(x[1]) == 2) { # opcja 2: str_length(x[1]) < str_length(x[2])
      rev(x)
    } else {
      x
    }
  }
)


fix_date_order <- function(date_vec) {
  if(str_length(date_vec[1]) < 4) {
    date_vec <- rev(date_vec)
  }
  paste(c(date_vec, "01"), sep = "-", collapse = "-")
}


dates <- lapply(dates_with_zero, fix_date_order)
dates <- ymd(unlist(dates))
class(dates)

dates

month(dates)

year(dates)

day(dates)

wday(dates, label = TRUE)

datetimes <- paste(
  as.character(dates),
  paste(sample(0:23, 4), sample(0:59, 4), sample(0:59, 4),
        sep = ":"))
datetimes <- ymd_hms(datetimes, tz = "UTC")
tz(datetimes)


round_date(datetimes, unit = "hour")
round_date(datetimes, unit = "month")

ceiling_date(datetimes, unit = "hour")
ceiling_date(datetimes, unit = "month")


floor_date(datetimes, unit = "hour")
floor_date(datetimes, unit = "month")

month(datetimes) = month(datetimes) + 1
datetimes

datetimes[1] - datetimes[2]
class(datetimes[1] - datetimes[2])

as.numeric(datetimes[1] - datetimes[2])
as.period(datetimes[1] - datetimes[2]) / days(1)
as.period(datetimes[1] - datetimes[2]) / hours(1)
