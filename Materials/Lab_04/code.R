# Prerequsites ----
# https://github.com/WhyR2019/presentations/blob/master/EDA/staniak_autoEDA.pdf
# Download files from: https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily
# install.packages(c("dplyr",
#                    "lubridate",
#                    "readr",
#                    "data.table",
#                    "stringr",
#                    "tidyr"))
library(readr)
library(dplyr)
library(data.table)
# Import ----
gas_files <- list.files("./data", full.names = TRUE)
## base
gas_dfs <- lapply(gas_files, read.csv)
gas_dfs[[1]]
gas_base <- do.call("rbind", gas_dfs)
head(gas_base)
## tidyverse
gas_dfs_tv <- lapply(gas_files, read_csv)
gas_tv <- bind_rows(gas_dfs_tv)
gas_tv
## data.table
gas_dfs_dt <- lapply(gas_files, fread)
gas_dt <- rbindlist(gas_dfs_dt)
gas_dt
# Glimpse -----
## base
head(gas_base)
nrow(gas_base)
ncol(gas_base)
str(gas_base)
summary(gas_base)
## tidyverse
glimpse(gas_tv)
## data.table
summary(gas_dt)
head(gas_dt)

# Detour. Functions in R ----
mean(gas_dt$`1st Max Hour`)
# lapply(gas_files, read.csv)
read.csv
myread <- read.csv

mean_noNA <- function(numeric_vector) {
    mean(numeric_vector, na.rm = TRUE)
}
mean_noNA(gas_dt$`1st Max Hour`)
mean_noNA(numeric_vector = gas_dt$`1st Max Hour`)
mean_noNA2 <- function(numeric_vector, ...) {
    mean(numeric_vector, na.rm = TRUE, ...)
}
mean_noNA2(numeric_vector = gas_dt$`1st Max Hour`)
mean_noNA2(numeric_vector = gas_dt$`1st Max Hour`, trim  = 0.1)

num_unique <- function(input_vector) {
    length(unique(input_vector))
}
num_unique_noNA <- function(input_vector) {
    sum(!is.na(unique(input_vector)))
    # length(unique(input_vector[!is.na(input_vector)]))
    # length(unique(na.omit(input_vector)))
}

summary_noNA <- function(input_vector, summary_function) {
    if (summary_function == "mean") {
        mean(input_vector, na.rm = TRUE)
    } else if (summary_function == "sum") {
        sum(input_vector, na.rm = TRUE)
    } else {
        max(input_vector, na.rm = TRUE)
    }
}

summary_noNA(gas_base$Arithmetic.Mean, "mean")
summary_noNA(gas_base$Arithmetic.Mean, "sum")
summary_noNA(gas_base$Arithmetic.Mean, "max")

summary_noNA2 <- function(input_vector, summary_function) {
    summary_function(input_vector, na.rm = TRUE)
}

summary_noNA2(gas_base$Arithmetic.Mean, min)
summary_noNA2(gas_base$Arithmetic.Mean, max)
summary_noNA2(gas_base$Arithmetic.Mean, median)

y <- summary_noNA2
y(gas_base$Arithmetic.Mean, min)

summary_noNA3 <- function(input_vector, summary_function = mean) {
    summary_function(input_vector, na.rm = TRUE)
}

summary_noNA3(gas_base$Arithmetic.Mean, median)
summary_noNA3(gas_base$Arithmetic.Mean, mean)
summary_noNA3(gas_base$Arithmetic.Mean)

summary_noNA4 <- function(input_vector, summary_function = mean, ...) {
    summary_function(input_vector, na.rm = TRUE, ...)
}

summary_noNA4(gas_base$Arithmetic.Mean)
summary_noNA4(gas_base$Arithmetic.Mean, median)
summary_noNA4(gas_base$Arithmetic.Mean, trim = 0.1)
summary_noNA4(gas_base$Arithmetic.Mean, summary_function = quantile, probs = c(0.25, 0.75))

summary_noNA5 <- function() {
    mean(x, na.rm = TRUE)
}

rm(x)
summary_noNA5()
x <- 1:10
summary_noNA5()

# draw_bars = function(data, colors) {
#     ggplot(data, aes(x = x, y = y)) +
#         geom_bar() +
#         theme_bw()
# }

# Column operations ----
## Summarize all / selected columns
### base
apply(gas_base, 2, num_unique_noNA)
lapply(gas_base, num_unique_noNA)
sapply(gas_base, num_unique_noNA)
### tidyverse
summarise_all(gas_tv, num_unique_noNA)
### data.table
gas_dt[, lapply(.SD, num_unique_noNA)]
## Select columns
#### Relevant colums: Date Local, Arithmetic Mean, Sample Duration,
####  State Name, County Name, City Name, Local Site Name, Parameter Name
# unique(gas_base[, c("Parameter.Name", "Pollutant.Standard")])
# unique(gas_dt[, list(`Parameter Name`, Pollutant)])
#### Spaces in column names:
# colnames(gas_tv)
# gas_tv$`State Code`
### base
gas_base = gas_base[, c("State.Name", "County.Name", "City.Name",
                        "Local.Site.Name", "Date.Local", "Parameter.Name",
                        "Sample.Duration", "Arithmetic.Mean")]
### tidyverse
gas_tv = select(gas_tv, `Date Local`, `Arithmetic Mean`, `Sample Duration`, `State Name`,
                `County Name`, `City Name`, `Local Site Name`, `Parameter Name`)
gas_tv <- gas_tv %>%
    select(`Date Local`, `Arithmetic Mean`, `Sample Duration`, `State Name`,
           `County Name`, `City Name`, `Local Site Name`, `Parameter Name`)
### data.table
gas_dt = gas_dt[, c("State Name", "County Name", "City Name",
                    "Local Site Name", "Date Local", "Parameter Name",
                    "Sample Duration", "Arithmetic Mean")]
cols = c("State Name", "County Name", "City Name",
         "Local Site Name", "Date Local", "Parameter Name",
         "Sample Duration", "Arithmetic Mean")
gas_dt[, cols]
gas_dt[, ..cols]
gas_dt[, cols, with = FALSE]

gas_dt[, colnames(gas_dt) %in% c("State Name", "County Name")]
gas_dt[, colnames(gas_dt) %in% c("State Name", "County Name"), with = FALSE]

gas_dt[, list(`State Name`, `County Name`, `City Name`,
              `Local Site Name`, `Date Local`, `Parameter Name`,
              `Sample Duration`, `Arithmetic Mean`)]
gas_dt[, .(`State Name`, `County Name`, `City Name`,
           `Local Site Name`, `Date Local`, `Parameter Name`,
           `Sample Duration`, `Arithmetic Mean`)]

head(gas_dt)
## Rename columns
### base
colnames(gas_base) <- c("State", "County", "City", "Site", "Date",
                        "Pollutant", "SampleDuration", "MeasuredValue")
head(gas_base)
### tidyverse
gas_tv <- gas_tv %>%
    rename(Date = `Date Local`, MeasuredValue = `Arithmetic Mean`,
           SampleDuration = `Sample Duration`, State = `State Name`,
           County = `County Name`, `City` = `City Name`) %>%
    rename(Site = `Local Site Name`, Pollutant = `Parameter Name`)
### data.table
setnames(gas_dt, colnames(gas_dt), colnames(gas_base))
head(gas_dt)
## Rows: filtering
unique(gas_base$SampleDuration)
table(gas_base$SampleDuration)
prop.table(table(gas_base$SampleDuration))
# ### base
# gas_base <- gas_base[gas_base$SampleDuration == "1 HOUR", ]
# ### tidyverse
# gas_tv <- gas_tv %>%
#     filter(SampleDuration == "1 HOUR")
# ### data.table
# gas_dt <- gas_dt[SampleDuration == "1 HOUR"]
## Drop columns
### base
gas_base = gas_base[, -7]
### tidyverse
gas_tv = select(gas_tv, -SampleDuration)
### data.table
gas_dt[, SampleDuration := NULL]
## Mutate columns
### base
gas_base[["MeasuredValue"]] <- ifelse(gas_base[["MeasuredValue"]] < 0,
                                      0, gas_base[["MeasuredValue"]])
# any(gas_base$MeasuredValue < 0)
### tidyverse
gas_tv <- gas_tv %>%
    mutate(MeasuredValue = ifelse(MeasuredValue < 0, 0, MeasuredValue))
### data.table
# gas_dt[["MeasuredValue"]] <- ifelse(gas_dt[["MeasuredValue"]] < 0,
# 0, gas_dt[["MeasuredValue"]])
gas_dt[, MeasuredValue := ifelse(MeasuredValue < 0, 0, MeasuredValue)]
## Apply to all / selected columns
### base
for (i in 1:ncol(gas_base)) {
    if (is.factor(gas_base[, i])) {
        gas_base[, i] <- as.character(gas_base[, i])
    }
}
#### or lapply
### tidyverse
gas_tv <- gas_tv %>%
    mutate_all(function(x) {
        if (is.factor(x)) {
            as.character(x)
        } else {
            x
        }
    })
### data.table
gas_dt = gas_dt[, lapply(.SD, function(x) {
    if (is.factor(x)) {
        as.character(x)
    } else {
        x
    }
})]
## Grupowanie i podsumowania
### base
aggregate(MeasuredValue ~ State + County + City + Pollutant,
          data = gas_base,
          FUN = function(x) mean(x, na.rm = TRUE))
aggregate(MeasuredValue ~ State + County + City + Pollutant,
          data = gas_base,
          FUN = mean, na.rm = TRUE)
### tidyverse
gas_tv %>%
    group_by(State, County, City, Pollutant) %>%
    summarize(MeanMeasured = mean(MeasuredValue, na.rm = TRUE),
              MaxMeasured = max(MeasuredValue, na.rm = TRUE))
### data.table
gas_dt[, list(MeanMeasured = mean(MeasuredValue, na.rm = TRUE),
              MaxMeasured = max(MeasuredValue, na.rm = TRUE)),
       by = c("State", "County", "City", "Pollutant")]


library(reshape2)
library(tidyr)
library(data.table)

gas_tv


unique(gas_tv$Pollutant)
gas_tv
gas_dt


# gas_dt[, by='Pollutant']

spread(data,pollutant,measured_value)


# Wąska (long) -> szeroka (wide)
# State | County | City | Site | Date | CO (MeasuredValue dla Pollutant == "Carbon monoxide") | Ozone (MeasuredValue dla Pollutantt == "Ozone")
## tidyverse: tidyr
gas_tv_before_spread <- gas_tv %>%
    group_by(State, County, City, Site, Date, Pollutant) %>%
    summarize(MeasuredValue = mean(MeasuredValue, na.rm = TRUE))
gas_wide <- spread(gas_tv_before_spread, Pollutant, MeasuredValue)
gas_wide2 <- spread(gas_tv_before_spread, Pollutant, MeasuredValue, fill = -1)
filter(gas_wide2, is.na(`Carbon monoxide`))
# gas_tv[c(1, 290), ]
##
# Szeroka -> wąska
gas_long <- gather(gas_wide, "Pollutant", "MeasuredValue", `Carbon monoxide`, Ozone)
filter(gas_long, !is.na(MeasuredValue))
dim(gas_long)
dim(gas_tv_before_spread)
# data.table
## Wąska -> szeroka
gas_dt_wide <- dcast(gas_dt, State + County + City + Site + Date ~ Pollutant,
                     value.var = "MeasuredValue", fill = NA_real_,
                     fun.aggregate = function(x) mean(x, na.rm = TRUE))
## Szeroka -> wąska
gas_dt_long <- melt(gas_dt_wide,
                    id.vars = setdiff(colnames(gas_dt_wide), c("Carbon monoxide", "Ozone")),
                    measure.vars = c("Carbon monoxide", "Ozone"),
                    variable.name = "Pollutant", value.name = "MeasuredValue",
                    variable.factor = FALSE)
gas_dt_long

# Przykłady
gas_dt[, NumPollutants := uniqueN(Pollutant),
       by = c("State", "County", "City", "Site", "Date")]

gas_two <- gas_dt[NumPollutants > 1]
gas_two_wide <- dcast(gas_two, State + County + City + Site + Date ~ Pollutant,
                     value.var = "MeasuredValue",
                     fun.aggregate = function(x) mean(x, na.rm = TRUE))
gas_two_wide
setnames(gas_two_wide, "Carbon monoxide", "CO")

# Zadanie domowe:
# 1. Dla ramek danych gas_two_wide i gas_two
# -> obliczyć znormalizowane wartości MeasuredValue (odjęcie średniej, podzielenie przed odch. std.)
# (dla gas_two w postaci wąskiej, dla gas_two_wide w postaci szerokiej)
# dla gas_two: przez referencję i bez referencji
# dla gas_two_wide: przy użyciu lapply(), przy użyciu referencji i lapply(), lapply() bez referencji,
# bez lapply - z referencją i bez
# 2. Dla dowolnego miejsca:
# - przekonwertować do wersji szerokiej ze względu na ROK
# - wrócić do wersji wąskiej
# - przekonwertować do wersji szerokiej ze względu na miasto (różne miasta w jednym stanie)
# - znormalizować dla każdego miasta osobno
# 3. Dla gas_dt, zrobić to co na zajęciach ze średnimi bez użycia merge/join.

# gas_single <- gas_dt[State == "Alabama" & County == "Jefferson" &
#                          City == "Birmingham" & Site == "North Birmingham" &
#                          Pollutant == "Ozone"]
# dcast()


# tidyverse: left_join(), inner_join(), itd
# data.table: merge(), rodzaj joina zależy od all.x, all.y
gas_dt[, Year := year(Date)]
averages <- gas_dt[, .(AverageVal = mean(MeasuredValue, na.rm = TRUE)),
                   by = c("Pollutant", "State", "Year")]
averages <- averages[!is.na(AverageVal)]
averages
gas_dt[averages, on = c("State", "Pollutant", "Year")]
merge(gas_dt, averages,
      by = c("State", "Pollutant", "Year"),
      all.x = TRUE)
# setkey()
gas_dt[averages]


