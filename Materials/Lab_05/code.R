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

table(pollution[['Event Type']])

pollution <- filter(pollution, `Event Type` == "None")
pollution <- filter(pollution, 
                    !(`Event Type` %in% c("Excluded", "Included")))

pollution <- select(pollution, `Date Local`, `Parameter Name`,
                    `Arithmetic Mean`, `State Name`, `County Name`,
                    `City Name`)
pollution <- rename(pollution, 
                    date = `Date Local`,
                    pollutant = `Parameter Name`,
                    measurement = `Arithmetic Mean`,
                    state = `State Name`,
                    county = `County Name`,
                    city = `City Name`)
pollution <- mutate(pollution, 
                    weekday = wday(date, label = TRUE),
                    date_month = floor_date(date, 'month'),
                    date_year = floor_date(date, 'year'))
min(pollution[["date"]])
days_diff <- pollution[["date"]] - min(pollution[['date']])
head(days_diff)

means_by_month <- pollution %>%
    group_by(date_month, pollutant, state, county, city) %>%
    summarise(mean_pollution = mean(measurement, na.rm = TRUE))


pollution %>%
    summarise_each(list(column_class = function(column) class(column)[1]))

library(ggplot2)
head(pollution)

just_carbon <- filter(pollution, pollutant == "Carbon monoxide")

ggplot(just_carbon, aes(x = measurement)) +
    geom_histogram() +
    # scale_x_sqrt() +
    # scale_y_log10() +
    theme_bw()

ggplot(just_carbon, aes(x = measurement)) +
    geom_histogram(binwidth = 0.1) +
    # scale_x_sqrt() +
    scale_y_log10() +
    theme_bw()



ggplot(just_carbon, aes(x = measurement)) +
    geom_density() +
    # scale_x_sqrt() +
    # scale_y_log10() +
    theme_bw()


ggplot(just_carbon, aes(y = measurement, x = "measurement")) +
    geom_violin() +
    # scale_x_sqrt() +
    # scale_y_log10() +
    theme_bw()

# 
head(pollution)
ggplot(pollution, aes(x = state)) +
    geom_bar() +
    theme_bw()

pollution %>%
    group_by(state) %>%
    mutate(n_obs_by_state = n()) %>%
    ggplot(aes(x = reorder(state, -n_obs_by_state))) +
    geom_bar() +
    theme_bw()

pollution %>%
    group_by(state) %>%
    mutate(n_obs_by_state = n()) %>%
    ggplot(aes(x = reorder(state, n_obs_by_state))) +
    geom_bar() +
    coord_flip() +
    theme_bw() 
    
pollution %>%
    group_by(state) %>%
    mutate(n_obs_by_state = n()) %>%
    ggplot(aes(x = reorder(state, n_obs_by_state))) +
    geom_bar() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270))
