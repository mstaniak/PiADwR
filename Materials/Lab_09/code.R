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
    summarize(n_obs_by_state = n()) %>%
    select(state, n_obs_by_state) %>%
    arrange(-n_obs_by_state) %>%
    ungroup() %>%
    mutate(id = 1:n()) %>%
    mutate(state = ifelse(id <= 20, state, "Other")) %>%
    group_by(state) %>%
    summarise(n_obs_by_state = sum(n_obs_by_state)) %>%
    ggplot(aes(x = reorder(state, n_obs_by_state),
               y = n_obs_by_state)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw() 
    
pollution %>%
    group_by(state) %>%
    mutate(n_obs_by_state = n()) %>%
    ggplot(aes(x = reorder(state, n_obs_by_state))) +
    geom_bar() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270))

# 
pollution %>%
    filter(state == "California") %>%
    pull(county) %>%
    head()
alameda <- pollution %>%
    filter(state == "California", county == "Alameda")
# Zmienność w czasie
alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    ggplot(aes(x = date, y = measurement)) +
        geom_point() +
        theme_bw()

alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    ggplot(aes(x = date, y = measurement,
               color = city)) +
    geom_point() +
    theme_bw()

alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    ggplot(aes(x = date, y = measurement,
               color = city)) +
    geom_smooth(aes(group = paste0(city, date_year)),
                size = 2) +
    # geom_point(alpha = 0.1) +
    theme_bw()

alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    group_by(date) %>%
    summarise(daily = mean(measurement)) %>%
    ggplot(aes(x = date, y = daily)) +
    geom_point() +
    geom_line() +
    theme_bw()


alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    group_by(date) %>%
    summarise(daily = mean(measurement)) %>%
    ggplot(aes(x = date, y = daily)) +
    geom_point() +
    theme_bw()

alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    group_by(date) %>%
    summarise(daily = mean(measurement),
              date_year = unique(date_year)) %>%
    ggplot(aes(x = date, y = daily)) +
    geom_point() +
    geom_line(aes(group = date_year)) +
    theme_bw()


alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    group_by(date) %>%
    summarise(daily = mean(measurement),
              date_year = unique(date_year)) %>%
    ggplot(aes(x = date, y = daily)) +
    geom_point() +
    geom_line(aes(group = date_year)) +
    geom_smooth(aes(group = date_year), method = 'lm', 
                se = FALSE) +
    theme_bw()


alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    group_by(date) %>%
    summarise(daily = mean(measurement),
              date_year = unique(date_year)) %>%
    ggplot(aes(x = date, y = daily)) +
    geom_point(alpha = 0.5) +
    geom_smooth(aes(group = date_year), method = 'loess', 
                se = FALSE, size = 1.5) +
    theme_bw()

alameda %>%
    filter(pollutant == "Carbon monoxide") %>%
    group_by(date) %>%
    summarise(daily = mean(measurement),
              date_year = unique(date_year)) %>%
    ggplot(aes(x = date, y = daily)) +
    geom_point(alpha = 0.5) +
    geom_smooth(aes(group = date_year), method = 'loess', 
                se = FALSE, size = 1.5) +
    theme_bw() +
    facet_wrap(~date_year, scales = "free_x")


alameda %>%
    group_by(date, pollutant) %>%
    summarise(daily = mean(measurement),
              date_year = unique(date_year)) %>%
    ggplot(aes(x = date, y = daily, group = pollutant, 
               color = pollutant)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = 'loess', 
                se = FALSE, size = 1.5) +
    theme_bw() +
    facet_grid(pollutant~date_year, scales = "free")

alameda2 <- filter(alameda, date_year == ymd('2019-01-01'))
mosaicplot(table(alameda2[['date_month']], alameda2[['measurement']] > 0))
table(alameda[['date_month']], alameda[['measurement']] > 0)
mosaicplot()