# Processing raw data
# Select only relevant information for each city and merge into one tibble for all cities

library(tidyverse)

files <- c("data/raw/warszawa.csv", "data/raw/walbrzych.csv", "data/raw/olsztyn.csv")
city_names <- c("Warszawa", "Wałbrzych", "Olsztyn")

process_city <- function(city_name, path, years) {
  year_cols <- as.character(years)
  
  df <- read_csv2(path) %>% 
    slice(1, 16) %>% 
    select(`Podgrupa (wymiary)`, all_of(year_cols)) %>%
    rename(parametr = `Podgrupa (wymiary)`)
  
  prices <- df %>%
    slice(1) %>%
    select(all_of(year_cols)) %>%
    pivot_longer(cols = everything(), names_to = "year", values_to = "price")
  wages <- df %>%
    slice(2) %>%
    select(all_of(year_cols)) %>%
    pivot_longer(cols = everything(), names_to = "year", values_to = "wage")
  
  left_join(wages, prices, by="year") %>%
    mutate(year = as.integer(year)) %>%
    mutate(city = city_name, .before = 1)
}


process_cities <- function(years = 2013:2024) {
  city_dfs <- Map(process_city, city_names, files, MoreArgs = list(years = years))
  bind_rows(city_dfs)
}


get_years_range <- function(city_df) {
  city_df %>% 
    select(matches("\\d{4}")) %>% 
    select(where(~ !any(is.na(.)))) %>%
    colnames()
}
