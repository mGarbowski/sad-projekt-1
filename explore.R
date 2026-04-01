library(tidyverse)
load_city <- function(city_name, path, years) {
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

get_years <- function(df) {
  df %>% 
    select(matches("\\d{4}")) %>% 
    select(where(~ !any(is.na(.)))) %>%
    colnames()
}

load_data <- function(years = 2013:2024) {
  bind_rows(
    load_city("Warszawa", "data/warszawa.csv", years),
    load_city("Wałbrzych", "data/walbrzych.csv", years),
    load_city("Olsztyn", "data/olsztyn.csv", years),
  )
}

files <- c("data/warszawa.csv", "data/walbrzych.csv", "data/olsztyn.csv")
years <- files %>%
  map(~ read_csv2(.)) %>%
  map(~ get_years(.))


plot_price_vs_wage_for_city <- function(df, city_name) {
  data <- df %>% filter(city == city_name) 
  ggplot(data) +
    geom_line(aes(x = year, y = price, color="Cena za m2")) +
    geom_line(aes(x = year, y = wage, color = "Wynagrodzenie")) +
    scale_x_continuous(breaks = data$year) +
    labs(
      title = paste(city_name, "- miesięczne wynagrodzenie vs cena za m2 mieszkania"),
      x = "Rok",
      y = "Kwota",
      color = ""
      ) +
    theme_minimal()
}
  
df <- load_data()
plot_price_vs_wage_for_city(df, "Warszawa")