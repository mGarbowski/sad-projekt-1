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

plot_theme <- theme_minimal() + theme(panel.grid.minor.x = element_blank())

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
    plot_theme
}

plot_price_to_wage_ratio <- function(df) {
  data <- df %>% 
    mutate(ratio = price / wage)
  ggplot(data) +
    geom_line(aes(x = year, y = ratio, color = city)) +
    scale_x_continuous(breaks = data$year) +
    labs(
      title = paste("Stosunek ceny m2 do miesięcznego wynagrodzenia"),
      x = "Rok",
      y = "Cena / Wynagrodzenie",
      color = "Miasto"
    ) +
    plot_theme
}

plot_wage_by_city <- function(df) {
  ggplot(df) +
    geom_line(aes(x = year, y=wage, color=city)) +
    scale_x_continuous(breaks = df$year) +
    labs(
      title = "Przeciętne miesięczne wynagrodzenie brutto",
      color = "Miasto",
      x = "Rok",
      y = "Kwota"
      ) +
    plot_theme
}

plot_price_by_city <- function(df) {
  ggplot(df) +
    geom_line(aes(x = year, y=price, color=city)) +
    scale_x_continuous(breaks = df$year) +
    labs(
      title = "Mediana cen za 1 m2 lokali mieszkalnych",
      color = "Miasto",
      x = "Rok",
      y = "Kwota"
    ) +
    plot_theme
    
}
  
df <- load_data()
plot_price_vs_wage_for_city(df, "Warszawa")
plot_price_to_wage_ratio(df)
plot_wage_by_city(df)
plot_price_by_city(df)