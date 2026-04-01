# Plots for wage/house price data in different cities

library(tidyverse)

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