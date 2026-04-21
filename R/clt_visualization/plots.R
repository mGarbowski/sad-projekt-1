library(tidyverse)


plot_hist <- function(data, n_bins = 20) {
  theoretical_curves <- data %>%
    group_by(n) %>%
    do({
      mu_emp <- mean(.$mean_val)
      sd_emp <- sd(.$mean_val)
      
      range_val <- seq(min(.$mean_val), max(.$mean_val), length.out = 100)
      data.frame(
        x = range_val,
        y = dnorm(range_val, mean = mu_emp, sd = sd_emp)
      )
    })
  
  ggplot(data, aes(x = mean_val)) +
    geom_histogram(aes(y = after_stat(density)), bins = n_bins, 
                   fill = "steelblue", color = "white", alpha = 0.7) +
    geom_density(color = "red", linewidth = 1) +
    geom_line(data = theoretical_curves, aes(x = x, y = y), 
              color = "darkgreen", linetype = "dashed", linewidth = 1) +
    facet_wrap(~n, scales = "free", labeller = label_both) +
    theme_minimal() +
    labs(title = "Histogram i estymator jądrowy gęstości",
         subtitle = "Czerwona linia: gęstość empiryczna, zielona przerywana: teoretyczny Gauss",
         x = "Wartość statystyki", 
         y = "Gęstość") +
    theme(legend.position = "none")
}


plot_qq <- function(data) {
  ggplot(data, aes(sample = mean_val)) +
    stat_qq(color = "steelblue", alpha = 0.5) +
    stat_qq_line(color = "red", linewidth = 0.8) +
    facet_wrap(~n, scales = "free", labeller = label_both) +
    theme_minimal() +
    labs(
      title = "Wykres kwantyl-kwantyl (Q-Q Plot)",
      subtitle = "Czerwona linia oznacza idealny rozkład normalny",
      x = "Kwantyle teoretyczne",
      y = "Kwantyle z próby"
    )
}
