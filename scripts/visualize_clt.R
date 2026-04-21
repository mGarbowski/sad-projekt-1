library(tidyverse)
source("R/clt_visualization/sample_data.R")
source("R/clt_visualization/plots.R")


n_values <- c(20)
k_sim <- 50000

for (current_n in n_values) {
  
  sim_results <- sample_data(
    n = current_n, 
    k = k_sim, 
    dist_func = runif,
    transform_func = function(x) log(x)
  )
  
  p <- plot_hist(sim_results) +
    labs(subtitle = paste("Klatka animacji dla n =", current_n))
  
  print(p)
  
  Sys.sleep(1)
}