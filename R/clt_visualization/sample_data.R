library(tidyverse)


sample_data <- function(n, k = 1000, dist_func = rexp, transform_func = identity, ...) {
  raw_data = matrix(dist_func(n * k, ...), nrow = k, ncol = n)
  transformed_data <- transform_func(raw_data)
  sample_means <- rowMeans(transformed_data)
  
  tibble(
    mean_val = sample_means,
    n = n,
    transformation = deparse(substitute(transformer))
  )
}
  

sample_for_multiple_n <- function(n_vec, k = 10000, ...) {
  map_df(n_vec, function(curr_n) {
    sample_data(n = curr_n, k = k, ...)
  })
}
