source("R/data_processing.R")
process_cities() %>% write_csv("data/processed/cities.csv")
