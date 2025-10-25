# data_preprocessing.R

library(dplyr)
library(readr)

load_and_clean_data <- function(filepath) {
  data <- read_csv(filepath)
  
  # Basic cleaning
  data <- data %>%
    filter(!is.na(EconomicFreedomScore)) %>%
    mutate(Year = as.numeric(Year)) %>%
    arrange(Country, Year)
  
  return(data)
}
