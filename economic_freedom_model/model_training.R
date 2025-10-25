# model_training.R

library(dplyr)
library(caret)

train_freedom_model <- function(data) {
  # Create lag feature
  data <- data %>%
    group_by(Country) %>%
    mutate(PrevScore = lag(EconomicFreedomScore)) %>%
    filter(!is.na(PrevScore))
  
  # Train model
  model <- train(
    EconomicFreedomScore ~ PrevScore + GDP + Inflation + TradeFreedom + PropertyRights,
    data = data,
    method = "lm"
  )
  
  return(model)
}
