library(dplyr)

predict_future_scores <- function(model, data, country_name, years_ahead = 5) {
  country_data <- data %>% filter(Country == country_name)
  last_row <- tail(country_data, 1)
  
  predictions <- data.frame(Year = (last_row$Year + 1):(last_row$Year + years_ahead))
  
  for (i in 1:years_ahead) {
    new_score <- predict(model, newdata = last_row)
    new_row <- last_row
    new_row$Year <- new_row$Year + 1
    new_row$PrevScore <- new_score
    last_row <- new_row
    predictions$PredictedScore[i] <- new_score
  }
  
  predictions$Country <- country_name
  return(predictions)
}
