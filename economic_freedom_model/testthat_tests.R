library(testthat)
source("data_preprocessing.R")
source("model_training.R")
source("predictions.R")

test_that("model predicts future scores correctly", {
  data <- load_and_clean_data("economic_freedom.csv")
  model <- train_freedom_model(data)
  
  countries <- c("Canada", "United States", "Germany")
  
  for (country in countries) {
    preds <- predict_future_scores(model, data, country, years_ahead = 5)
    
    expect_equal(nrow(preds), 5)
    expect_true(all(!is.na(preds$PredictedScore)))
    expect_true(all(preds$PredictedScore > 0 & preds$PredictedScore < 100))
  }
})
