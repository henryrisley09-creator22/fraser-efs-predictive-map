# 4_changepoint_detect.R
# Detect significant changes in country EFS time series (e.g., CUSUM or changepoint)
library(dplyr)
library(changepoint)
library(broom)

df_scores_all <- readRDS("data/df_scores.rds")

# We'll use EFS_pca time series per country and look for structural breaks
detect_country_changepoints <- function(country_df) {
  cs <- country_df %>% arrange(year)
  if (nrow(cs) < 6) return(NULL)  # not enough obs
  # Remove NAs
  vec <- cs$EFS_pca
  if (sum(!is.na(vec)) < 6) return(NULL)
  # Apply changepoint detection on mean (could be mean+variance)
  tryCatch({
    cp <- cpt.mean(vec, method = "PELT", penalty = "MBIC")
    cps <- cpts(cp)  # indices of changepoints
    if (length(cps) == 0) return(NULL)
    data.frame(
      iso = cs$iso[1],
      country = cs$country[1],
      changepoints_index = paste(cps, collapse = ";"),
      changepoints_year = paste(cs$year[cps], collapse = ";"),
      n_obs = nrow(cs),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}

# Apply per country
country_list <- split(df_scores_all, df_scores_all$iso)
results <- lapply(country_list, detect_country_changepoints)
results_df <- do.call(rbind, results)

# Save
if (!is.null(results_df) && nrow(results_df) > 0) {
  write_csv(results_df, file = "outputs/changepoints_by_country.csv")
  saveRDS(results_df, file = "data/changepoints_by_country.rds")
  message("Changepoints detected for ", nrow(results_df), " countries.")
} else {
  message("No changepoints found (or insufficient data).")
}
