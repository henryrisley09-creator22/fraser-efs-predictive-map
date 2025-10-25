library(tidyverse)
library(forecast)
library(readr)
library(lubridate)
library(data.table)
library(parallel)

# install.packages("caret"); install.packages("xgboost")
suppressPackageStartupMessages({
  library(caret)
  library(xgboost)
})

# PARAMETERS
H <- 3  # forecast horizon in years
MIN_YEARS <- 8  # minimum historical years required to fit model
OUTPUT_DIR <- "outputs"
FIG_DIR <- file.path("figures", "forecasts")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# Load scored panel (expects EFS_pca or EFS_manual to exist)
if (!file.exists("data/df_scores.rds")) {
  stop("Required file data/df_scores.rds not found. Run 2_compute_scores.R first.")
}
df_scores <- readRDS("data/df_scores.rds") %>% as_tibble()

# Determine which EFS column to use
efs_col <- if ("EFS_pca" %in% names(df_scores)) "EFS_pca" else if ("EFS_manual" %in% names(df_scores)) "EFS_manual" else if ("EFS_eq" %in% names(df_scores)) "EFS_eq" else stop("No EFS column found (EFS_pca, EFS_manual, or EFS_eq expected).")

message("Using EFS column: ", efs_col)

# Quick tidy
df_scores <- df_scores %>%
  select(iso, country, year, all_of(efs_col), everything()) %>%
  mutate(year = as.integer(year)) %>%
  arrange(iso, year)

# Helper: per-country ARIMA forecast function
forecast_country_arima <- function(df_country, h = H, efs_col_local = efs_col) {
  # df_country should be rows for one country sorted by year
  df_country <- df_country %>% arrange(year)
  yrs <- df_country$year
  vals <- df_country[[efs_col_local]]
  # Remove NA trailing or leading
  non_na_idx <- which(!is.na(vals))
  if (length(non_na_idx) < 2) {
    return(NULL)
  }
  vals_trim <- vals[min(non_na_idx):max(non_na_idx)]
  yrs_trim <- yrs[min(non_na_idx):max(non_na_idx)]
  if (length(vals_trim) < MIN_YEARS) {
    # still attempt but flag as low-sample
    low_sample <- TRUE
  } else {
    low_sample <- FALSE
  }
  # convert to ts object with yearly frequency
  start_year <- min(yrs_trim)
  ts_data <- ts(vals_trim, start = start_year, frequency = 1)
  # Try auto.arima, with tryCatch to handle failures
  fit <- tryCatch({
    auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
  }, error = function(e) {
    # fallback: naive ETS
    ets(ts_data)
  })
  fc <- forecast(fit, h = h, level = c(80, 95))
  # Build output frame
  future_years <- (max(yrs_trim) + 1):(max(yrs_trim) + h)
  out <- tibble(
    iso = df_country$iso[1],
    country = df_country$country[1],
    model = class(fit)[1],
    year = c(yrs_trim, future_years),
    is_forecast = c(rep(FALSE, length(vals_trim)), rep(TRUE, length(future_years))),
    point = c(as.numeric(fitted(fit)), as.numeric(fc$mean)),
    lo80 = c(rep(NA_real_, length(vals_trim)), as.numeric(fc$lower[,1])),
    hi80 = c(rep(NA_real_, length(vals_trim)), as.numeric(fc$upper[,1])),
    lo95 = c(rep(NA_real_, length(vals_trim)), as.numeric(fc$lower[,2])),
    hi95 = c(rep(NA_real_, length(vals_trim)), as.numeric(fc$upper[,2])),
    low_sample = low_sample
  )
  return(list(fit = fit, forecast_df = out, ts_data = ts_data, fc = fc))
}

# Parallel apply across countries
country_list <- df_scores %>% distinct(iso, country) %>% arrange(iso)
isos <- country_list$iso

# For speed, define a function to run per-iso
run_one_iso <- function(iso_code) {
  subdf <- df_scores %>% filter(iso == iso_code) %>% arrange(year)
  # need at least 3 non-NA
  if (sum(!is.na(subdf[[efs_col]])) < 4) {
    return(NULL)
  }
  res <- forecast_country_arima(subdf, h = H, efs_col_local = efs_col)
  if (is.null(res)) return(NULL)
  # Save plot for diagnostics
  # We'll plot actual historical values and forecast with 95% CI
  try({
    png(filename = file.path(FIG_DIR, paste0(iso_code, "_forecast.png")), width = 900, height = 500)
    plot(res$fc, main = paste0(subdf$country[1], " (", iso_code, ") — EFS forecast"), xlab = "Year", ylab = "EFS", flty = 2)
    points(res$ts_data, col = "blue")
    dev.off()
  }, silent = TRUE)
  return(res$forecast_df)
}

# Use mclapply (parallel) where available
ncores <- detectCores(logical = FALSE) - 1
if (is.na(ncores) || ncores < 1) ncores <- 1

results <- mclapply(isos, run_one_iso, mc.cores = ncores)

# Combine results
results_df <- bind_rows(results)

# Keep only forecast rows for export and ranking
forecast_rows <- results_df %>% filter(is_forecast == TRUE)

# If some countries had short histories, they are flagged low_sample; keep them but note
forecast_rows <- forecast_rows %>%
  mutate(point = as.numeric(point),
         lo95 = as.numeric(lo95),
         hi95 = as.numeric(hi95))

# Save full forecast table
out_csv_full <- file.path(OUTPUT_DIR, paste0("efs_forecasts_full_h", H, ".csv"))
write_csv(results_df, out_csv_full)
message("Saved full forecast table to ", out_csv_full)

# Save only future-year forecasts
out_csv_future <- file.path(OUTPUT_DIR, paste0("efs_forecasts_future_h", H, ".csv"))
write_csv(forecast_rows, out_csv_future)
message("Saved future forecast rows to ", out_csv_future)

# Create a summary for the last forecast year (max predicted year)
forecast_year <- max(forecast_rows$year, na.rm = TRUE)
summary_forecast <- forecast_rows %>%
  filter(year == forecast_year) %>%
  arrange(desc(point)) %>%
  mutate(rank_forecast = row_number(desc(point))) %>%
  select(rank_forecast, iso, country, year, point, lo95, hi95, low_sample)

out_summary <- file.path(OUTPUT_DIR, paste0("efs_forecast_summary_", forecast_year, ".csv"))
write_csv(summary_forecast, out_summary)
message("Saved forecast summary for year ", forecast_year, " to ", out_summary)

# Top 10 forecast printout
message("Top 10 forecasted EFS for ", forecast_year, ":")
print(summary_forecast %>% head(10))

# OPTIONAL: If macro covariates exist (e.g., GDP or inflation), produce an ML ensemble forecast
macro_candidates <- c("GDP_per_capita", "gdp", "GDP", "inflation", "CPI", "unemployment")
present_macro <- intersect(tolower(names(df_scores)), tolower(macro_candidates))
# We'll standardize column detection
macro_cols <- names(df_scores)[tolower(names(df_scores)) %in% present_macro]

if (length(macro_cols) > 0) {
  message("Found macro covariates: ", paste(macro_cols, collapse = ", "), ". Running optional ML ensemble forecast (country-level lag features).")
  # NOTE: This block is intentionally conservative and will only run if covariates present.
  # Build dataset with lagged features by country and train a single XGBoost per country (lightweight)
  # For brevity and safety, we do a simple lag-1 feature regression if data is available.
  ml_forecasts <- list()
  for (iso_code in isos) {
    sub <- df_scores %>% filter(iso == iso_code) %>% arrange(year)
    if (nrow(sub) < MIN_YEARS) next
    # check macro columns non-NA
    if (all(is.na(sub[[macro_cols[1]]]))) next
    df_ml <- sub %>% select(year, efs = all_of(efs_col), all_of(macro_cols)) %>% mutate(across(everything(), ~ as.numeric(.)))
    # create lag-1 features
    df_ml <- df_ml %>% mutate(across(all_of(macro_cols), ~lag(.x, 1), .names = "lag_{col}"))
    df_ml <- df_ml %>% filter(!is.na(efs) & !is.na(!!sym(paste0("lag_", macro_cols[1]))))
    if (nrow(df_ml) < MIN_YEARS-1) next
    # Train simple xgboost using caret wrapper
    train_idx <- 1:(nrow(df_ml)-1)
    test_idx <- nrow(df_ml)
    train_x <- df_ml[train_idx, paste0("lag_", macro_cols), drop=FALSE] %>% as.matrix()
    train_y <- df_ml$efs[train_idx]
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective = "reg:squarederror", max_depth = 3, eta = 0.1, subsample = 0.8)
    xgb_mod <- xgb.train(params, dtrain, nrounds = 50, verbose = 0)
    # Predict next H steps by rolling forward (requires macro forecasts; we skip if not available)
    # For safety, only predict 1 step if macro future covariates not present
    # We'll forecast only next year with ml if possible
    last_macro_lag <- as.numeric(tail(df_ml, 1)[paste0("lag_", macro_cols)])
    pred <- predict(xgb_mod, matrix(last_macro_lag, nrow=1))
    ml_forecasts[[iso_code]] <- tibble(iso = iso_code, country = sub$country[1], year = max(sub$year)+1, ml_point = pred)
  }
  if (length(ml_forecasts) > 0) {
    ml_df <- bind_rows(ml_forecasts)
    write_csv(ml_df, file.path(OUTPUT_DIR, paste0("efs_ml_forecasts_step1.csv")))
    message("Saved simple ML forecasts (1-step) to outputs/efs_ml_forecasts_step1.csv")
  }
}

message("Forecasting complete. Check outputs/ and figures/forecasts for plots and CSVs.")
