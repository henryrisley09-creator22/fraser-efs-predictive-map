# 6_future_covariates.R
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
})

# CONFIGURATION

YEARS_AHEAD <- 5          # how many years to project
DATA_DIR <- "data"
OUTPUT_FILE <- file.path(DATA_DIR, "future_covariates.csv")

# Ensure directory exists
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

# LOAD HISTORICAL PANEL 

if (!file.exists(file.path(DATA_DIR, "df_panel_raw.rds"))) {
  stop("Run 0_load_data.R first to create data/df_panel_raw.rds")
}

df_hist <- readRDS(file.path(DATA_DIR, "df_panel_raw.rds")) %>%
  rename_with(tolower)

# HANDLE YEAR COLUMN 
if (!"year" %in% names(df_hist)) {
  possible_year_col <- names(df_hist)[grepl("year", names(df_hist), ignore.case = TRUE)]
  if (length(possible_year_col) > 0) {
    df_hist <- df_hist %>% rename(year = all_of(possible_year_col[1]))
  } else {
    stop("No column containing 'year' found in df_hist.")
  }
}

# DEFINE TARGET PREDICTORS 
predictors <- c("inflation", "gdp_growth", "reg_quality", "trade_open", "fiscal_balance")

# Keep only existing predictors
present_predictors <- intersect(predictors, names(df_hist))
if (length(present_predictors) == 0) {
  message("No predictors found in df_hist. Creating placeholders with random noise.")
  # Create mock predictors for demo consistency
  df_hist <- df_hist %>%
    mutate(
      inflation = runif(n(), 1, 5),
      gdp_growth = runif(n(), -2, 5),
      reg_quality = runif(n(), 0, 1),
      trade_open = runif(n(), 30, 120),
      fiscal_balance = runif(n(), -5, 3)
    )
  present_predictors <- predictors
}

# SELECT LATEST YEAR PER COUNTRY 
df_latest <- df_hist %>%
  filter(!is.na(year)) %>%
  group_by(iso_code) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  select(iso_code, countries, year, all_of(present_predictors))

# SIMPLE PROJECTION MODEL 
set.seed(2025)
future_years <- max(df_latest$year, na.rm = TRUE) + seq_len(YEARS_AHEAD)

df_future <- df_latest %>%
  group_by(iso_code) %>%
  do({
    base <- .
    map_dfr(future_years, function(y) {
      tibble(
        iso_code = base$iso_code,
        countries = base$countries,
        year = y,
        gdp_growth = base$gdp_growth * runif(1, 0.9, 1.1),
        inflation = base$inflation * runif(1, 0.95, 1.05),
        reg_quality = base$reg_quality + rnorm(1, 0, 0.02),
        trade_open = base$trade_open * runif(1, 0.98, 1.02),
        fiscal_balance = base$fiscal_balance * runif(1, 0.95, 1.05)
      )
    })
  }) %>%
  ungroup()

# COMBINE HISTORICAL + FUTURE 

# Tag both datasets before combining
df_hist_tagged <- df_hist %>%
  mutate(is_forecast = FALSE) %>%
  select(iso_code, countries, year, all_of(present_predictors), is_forecast)

df_future_tagged <- df_future %>%
  mutate(is_forecast = TRUE) %>%
  select(iso_code, countries, year, all_of(present_predictors), is_forecast)

# Combine with consistent column order
df_combined <- bind_rows(df_hist_tagged, df_future_tagged)

# SAVE RESULTS 
write_csv(df_combined, OUTPUT_FILE)
saveRDS(df_combined, file.path(DATA_DIR, "future_covariates.rds"))

message("   Future covariates file created: ", OUTPUT_FILE)
message("   Contains ", nrow(df_combined), " rows across ", length(unique(df_combined$iso_code)), " countries.")
message("   Future years included: ", paste(unique(df_future$year), collapse = ", "))
