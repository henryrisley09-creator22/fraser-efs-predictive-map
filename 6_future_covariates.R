
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ------------------ CONFIGURATION ------------------
YEARS_AHEAD <- 5          # how many years to project
DATA_DIR <- "data"
OUTPUT_FILE <- file.path(DATA_DIR, "future_covariates.csv")

# Ensure directory exists
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------ LOAD HISTORICAL PANEL ------------------
# You should already have df_panel_raw.rds from 0_load_data.R
if (!file.exists(file.path(DATA_DIR, "df_panel_raw.rds"))) {
  stop("Run 0_load_data.R first to create data/df_panel_raw.rds")
}

df_hist <- readRDS(file.path(DATA_DIR, "df_panel_raw.rds"))

# ------------------ BASIC PREDICTORS ------------------
# We'll focus on inflation, gdp_growth, regulatory quality, trade openness, fiscal balance
predictors <- c("inflation", "gdp_growth", "reg_quality", "trade_open", "fiscal_balance")

# Keep only latest available year per country
df_hist <- df_hist %>% rename_with(tolower)

# Ensure we have a 'year' column
if (!"year" %in% names(df_hist)) {
  possible_year_col <- names(df_hist)[grepl("year", names(df_hist), ignore.case = TRUE)]
  if (length(possible_year_col) > 0) {
    df_hist <- df_hist %>% rename(year = all_of(possible_year_col[1]))
  } else {
    stop("No column containing 'year' found in df_hist.")
  }
}

# Now safely group and filter
df_hist <- df_hist %>% rename_with(tolower)

# Ensure a valid 'year' column exists
if (!"year" %in% names(df_hist)) {
  possible_year_col <- names(df_hist)[grepl("year", names(df_hist), ignore.case = TRUE)]
  if (length(possible_year_col) > 0) {
    df_hist <- df_hist %>% rename(year = all_of(possible_year_col[1]))
  } else {
    stop("No column containing 'year' found in df_hist.")
  }
}

# Define predictors you WANT to use (future-ready)
predictors <- c("inflation", "gdp_growth", "reg_quality", "trade_open", "fiscal_balance")

# Only keep predictors that actually exist
present_predictors <- intersect(predictors, names(df_hist))

# Prevent select() from failing: use if/else logic
if (length(present_predictors) == 0) {
  message("No predictor columns found in df_hist. Proceeding with base columns only.")
  df_latest <- df_hist %>%
    filter(!is.na(year), !is.na(iso_code)) %>%
    group_by(iso_code) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    ungroup() %>%
    select(iso_code, countries, year)
} else {
  df_latest <- df_hist %>%
    filter(!is.na(year), !is.na(iso_code)) %>%
    group_by(iso_code) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    ungroup() %>%
    select(iso_code, countries, year, all_of(present_predictors))
}

message("df_latest successfully created with ", nrow(df_latest), " countries and ",
        ifelse(length(present_predictors) > 0,
               paste(length(present_predictors), "predictors."),
               "no extra predictors."))
# ------------------ SIMPLE PROJECTION MODEL ------------------
# You can replace this section later with actual IMF / OECD data
# For now, we build simple autoregressive (AR1-like) trend extrapolations

set.seed(2025)
future_years <- max(df_latest$year, na.rm = TRUE) + seq_len(YEARS_AHEAD)

df_future <- df_latest %>%
  group_by(ISO_Code) %>%
  do({
    base <- .
    purrr::map_dfr(future_years, function(y) {
      tibble(
        ISO_Code = base$ISO_Code,
        Countries = base$Countries,
        year = y,
        # Conservative AR(1)-like drift
        gdp_growth = base$gdp_growth * runif(1, 0.9, 1.1),
        inflation = base$inflation * runif(1, 0.95, 1.05),
        reg_quality = base$reg_quality + rnorm(1, 0, 0.02),
        trade_open = base$trade_open * runif(1, 0.98, 1.02),
        fiscal_balance = base$fiscal_balance * runif(1, 0.95, 1.05)
      )
    })
  }) %>%
  ungroup()

# ------------------ COMBINE HISTORICAL + FUTURE ------------------
df_combined <- bind_rows(
  df_hist %>% select(ISO_Code, Countries, year, all_of(predictors)),
  df_future
)

# ------------------ SAVE RESULTS ------------------
write_csv(df_combined, OUTPUT_FILE)
saveRDS(df_combined, file.path(DATA_DIR, "future_covariates.rds"))

message("Future covariates file created: ", OUTPUT_FILE)
message("   Contains ", nrow(df_combined), " rows across ", length(unique(df_combined$ISO_Code)), " countries.")
message("   Future years included: ", paste(unique(df_future$year), collapse = ", "))
