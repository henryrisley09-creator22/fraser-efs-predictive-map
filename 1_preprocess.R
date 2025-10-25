# 1_preprocess.R
# Preprocess and normalize area columns (min-max)
library(dplyr)
library(tidyr)
library(readr)

df_panel <- readRDS("data/df_panel_raw.rds")

# Standardize column names (no changes to original structure)
# Key columns expected: ISO_Code, Countries, Year, Summary, Area 1 ... Area 5
# If column names include spaces, we access them with backticks: `Area 1`

# Select relevant columns and rename for convenience
df <- df_panel %>%
  rename(
    iso = ISO_Code,
    country = Countries,
    year = Year,
    summary = Summary,
    area1 = `Area 1`,
    area2 = `Area 2`,
    area3 = `Area 3`,
    area4 = `Area 4`,
    area5 = `Area 5`
  ) %>%
  select(iso, country, year, summary, area1, area2, area3, area4, area5, everything())

# Convert to numeric and handle NAs
df <- df %>%
  mutate(across(c(area1:area5, summary), ~ as.numeric(.)))

# Function: min-max normalize by year or cross-section
minmax_norm <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# Normalize the five areas across countries *for each year* so years are comparable
df_norm <- df %>%
  group_by(year) %>%
  mutate(
    area1_n = minmax_norm(area1),
    area2_n = minmax_norm(area2),
    area3_n = minmax_norm(area3),
    area4_n = minmax_norm(area4),
    area5_n = minmax_norm(area5)
  ) %>%
  ungroup()

# Save processed data
dir.create("data", showWarnings = FALSE)
saveRDS(df_norm, file = "data/df_panel_norm.rds")
message("Preprocessing complete. Saved to data/df_panel_norm.rds")
