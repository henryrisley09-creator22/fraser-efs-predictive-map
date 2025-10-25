# 5_visualize.R
# Produce core visualizations: top N countries, EFS with CI, and country time series
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

latest_year <- max(readr::parse_number(list.files("outputs", pattern = "efw_scores_.*csv")) , na.rm = TRUE)

df_ci <- readr::read_csv(list.files("outputs", pattern = "efw_scores_bootstrap_.*csv", full.names = TRUE)[1])

# Top 10 by mean EFS
top10 <- df_ci %>% arrange(desc(EFS_pca_mean)) %>% head(10)

p1 <- ggplot(top10, aes(x = reorder(country, EFS_pca_mean), y = EFS_pca_mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = EFS_pca_lo, ymax = EFS_pca_hi), width = 0.3) +
  coord_flip() +
  labs(title = paste0("Top 10 Countries by Economic Freedom (", latest_year, ")"),
       x = "", y = "EFS (bootstrap mean with 95% CI)") +
  theme_minimal()

dir.create("figures", showWarnings = FALSE)
ggsave("figures/top10_efs_ci.png", p1, width = 8, height = 6, dpi = 150)

# Example: time series for a single country (e.g., USA)
df_scores_all <- readRDS("data/df_scores.rds")

country_ts <- df_scores_all %>% filter(iso == "USA") %>% arrange(year)

p2 <- ggplot(country_ts, aes(x = year, y = EFS_pca)) +
  geom_line() + geom_point() +
  labs(title = "EFS (PCA-weighted) over time — USA", x = "Year", y = "EFS") +
  theme_minimal()
ggsave("figures/usa_time_series.png", p2, width = 8, height = 4, dpi = 150)

message("Visualizations saved to figures/")
