# 5_visualize.R
# Produce core visualizations: top N countries, EFS with CI, and country time series

library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# ---------------- Load latest bootstrapped EFS data ----------------
latest_year <- max(readr::parse_number(list.files("outputs", pattern = "efw_scores_.*csv")), na.rm = TRUE)
df_ci <- readr::read_csv(list.files("outputs", pattern = "efw_scores_bootstrap_.*csv", full.names = TRUE)[1])

# ---------------- Top 10 Countries by Mean EFS ----------------
top10 <- df_ci %>% arrange(desc(EFS_pca_mean)) %>% head(10)

p1 <- ggplot(top10, aes(x = reorder(country, EFS_pca_mean), y = EFS_pca_mean)) +
  geom_col(fill = "#3182bd") +
  geom_errorbar(aes(ymin = EFS_pca_lo, ymax = EFS_pca_hi), width = 0.3) +
  coord_flip() +
  labs(title = paste0("Top 10 Countries by Economic Freedom (", latest_year, ")"),
       x = "", y = "EFS (Bootstrap Mean ± 95% CI)") +
  theme_minimal(base_size = 13)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/top10_efs_ci.png", p1, width = 8, height = 6, dpi = 150)

# ---------------- Example: Time Series for a Single Country (USA) ----------------
df_scores_all <- readRDS("data/df_scores.rds")

country_ts <- df_scores_all %>%
  filter(iso == "USA") %>%
  arrange(year)

p2 <- ggplot(country_ts, aes(x = year, y = EFS_pca)) +
  geom_line(color = "#08519c", linewidth = 1) +
  geom_point(color = "#08519c") +
  labs(title = "EFS (PCA-weighted) Over Time — USA",
       x = "Year", y = "Economic Freedom Score (EFS)") +
  theme_minimal(base_size = 13)
ggsave("figures/usa_time_series.png", p2, width = 8, height = 4, dpi = 150)

# ---------------- Bayesian Forecast Visualization ----------------
# This section produces country-level forecast plots with posterior uncertainty bands
# (requires df_model and df_out from your Bayesian model run)

if (exists("df_model") && exists("df_out")) {

  FIG_DIR <- file.path("figures", "bayes")
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  set.seed(2025)

  sample_isos <- sample(unique(df_model$iso), size = min(6, length(unique(df_model$iso))))

  for (iso_code in sample_isos) {
    sub_hist <- df_model %>% filter(iso == iso_code)

    # Safety check
    if (nrow(sub_hist) == 0) {
      message("Skipping ", iso_code, " — no data available.")
      next
    }

    idxs <- which(df_model$iso == iso_code)
    sub_preds <- df_out[idxs, ]

    if (nrow(sub_preds) == 0) {
      message("Skipping ", iso_code, " — no predictions found.")
      next
    }

    png(file.path(FIG_DIR, paste0(iso_code, "_bayes_pred.png")), width = 900, height = 500)
    plot(
      sub_hist$year, sub_hist$EFS,
      type = "b", pch = 19, col = "black",
      ylab = "Economic Freedom Score (EFS)",
      xlab = "Year",
      main = paste0(sub_hist$country[1], " (", iso_code, ") — Observed vs Predicted")
    )

    # Add posterior predictive mean + CI
    points(sub_hist$year, sub_preds$EFS_pred_mean, col = "blue", pch = 18)
    arrows(
      sub_hist$year, sub_preds$EFS_pred_lo,
      sub_hist$year, sub_preds$EFS_pred_hi,
      length = 0.05, angle = 90, code = 3, col = "blue"
    )

    legend("topleft",
           legend = c("Observed", "Predicted (95% CI)"),
           col = c("black", "blue"),
           pch = c(19, 18), bty = "n")
    dev.off()
  }

  message("Bayesian forecast plots saved to ", FIG_DIR)

} else {
  message("Skipping Bayesian forecast visualizations — df_model or df_out not found.")
}

message("All visualizations saved to figures/")
