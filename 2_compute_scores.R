# 2_compute_scores.R
# Compute composite EFS using manual or PCA-derived weights
library(dplyr)
library(tidyr)
library(factoextra)  # for PCA interpretation
library(psych)

df <- readRDS("data/df_panel_norm.rds")

# Choose normalized columns
norm_cols <- c("area1_n","area2_n","area3_n","area4_n","area5_n")

# 2a) Manual equal weights
weights_equal <- rep(1/5, 5)
names(weights_equal) <- norm_cols

compute_score_manual <- function(df_in, weights) {
  df_in %>%
    rowwise() %>%
    mutate(EFS_manual = sum(c_across(all_of(names(weights))) * weights)) %>%
    ungroup()
}

df_scores <- compute_score_manual(df, weights_equal)

# 2b) PCA-derived weights (data-driven) - compute per year or global?
# We compute PCA loadings using the latest year (or you can compute globally)
latest_year <- max(df_scores$year, na.rm = TRUE)
df_latest <- df_scores %>% filter(year == latest_year)
X <- df_latest %>% select(all_of(norm_cols)) %>% drop_na()

pca_res <- prcomp(X, center = TRUE, scale. = TRUE)
loadings <- abs(pca_res$rotation[,1])  # absolute first-PC loadings
pca_weights <- loadings / sum(loadings)
names(pca_weights) <- norm_cols
message("PCA weights (based on latest year):")
print(pca_weights)

# Apply PCA weights to all rows
apply_pca_weights <- function(df_in, w) {
  df_in %>%
    rowwise() %>%
    mutate(EFS_pca = sum(c_across(all_of(names(w))) * w)) %>%
    ungroup()
}

df_scores <- apply_pca_weights(df_scores, pca_weights)

# Create rank-by-year for both methods
df_scores <- df_scores %>%
  group_by(year) %>%
  mutate(rank_manual = dense_rank(desc(EFS_manual)),
         rank_pca = dense_rank(desc(EFS_pca))) %>%
  ungroup()

# Save
saveRDS(df_scores, file = "data/df_scores.rds")
write_csv(df_scores %>% filter(year == latest_year) %>%
            arrange(rank_pca),
          file = paste0("outputs/efw_scores_", latest_year, ".csv"))

message("Scores computed and exported for year ", latest_year)
