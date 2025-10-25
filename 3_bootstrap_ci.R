# 3_bootstrap_ci.R
# Compute bootstrap confidence intervals for the EFS_pca scores
library(dplyr)
library(boot)

df_scores <- readRDS("data/df_scores.rds")
norm_cols <- c("area1_n","area2_n","area3_n","area4_n","area5_n")

# For computational speed, do bootstrap on the latest year subset
latest_year <- max(df_scores$year, na.rm = TRUE)
df_latest <- df_scores %>% filter(year == latest_year) %>% drop_na(all_of(norm_cols))

# Bootstrapping by resampling *indicators* (columns) with replacement
bootstrap_efs <- function(data_matrix, weights, R=1000) {
  # data_matrix: n x p (countries x indicators)
  n <- nrow(data_matrix)
  p <- ncol(data_matrix)
  boot_stat <- function(dat, indices) {
    # indices here are column indices (we will pass col indices)
    sampled_cols <- indices
    sampled_data <- dat[, sampled_cols, drop = FALSE]
    # weights for sampled columns correspond to original weights for those indices
    w <- weights[sampled_cols]
    w_norm <- w / sum(w)
    as.numeric(sampled_data %*% w_norm)
  }
  # We'll do manual bootstrap: for b in 1:R sample p columns w/ replacement and compute
  B <- R
  result_mat <- matrix(NA, nrow = n, ncol = B)
  set.seed(2025)
  for (b in seq_len(B)) {
    sampled_cols <- sample(seq_len(p), size = p, replace = TRUE)
    result_mat[, b] <- boot_stat(data_matrix, sampled_cols)
  }
  list(mean = rowMeans(result_mat),
       lo = apply(result_mat, 1, function(x) quantile(x, 0.025)),
       hi = apply(result_mat, 1, function(x) quantile(x, 0.975)))
}

# Prepare data matrix and weights (based on PCA weights saved earlier)
# Note: we will recompute pca_weights here to ensure independence
X <- df_latest %>% select(all_of(norm_cols)) %>% as.matrix()
pca_res <- prcomp(X, center = TRUE, scale. = TRUE)
pca_weights <- abs(pca_res$rotation[,1])
pca_weights <- pca_weights / sum(pca_weights)

bs <- bootstrap_efs(as.matrix(df_latest %>% select(all_of(norm_cols))), pca_weights, R = 500)

# Attach to df_latest
df_ci <- df_latest %>%
  mutate(EFS_pca_mean = bs$mean,
         EFS_pca_lo = bs$lo,
         EFS_pca_hi = bs$hi) %>%
  arrange(desc(EFS_pca_mean))

dir.create("outputs", showWarnings = FALSE)
write_csv(df_ci, file = paste0("outputs/efw_scores_bootstrap_", latest_year, ".csv"))
saveRDS(df_ci, file = paste0("data/df_ci_", latest_year, ".rds"))

message("Bootstrap CI computed and exported for year ", latest_year)
