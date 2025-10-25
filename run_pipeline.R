# run_pipeline.R
# Author: Henry Risley
# Date: Oct 2025
# Purpose: Automate computation of Economic Freedom Scores (EFS) from the Fraser EFW panel dataset

# --- Load Dependencies ---
library(tidyverse)
library(readxl)
library(changepoint)
library(boot)
library(factoextra)

source("0_load_data.R")
source("1_preprocess.R")
source("2_compute_scores.R")
source("3_bootstrap_ci.R")
source("4_changepoint_detect.R")
source("5_visualize.R")

# --- Step 1: Load raw data ---
raw <- load_panel_data("data/efotw-2025-master-index-data-for-researchers-iso.xlsx")

# --- Step 2: Normalize + prepare areas ---
clean <- preprocess_panel(raw)

# --- Step 3: Compute Economic Freedom Scores ---
efs <- compute_scores(clean)

# --- Step 4: Bootstrap confidence intervals ---
efs_ci <- bootstrap_ci(efs)

# --- Step 5: Detect changepoints in time series per country ---
change_summary <- detect_changepoints(efs)

# --- Step 6: Save outputs and visualize ---
write_csv(efs, "outputs/efw_scores_panel.csv")
write_csv(change_summary, "outputs/efw_summary_2023.csv")

plot_top10(efs)
