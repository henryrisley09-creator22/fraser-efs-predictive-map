# 0_load_data.R
# Load the panel dataset from the provided Excel file
library(readxl)
library(dplyr)

file_path <- "efotw-2025-master-index-data-for-researchers-iso.xlsx"
sheet_name <- "EFW Panel Dataset"

df_panel <- read_excel(file_path, sheet = sheet_name)

# Quick sanity
message("Rows: ", nrow(df_panel), " Columns: ", ncol(df_panel))
glimpse(df_panel)

# Save to RDS for faster reloads
saveRDS(df_panel, file = "data/df_panel_raw.rds")
