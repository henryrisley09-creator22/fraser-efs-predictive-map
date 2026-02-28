library(readxl)
library(dplyr)

file_path <- "~/Downloads/efotw-2025-master-index-data-for-researchers-iso.xlsx" 
sheet_name <- "EFW Panel Dataset"

if (!file.exists(file_path)) {
  stop("The file was not found at: ", file_path)
}

df_panel <- read_excel(file_path, sheet = sheet_name)

message("Rows: ", nrow(df_panel), " Columns: ", ncol(df_panel))
glimpse(df_panel)

if (!dir.exists("data")) dir.create("data")
saveRDS(df_panel, file = "data/df_panel_raw.rds")

