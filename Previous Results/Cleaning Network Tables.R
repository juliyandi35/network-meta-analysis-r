library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Folder input dan output
input_dir  <- "Network Tables"
output_dir <- "Network Tables Cleaned"

dir.create(output_dir, showWarnings = FALSE)

# Ambil semua file Excel
files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Fungsi untuk membersihkan satu sheet
clean_sheet <- function(df) {
  df %>%
    mutate(
      nums = str_extract_all(`Network meta-analysis`, "-?\\d+\\.?\\d*")
    ) %>%
    mutate(
      lower = as.numeric(sapply(nums, function(x) if (length(x) >= 2) x[2] else NA)),
      upper = as.numeric(sapply(nums, function(x) if (length(x) >= 3) x[3] else NA))
    ) %>%
    filter(is.na(lower) | !(lower < 0 & upper > 0)) %>%
    select(-nums, -lower, -upper)
}


# Loop semua file
for (f in files) {
  message("Processing: ", basename(f))
  
  common_df <- read_excel(f, sheet = "common")
  random_df <- read_excel(f, sheet = "random")
  
  common_clean <- clean_sheet(common_df)
  random_clean <- clean_sheet(random_df)
  
  output_file <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(f)), "_cleaned.xlsx")
  )
  
  write_xlsx(
    list(
      common = common_clean,
      random = random_clean
    ),
    output_file
  )
}
