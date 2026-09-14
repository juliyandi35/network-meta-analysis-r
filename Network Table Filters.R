# Folder input (network tables)
input_folder <- "Network Tables"

# Folder output
output_folder <- file.path(input_folder, "Network Tables Significance Effect Only")

# Buat folder output jika belum ada
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# Ambil semua file Excel
files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)

library(readxl)
library(dplyr)
library(writexl)
library(stringr)

is_significant_ci <- function(x) {
  # Ambil bagian dalam kurung []
  ci <- str_extract(x, "\\[.*\\]")
  
  # Ambil angka lower & upper
  nums <- str_extract_all(ci, "-?\\d+\\.?\\d*")[[1]]
  
  if (length(nums) < 2) return(FALSE)
  
  lower <- as.numeric(nums[1])
  upper <- as.numeric(nums[2])
  
  # Kondisi SIGNIFIKAN
  return((lower < 0 & upper < 0) | (lower > 0 & upper > 0))
}

for (file in files) {
  
  df_common <- read_excel(file, sheet = "common")
  df_random <- read_excel(file, sheet = "random")
  
  # Filter berdasarkan CI dari Network meta-analysis
  df_common_sig <- df_common %>%
    filter(sapply(`Network meta-analysis`, is_significant_ci))
  
  df_random_sig <- df_random %>%
    filter(sapply(`Network meta-analysis`, is_significant_ci))
  
  # Simpan
  write_xlsx(
    list(
      common = df_common_sig,
      random = df_random_sig
    ),
    file.path(output_folder, basename(file))
  )
}
