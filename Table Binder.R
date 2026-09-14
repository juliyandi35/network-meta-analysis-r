library(readxl)
library(dplyr)

# Folder tempat file Excel
folder_path <- "Significance Tables"

# Ambil semua file Excel (.xlsx)
files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Baca dan gabungkan semua file
data_gabungan <- files %>%
  lapply(read_excel) %>%   # baca tiap file
  bind_rows()              # gabungkan jadi satu tabel

library(writexl)

write_xlsx(data_gabungan, "Significance Tables/Significances Table.xlsx")
