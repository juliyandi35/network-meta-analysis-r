library(pdftools)

# Folder input (isi PDF)
input_folder <- "Figures"

# Folder output utama
dir.create("Converted Figures", showWarnings = FALSE)
output_folder <- "Converted Figures"

# Ambil semua file PDF
pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)

for (pdf in pdf_files) {
  
  # Nama file tanpa ekstensi
  file_name <- tools::file_path_sans_ext(basename(pdf))
  
  # Buat folder khusus untuk tiap PDF
  out_dir <- file.path(output_folder, file_name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Convert semua halaman
  output <- pdf_convert(
    pdf = pdf,
    format = "png",
    dpi = 600
  )
  
  # Rename + pindahkan ke folder masing-masing
  new_names <- file.path(out_dir, sprintf("%s_%03d.png", file_name, seq_along(output)))
  file.rename(output, new_names)
}
