#!/usr/bin/env Rscript

# Pembersihan data harga rumah Jakarta Selatan.
# Dependensi R: hanya base R.
# LibreOffice/soffice digunakan untuk membaca workbook XLSX tanpa mengubah
# file sumber.

options(stringsAsFactors = FALSE, scipen = 999, digits = 15)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Skrip harus dijalankan dengan Rscript.")
}

script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
project_dir <- dirname(dirname(script_path))
raw_dir <- file.path(project_dir, "data")
output_dir <- file.path(raw_dir, "processed")
audit_dir <- file.path(output_dir, "audit")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

source_data_rumah <- file.path(raw_dir, "DATA RUMAH.xlsx")
source_harga_jaksel <- file.path(raw_dir, "HARGA RUMAH JAKSEL.xlsx")

if (!file.exists(source_data_rumah) || !file.exists(source_harga_jaksel)) {
  stop("Salah satu workbook sumber tidak ditemukan di folder data/.")
}

find_office <- function() {
  candidates <- Sys.which(c("libreoffice", "soffice"))
  candidates <- unname(candidates[nzchar(candidates)])
  if (!length(candidates)) {
    stop("LibreOffice/soffice diperlukan untuk membaca XLSX, tetapi tidak ditemukan.")
  }
  candidates[[1L]]
}

read_xlsx_first_sheet <- function(path) {
  temp_dir <- tempfile("xlsx_csv_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  office <- find_office()
  conversion_log <- system2(
    office,
    c(
      "--headless",
      "--convert-to", "csv",
      "--outdir", shQuote(temp_dir),
      shQuote(normalizePath(path, mustWork = TRUE))
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  csv_name <- paste0(tools::file_path_sans_ext(basename(path)), ".csv")
  csv_path <- file.path(temp_dir, csv_name)
  if (!file.exists(csv_path)) {
    stop(
      "Konversi XLSX gagal untuk ", basename(path), ". Log: ",
      paste(conversion_log, collapse = " | ")
    )
  }

  lines <- readLines(csv_path, warn = FALSE, encoding = "UTF-8")
  has_value <- vapply(
    strsplit(lines, ",", fixed = TRUE),
    function(parts) any(nzchar(trimws(parts))),
    logical(1)
  )
  header_row <- which(has_value)[1L]
  if (is.na(header_row)) {
    stop("Workbook kosong: ", basename(path))
  }

  read.csv(
    csv_path,
    skip = header_row - 1L,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    strip.white = TRUE,
    encoding = "UTF-8"
  )
}

assert_columns <- function(data, expected, dataset_name) {
  actual <- toupper(trimws(names(data)))
  names(data) <- actual
  if (!identical(actual, expected)) {
    stop(
      "Struktur kolom ", dataset_name, " berubah. Diharapkan: ",
      paste(expected, collapse = ", "), "; ditemukan: ",
      paste(actual, collapse = ", ")
    )
  }
  data
}

as_numeric_strict <- function(x, column_name) {
  text <- trimws(as.character(x))
  value <- suppressWarnings(as.numeric(text))
  failed <- !is.na(text) & nzchar(text) & is.na(value)
  if (any(failed)) {
    stop("Nilai nonnumerik ditemukan pada kolom ", column_name, ".")
  }
  value
}

as_integer_strict <- function(x, column_name) {
  value <- as_numeric_strict(x, column_name)
  not_integer <- !is.na(value) & value != floor(value)
  if (any(not_integer)) {
    stop("Nilai bukan bilangan bulat ditemukan pada kolom ", column_name, ".")
  }
  as.integer(value)
}

normalize_text <- function(x) {
  gsub("[[:space:]]+", " ", trimws(as.character(x)))
}

row_signature <- function(data, columns) {
  signature_parts <- lapply(data[columns], function(x) {
    value <- as.character(x)
    value[is.na(value)] <- "<NA>"
    value
  })
  do.call(paste, c(signature_parts, sep = "\r"))
}

flag_iqr_outlier <- function(x) {
  usable <- x[is.finite(x)]
  if (length(usable) < 4L) {
    return(rep(FALSE, length(x)))
  }
  limits <- quantile(usable, c(0.25, 0.75), names = FALSE, type = 7)
  spread <- limits[[2L]] - limits[[1L]]
  lower <- limits[[1L]] - 1.5 * spread
  upper <- limits[[2L]] + 1.5 * spread
  !is.na(x) & (x < lower | x > upper)
}

write_csv <- function(data, path) {
  write.csv(data, path, row.names = FALSE, na = "")
}

add_outlier_flags <- function(data, columns) {
  for (column in columns) {
    flag_name <- paste0("flag_outlier_iqr_", column)
    data[[flag_name]] <- flag_iqr_outlier(data[[column]])
  }
  flag_columns <- paste0("flag_outlier_iqr_", columns)
  data$flag_outlier_iqr <- rowSums(data[flag_columns]) > 0L
  data
}

make_summary <- function(dataset, raw_rows, duplicate_rows, clean_data) {
  outlier_columns <- grep("^flag_outlier_iqr_", names(clean_data), value = TRUE)
  outlier_columns <- setdiff(outlier_columns, "flag_outlier_iqr")
  base <- data.frame(
    dataset = dataset,
    metrik = c(
      "baris_sumber",
      "duplikat_dihapus",
      "baris_bersih",
      "baris_tidak_valid",
      "sel_kosong_data_bersih",
      "baris_dengan_outlier_iqr"
    ),
    nilai = c(
      raw_rows,
      duplicate_rows,
      nrow(clean_data),
      sum(clean_data$flag_data_tidak_valid),
      sum(is.na(clean_data)),
      sum(clean_data$flag_outlier_iqr)
    )
  )
  details <- data.frame(
    dataset = dataset,
    metrik = paste0("jumlah_", outlier_columns),
    nilai = vapply(clean_data[outlier_columns], sum, numeric(1))
  )
  rbind(base, details)
}

# -----------------------------------------------------------------------------
# Dataset 1: DATA RUMAH.xlsx
# -----------------------------------------------------------------------------

raw_data_rumah <- read_xlsx_first_sheet(source_data_rumah)
raw_data_rumah <- assert_columns(
  raw_data_rumah,
  c("NO", "NAMA RUMAH", "HARGA", "LB", "LT", "KT", "KM", "GRS"),
  "DATA RUMAH.xlsx"
)

data_rumah <- data.frame(
  baris_sumber_excel = seq_len(nrow(raw_data_rumah)) + 1L,
  nomor_sumber = as_integer_strict(raw_data_rumah$NO, "NO"),
  nama_rumah = normalize_text(raw_data_rumah$`NAMA RUMAH`),
  harga = as_numeric_strict(raw_data_rumah$HARGA, "HARGA"),
  luas_bangunan = as_numeric_strict(raw_data_rumah$LB, "LB"),
  luas_tanah = as_numeric_strict(raw_data_rumah$LT, "LT"),
  kamar_tidur = as_integer_strict(raw_data_rumah$KT, "KT"),
  kamar_mandi = as_integer_strict(raw_data_rumah$KM, "KM"),
  jumlah_garasi = as_integer_strict(raw_data_rumah$GRS, "GRS")
)

rumah_key <- c(
  "nama_rumah", "harga", "luas_bangunan", "luas_tanah",
  "kamar_tidur", "kamar_mandi", "jumlah_garasi"
)
rumah_signature <- row_signature(data_rumah, rumah_key)
rumah_duplicate <- duplicated(rumah_signature)
duplikat_data_rumah <- data_rumah[rumah_duplicate, , drop = FALSE]
data_rumah <- data_rumah[!rumah_duplicate, , drop = FALSE]

data_rumah$garasi <- factor(
  ifelse(
    is.na(data_rumah$jumlah_garasi),
    NA_character_,
    ifelse(data_rumah$jumlah_garasi > 0L, "ada", "tidak_ada")
  ),
  levels = c("tidak_ada", "ada")
)
data_rumah$harga_per_m2_tanah <- ifelse(
  data_rumah$luas_tanah > 0,
  round(data_rumah$harga / data_rumah$luas_tanah, 2),
  NA_real_
)
data_rumah$harga_per_m2_bangunan <- ifelse(
  data_rumah$luas_bangunan > 0,
  round(data_rumah$harga / data_rumah$luas_bangunan, 2),
  NA_real_
)
data_rumah$flag_data_tidak_valid <- with(
  data_rumah,
  is.na(nomor_sumber) |
    is.na(nama_rumah) | !nzchar(nama_rumah) |
    is.na(harga) | !is.finite(harga) | harga <= 0 |
    is.na(luas_tanah) | !is.finite(luas_tanah) | luas_tanah <= 0 |
    is.na(luas_bangunan) | !is.finite(luas_bangunan) | luas_bangunan <= 0 |
    is.na(kamar_tidur) | kamar_tidur < 0 |
    is.na(kamar_mandi) | kamar_mandi < 0 |
    is.na(jumlah_garasi) | jumlah_garasi < 0
)
data_rumah <- add_outlier_flags(
  data_rumah,
  c(
    "harga", "luas_tanah", "luas_bangunan", "kamar_tidur",
    "kamar_mandi", "jumlah_garasi"
  )
)
data_rumah$id_data <- sprintf("RMT%04d", seq_len(nrow(data_rumah)))
data_rumah$baris_sumber_excel <- NULL
data_rumah <- data_rumah[c(
  "id_data", "nomor_sumber", "nama_rumah", "harga", "luas_tanah",
  "luas_bangunan", "kamar_tidur", "kamar_mandi", "jumlah_garasi", "garasi",
  "harga_per_m2_tanah", "harga_per_m2_bangunan", "flag_data_tidak_valid",
  "flag_outlier_iqr_harga", "flag_outlier_iqr_luas_tanah",
  "flag_outlier_iqr_luas_bangunan", "flag_outlier_iqr_kamar_tidur",
  "flag_outlier_iqr_kamar_mandi", "flag_outlier_iqr_jumlah_garasi",
  "flag_outlier_iqr"
)]

# -----------------------------------------------------------------------------
# Dataset 2: HARGA RUMAH JAKSEL.xlsx
# -----------------------------------------------------------------------------

raw_harga_jaksel <- read_xlsx_first_sheet(source_harga_jaksel)
raw_harga_jaksel <- assert_columns(
  raw_harga_jaksel,
  c("HARGA", "LT", "LB", "JKT", "JKM", "GRS", "KOTA"),
  "HARGA RUMAH JAKSEL.xlsx"
)

garage_raw <- toupper(normalize_text(raw_harga_jaksel$GRS))
garage_value <- ifelse(
  garage_raw == "ADA",
  "ada",
  ifelse(garage_raw == "TIDAK ADA", "tidak_ada", NA_character_)
)
kota_raw <- toupper(normalize_text(raw_harga_jaksel$KOTA))
kota_value <- ifelse(
  kota_raw == "JAKSEL",
  "Jakarta Selatan",
  normalize_text(raw_harga_jaksel$KOTA)
)

harga_jaksel <- data.frame(
  baris_sumber_excel = seq_len(nrow(raw_harga_jaksel)) + 2L,
  harga = as_numeric_strict(raw_harga_jaksel$HARGA, "HARGA"),
  luas_tanah = as_numeric_strict(raw_harga_jaksel$LT, "LT"),
  luas_bangunan = as_numeric_strict(raw_harga_jaksel$LB, "LB"),
  kamar_tidur = as_integer_strict(raw_harga_jaksel$JKT, "JKT"),
  kamar_mandi = as_integer_strict(raw_harga_jaksel$JKM, "JKM"),
  garasi = factor(garage_value, levels = c("tidak_ada", "ada")),
  kota = kota_value
)

jaksel_key <- c(
  "harga", "luas_tanah", "luas_bangunan", "kamar_tidur",
  "kamar_mandi", "garasi", "kota"
)
jaksel_signature <- row_signature(harga_jaksel, jaksel_key)
jaksel_duplicate <- duplicated(jaksel_signature)
duplikat_harga_jaksel <- harga_jaksel[jaksel_duplicate, , drop = FALSE]
harga_jaksel <- harga_jaksel[!jaksel_duplicate, , drop = FALSE]

harga_jaksel$harga_per_m2_tanah <- ifelse(
  harga_jaksel$luas_tanah > 0,
  round(harga_jaksel$harga / harga_jaksel$luas_tanah, 2),
  NA_real_
)
harga_jaksel$harga_per_m2_bangunan <- ifelse(
  harga_jaksel$luas_bangunan > 0,
  round(harga_jaksel$harga / harga_jaksel$luas_bangunan, 2),
  NA_real_
)
harga_jaksel$flag_data_tidak_valid <- with(
  harga_jaksel,
  is.na(harga) | !is.finite(harga) | harga <= 0 |
    is.na(luas_tanah) | !is.finite(luas_tanah) | luas_tanah <= 0 |
    is.na(luas_bangunan) | !is.finite(luas_bangunan) | luas_bangunan <= 0 |
    is.na(kamar_tidur) | kamar_tidur < 0 |
    is.na(kamar_mandi) | kamar_mandi < 0 |
    is.na(garasi) |
    is.na(kota) | !nzchar(kota)
)
harga_jaksel <- add_outlier_flags(
  harga_jaksel,
  c("harga", "luas_tanah", "luas_bangunan", "kamar_tidur", "kamar_mandi")
)
harga_jaksel$id_data <- sprintf("JKS%04d", seq_len(nrow(harga_jaksel)))
harga_jaksel$baris_sumber_excel <- NULL
harga_jaksel <- harga_jaksel[c(
  "id_data", "harga", "luas_tanah", "luas_bangunan", "kamar_tidur",
  "kamar_mandi", "garasi", "kota", "harga_per_m2_tanah",
  "harga_per_m2_bangunan", "flag_data_tidak_valid",
  "flag_outlier_iqr_harga", "flag_outlier_iqr_luas_tanah",
  "flag_outlier_iqr_luas_bangunan", "flag_outlier_iqr_kamar_tidur",
  "flag_outlier_iqr_kamar_mandi", "flag_outlier_iqr"
)]

# Pemeriksaan hasil. Nilai ekstrem hanya diberi flag dan tidak dihapus.
stopifnot(
  !anyDuplicated(data_rumah[rumah_key]),
  !anyDuplicated(harga_jaksel[jaksel_key]),
  all(abs(data_rumah$harga_per_m2_tanah - data_rumah$harga / data_rumah$luas_tanah) < 0.011),
  all(abs(harga_jaksel$harga_per_m2_tanah - harga_jaksel$harga / harga_jaksel$luas_tanah) < 0.011)
)

write_csv(data_rumah, file.path(output_dir, "data_rumah_bersih.csv"))
write_csv(harga_jaksel, file.path(output_dir, "harga_rumah_jaksel_bersih.csv"))
saveRDS(data_rumah, file.path(output_dir, "data_rumah_bersih.rds"))
saveRDS(harga_jaksel, file.path(output_dir, "harga_rumah_jaksel_bersih.rds"))

write_csv(
  duplikat_data_rumah,
  file.path(audit_dir, "duplikat_dihapus_data_rumah.csv")
)
write_csv(
  duplikat_harga_jaksel,
  file.path(audit_dir, "duplikat_dihapus_harga_jaksel.csv")
)

cleaning_summary <- rbind(
  make_summary(
    "data_rumah",
    nrow(raw_data_rumah),
    sum(rumah_duplicate),
    data_rumah
  ),
  make_summary(
    "harga_rumah_jaksel",
    nrow(raw_harga_jaksel),
    sum(jaksel_duplicate),
    harga_jaksel
  )
)
write_csv(
  cleaning_summary,
  file.path(audit_dir, "ringkasan_pembersihan.csv")
)

message("Pembersihan selesai.")
message("- data_rumah: ", nrow(raw_data_rumah), " -> ", nrow(data_rumah), " baris")
message("- harga_rumah_jaksel: ", nrow(raw_harga_jaksel), " -> ", nrow(harga_jaksel), " baris")

