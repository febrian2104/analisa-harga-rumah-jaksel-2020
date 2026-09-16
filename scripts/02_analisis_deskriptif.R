#!/usr/bin/env Rscript

# Analisis deskriptif data harga rumah Jakarta Selatan.
# Tahap ini tidak melakukan pemodelan, pengujian hipotesis, atau penghapusan
# pencilan. Dependensi R: hanya base R.

options(stringsAsFactors = FALSE, scipen = 999, digits = 15)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Skrip harus dijalankan dengan Rscript.")
}

script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
project_dir <- dirname(dirname(script_path))
input_dir <- file.path(project_dir, "data", "processed")
report_dir <- file.path(project_dir, "reports", "deskriptif")
table_dir <- file.path(report_dir, "tabel")
plot_dir <- file.path(report_dir, "plots")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

input_data_rumah <- file.path(input_dir, "data_rumah_bersih.rds")
input_harga_jaksel <- file.path(input_dir, "harga_rumah_jaksel_bersih.rds")
if (!file.exists(input_data_rumah) || !file.exists(input_harga_jaksel)) {
  stop("Data bersih belum tersedia. Jalankan scripts/01_clean_data.R terlebih dahulu.")
}

data_rumah <- readRDS(input_data_rumah)
harga_jaksel <- readRDS(input_harga_jaksel)

write_csv <- function(data, path) {
  write.csv(data, path, row.names = FALSE, na = "")
}

pretty_variable <- function(x) {
  labels <- c(
    harga = "Harga",
    luas_tanah = "Luas tanah",
    luas_bangunan = "Luas bangunan",
    kamar_tidur = "Kamar tidur",
    kamar_mandi = "Kamar mandi",
    jumlah_garasi = "Jumlah garasi",
    harga_per_m2_tanah = "Harga per m² tanah",
    harga_per_m2_bangunan = "Harga per m² bangunan"
  )
  result <- unname(labels[x])
  result[is.na(result)] <- x[is.na(result)]
  result
}

numeric_summary <- function(data, dataset, columns) {
  result <- lapply(columns, function(column) {
    x <- data[[column]]
    usable <- x[is.finite(x)]
    quantiles <- quantile(
      usable,
      probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
      names = FALSE,
      type = 7
    )
    data.frame(
      dataset = dataset,
      variabel = column,
      n = length(usable),
      missing = sum(is.na(x) | !is.finite(x)),
      minimum = min(usable),
      p05 = quantiles[[1L]],
      q1 = quantiles[[2L]],
      median = quantiles[[3L]],
      mean = mean(usable),
      q3 = quantiles[[4L]],
      p95 = quantiles[[5L]],
      maximum = max(usable),
      sd = sd(usable)
    )
  })
  do.call(rbind, result)
}

categorical_summary <- function(data, dataset, columns) {
  result <- lapply(columns, function(column) {
    values <- as.character(data[[column]])
    values[is.na(values)] <- "(missing)"
    counts <- table(values, useNA = "no")
    data.frame(
      dataset = dataset,
      variabel = column,
      kategori = names(counts),
      jumlah = as.integer(counts),
      persentase = round(as.integer(counts) / length(values) * 100, 2)
    )
  })
  do.call(rbind, result)
}

garage_summary <- function(data, dataset) {
  groups <- split(data, data$garasi, drop = TRUE)
  result <- lapply(names(groups), function(group_name) {
    group <- groups[[group_name]]
    data.frame(
      dataset = dataset,
      garasi = group_name,
      jumlah = nrow(group),
      persentase = round(nrow(group) / nrow(data) * 100, 2),
      median_harga = median(group$harga),
      mean_harga = mean(group$harga),
      q1_harga = unname(quantile(group$harga, 0.25)),
      q3_harga = unname(quantile(group$harga, 0.75)),
      median_luas_tanah = median(group$luas_tanah),
      median_luas_bangunan = median(group$luas_bangunan),
      median_harga_per_m2_tanah = median(group$harga_per_m2_tanah),
      median_harga_per_m2_bangunan = median(group$harga_per_m2_bangunan)
    )
  })
  do.call(rbind, result)
}

correlation_long <- function(data, dataset, columns, method) {
  matrix_value <- cor(data[columns], use = "pairwise.complete.obs", method = method)
  pairs <- which(upper.tri(matrix_value), arr.ind = TRUE)
  data.frame(
    dataset = dataset,
    metode = method,
    variabel_1 = rownames(matrix_value)[pairs[, "row"]],
    variabel_2 = colnames(matrix_value)[pairs[, "col"]],
    korelasi = matrix_value[pairs]
  )
}

format_number_id <- function(x, digits = 1L) {
  format(
    round(x, digits),
    big.mark = ".",
    decimal.mark = ",",
    nsmall = digits,
    trim = TRUE,
    scientific = FALSE
  )
}

format_rupiah <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  if (abs(x) >= 1e9) {
    return(paste0("Rp", format_number_id(x / 1e9, 2L), " miliar"))
  }
  if (abs(x) >= 1e6) {
    return(paste0("Rp", format_number_id(x / 1e6, 2L), " juta"))
  }
  paste0("Rp", format_number_id(x, 0L))
}

format_percent <- function(x) {
  paste0(format_number_id(x, 1L), "%")
}

plot_price_distribution <- function(data, dataset_title, path) {
  png(path, width = 1800, height = 1300, res = 170)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(2, 2), mar = c(4.5, 4.7, 3.2, 1.2), las = 1)

  price_billion <- data$harga / 1e9
  hist(
    price_billion,
    breaks = "FD",
    col = "#4C78A8",
    border = "white",
    main = "Distribusi harga",
    xlab = "Harga (Rp miliar)",
    ylab = "Frekuensi"
  )
  abline(v = median(price_billion), col = "#E45756", lwd = 2)
  abline(v = mean(price_billion), col = "#F2CF5B", lwd = 2, lty = 2)
  legend(
    "topright",
    legend = c("Median", "Mean"),
    col = c("#E45756", "#B88700"),
    lty = c(1, 2),
    lwd = 2,
    bty = "n"
  )

  hist(
    log10(data$harga),
    breaks = "FD",
    col = "#72B7B2",
    border = "white",
    main = "Distribusi log10(harga)",
    xlab = "log10(harga dalam rupiah)",
    ylab = "Frekuensi"
  )

  point_color <- grDevices::adjustcolor("#4C78A8", alpha.f = 0.35)
  plot(
    data$luas_tanah,
    price_billion,
    log = "xy",
    pch = 16,
    cex = 0.65,
    col = point_color,
    main = "Harga dan luas tanah",
    xlab = "Luas tanah (m², skala log)",
    ylab = "Harga (Rp miliar, skala log)"
  )
  plot(
    data$luas_bangunan,
    price_billion,
    log = "xy",
    pch = 16,
    cex = 0.65,
    col = point_color,
    main = "Harga dan luas bangunan",
    xlab = "Luas bangunan (m², skala log)",
    ylab = "Harga (Rp miliar, skala log)"
  )
  mtext(dataset_title, outer = TRUE, line = -1.5, cex = 1.15, font = 2)
}

plot_garage_comparison <- function(data, dataset_title, path) {
  png(path, width = 1800, height = 650, res = 170)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(1, 3), mar = c(5, 4.7, 3.2, 1.2), las = 1)

  group_label <- factor(
    data$garasi,
    levels = c("tidak_ada", "ada"),
    labels = c("Tidak ada", "Ada")
  )
  boxplot(
    data$harga / 1e9 ~ group_label,
    log = "y",
    col = c("#E0E0E0", "#72B7B2"),
    border = "#555555",
    main = "Harga menurut garasi",
    xlab = "Garasi",
    ylab = "Harga (Rp miliar, skala log)",
    outline = TRUE
  )
  boxplot(
    data$harga_per_m2_tanah / 1e6 ~ group_label,
    log = "y",
    col = c("#E0E0E0", "#4C78A8"),
    border = "#555555",
    main = "Harga per m² tanah",
    xlab = "Garasi",
    ylab = "Rp juta/m² (skala log)",
    outline = TRUE
  )
  garage_counts <- table(group_label)
  barplot(
    garage_counts,
    col = c("#E0E0E0", "#F58518"),
    border = NA,
    main = "Jumlah listing",
    xlab = "Garasi",
    ylab = "Jumlah"
  )
  mtext(dataset_title, outer = TRUE, line = -1.5, cex = 1.15, font = 2)
}

plot_correlation <- function(data, columns, dataset_title, path) {
  matrix_value <- cor(data[columns], use = "pairwise.complete.obs", method = "spearman")
  labels <- pretty_variable(colnames(matrix_value))
  n <- ncol(matrix_value)
  palette <- colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))(201)

  png(path, width = 1300, height = 1200, res = 170)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mar = c(8, 9, 4, 2), xpd = NA)
  image(
    seq_len(n),
    seq_len(n),
    matrix_value,
    zlim = c(-1, 1),
    col = palette,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = paste0(dataset_title, "\nKorelasi Spearman")
  )
  axis(1, at = seq_len(n), labels = labels, las = 2, tick = FALSE)
  axis(2, at = seq_len(n), labels = labels, las = 2, tick = FALSE)
  box()
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      text(i, j, sprintf("%.2f", matrix_value[i, j]), cex = 0.85)
    }
  }
}

numeric_columns_jaksel <- c(
  "harga", "luas_tanah", "luas_bangunan", "kamar_tidur", "kamar_mandi",
  "harga_per_m2_tanah", "harga_per_m2_bangunan"
)
numeric_columns_rumah <- c(
  "harga", "luas_tanah", "luas_bangunan", "kamar_tidur", "kamar_mandi",
  "jumlah_garasi", "harga_per_m2_tanah", "harga_per_m2_bangunan"
)
correlation_columns_jaksel <- c(
  "harga", "luas_tanah", "luas_bangunan", "kamar_tidur", "kamar_mandi"
)
correlation_columns_rumah <- c(
  "harga", "luas_tanah", "luas_bangunan", "kamar_tidur", "kamar_mandi",
  "jumlah_garasi"
)

summary_numeric <- rbind(
  numeric_summary(harga_jaksel, "harga_rumah_jaksel", numeric_columns_jaksel),
  numeric_summary(data_rumah, "data_rumah", numeric_columns_rumah)
)
summary_categorical <- rbind(
  categorical_summary(harga_jaksel, "harga_rumah_jaksel", c("garasi", "kota")),
  categorical_summary(data_rumah, "data_rumah", c("garasi", "jumlah_garasi"))
)
summary_garage <- rbind(
  garage_summary(harga_jaksel, "harga_rumah_jaksel"),
  garage_summary(data_rumah, "data_rumah")
)
correlation_pearson <- rbind(
  correlation_long(
    harga_jaksel,
    "harga_rumah_jaksel",
    correlation_columns_jaksel,
    "pearson"
  ),
  correlation_long(data_rumah, "data_rumah", correlation_columns_rumah, "pearson")
)
correlation_spearman <- rbind(
  correlation_long(
    harga_jaksel,
    "harga_rumah_jaksel",
    correlation_columns_jaksel,
    "spearman"
  ),
  correlation_long(data_rumah, "data_rumah", correlation_columns_rumah, "spearman")
)

write_csv(summary_numeric, file.path(table_dir, "ringkasan_numerik.csv"))
write_csv(summary_categorical, file.path(table_dir, "distribusi_kategorik.csv"))
write_csv(summary_garage, file.path(table_dir, "ringkasan_menurut_garasi.csv"))
write_csv(correlation_pearson, file.path(table_dir, "korelasi_pearson.csv"))
write_csv(correlation_spearman, file.path(table_dir, "korelasi_spearman.csv"))

plot_price_distribution(
  harga_jaksel,
  "Data Utama Harga Rumah Jakarta Selatan",
  file.path(plot_dir, "harga_jaksel_distribusi_dan_luas.png")
)
plot_garage_comparison(
  harga_jaksel,
  "Data Utama Harga Rumah Jakarta Selatan",
  file.path(plot_dir, "harga_jaksel_perbandingan_garasi.png")
)
plot_correlation(
  harga_jaksel,
  correlation_columns_jaksel,
  "Data Utama Harga Rumah Jakarta Selatan",
  file.path(plot_dir, "harga_jaksel_korelasi_spearman.png")
)

plot_price_distribution(
  data_rumah,
  "Data Listing Rumah",
  file.path(plot_dir, "data_rumah_distribusi_dan_luas.png")
)
plot_garage_comparison(
  data_rumah,
  "Data Listing Rumah",
  file.path(plot_dir, "data_rumah_perbandingan_garasi.png")
)
plot_correlation(
  data_rumah,
  correlation_columns_rumah,
  "Data Listing Rumah",
  file.path(plot_dir, "data_rumah_korelasi_spearman.png")
)

get_numeric_stat <- function(dataset, variable, statistic) {
  row <- summary_numeric[
    summary_numeric$dataset == dataset & summary_numeric$variabel == variable,
    ,
    drop = FALSE
  ]
  row[[statistic]][[1L]]
}

get_garage_stat <- function(dataset, garage, statistic) {
  row <- summary_garage[
    summary_garage$dataset == dataset & summary_garage$garasi == garage,
    ,
    drop = FALSE
  ]
  row[[statistic]][[1L]]
}

get_price_spearman <- function(data, variables) {
  values <- vapply(
    variables,
    function(variable) cor(data$harga, data[[variable]], method = "spearman"),
    numeric(1)
  )
  values[order(abs(values), decreasing = TRUE)]
}

jaksel_spearman <- get_price_spearman(
  harga_jaksel,
  setdiff(correlation_columns_jaksel, "harga")
)
rumah_spearman <- get_price_spearman(
  data_rumah,
  setdiff(correlation_columns_rumah, "harga")
)

report_lines <- c(
  "# Analisis Deskriptif Harga Rumah Jakarta Selatan",
  "",
  "Analisis ini menggunakan data yang telah dibersihkan. Duplikat telah dikeluarkan, sedangkan pencilan tetap dipertahankan. Seluruh angka harga menggambarkan harga pada dataset/listing, bukan harga transaksi yang telah diverifikasi.",
  "",
  "## Data utama Jakarta Selatan",
  "",
  paste0("Jumlah observasi: **", format_number_id(nrow(harga_jaksel), 0L), " rumah**. Seluruh observasi berkategori Jakarta Selatan."),
  "",
  paste0(
    "- Median harga adalah **", format_rupiah(get_numeric_stat("harga_rumah_jaksel", "harga", "median")),
    "**, dengan 50% data berada antara **", format_rupiah(get_numeric_stat("harga_rumah_jaksel", "harga", "q1")),
    "** dan **", format_rupiah(get_numeric_stat("harga_rumah_jaksel", "harga", "q3")), "**."
  ),
  paste0(
    "- Mean harga sebesar **", format_rupiah(get_numeric_stat("harga_rumah_jaksel", "harga", "mean")),
    "**, lebih tinggi daripada median. Ini menunjukkan distribusi menceng ke kanan karena sebagian listing berharga sangat tinggi."
  ),
  paste0(
    "- Median luas tanah dan bangunan masing-masing **",
    format_number_id(get_numeric_stat("harga_rumah_jaksel", "luas_tanah", "median"), 0L),
    " m²** dan **", format_number_id(get_numeric_stat("harga_rumah_jaksel", "luas_bangunan", "median"), 0L), " m²**."
  ),
  paste0(
    "- Median harga per m² tanah adalah **",
    format_rupiah(get_numeric_stat("harga_rumah_jaksel", "harga_per_m2_tanah", "median")),
    "/m²**, sedangkan median harga per m² bangunan adalah **",
    format_rupiah(get_numeric_stat("harga_rumah_jaksel", "harga_per_m2_bangunan", "median")), "/m²**."
  ),
  paste0(
    "- Garasi tersedia pada **",
    format_percent(get_garage_stat("harga_rumah_jaksel", "ada", "persentase")),
    "** observasi. Median harga rumah dengan garasi adalah **",
    format_rupiah(get_garage_stat("harga_rumah_jaksel", "ada", "median_harga")),
    "**, dibanding **", format_rupiah(get_garage_stat("harga_rumah_jaksel", "tidak_ada", "median_harga")),
    "** tanpa garasi. Perbandingan ini bersifat deskriptif, bukan efek kausal garasi."
  ),
  paste0(
    "- Hubungan monoton terkuat dengan harga adalah **",
    pretty_variable(names(jaksel_spearman)[[1L]]), "** (korelasi Spearman **",
    sprintf("%.2f", jaksel_spearman[[1L]]), "**), diikuti **",
    pretty_variable(names(jaksel_spearman)[[2L]]), "** (**",
    sprintf("%.2f", jaksel_spearman[[2L]]), "**)."
  ),
  paste0(
    "- Sebanyak **", sum(harga_jaksel$flag_outlier_iqr), " observasi (",
    format_percent(mean(harga_jaksel$flag_outlier_iqr) * 100),
    ")** memiliki sedikitnya satu flag pencilan IQR dan tetap disertakan."
  ),
  "",
  "![Distribusi harga dan hubungan dengan luas](plots/harga_jaksel_distribusi_dan_luas.png)",
  "",
  "![Perbandingan berdasarkan garasi](plots/harga_jaksel_perbandingan_garasi.png)",
  "",
  "![Korelasi Spearman](plots/harga_jaksel_korelasi_spearman.png)",
  "",
  "## Data listing rumah",
  "",
  paste0("Jumlah observasi: **", format_number_id(nrow(data_rumah), 0L), " rumah**."),
  "",
  paste0(
    "- Median harga adalah **", format_rupiah(get_numeric_stat("data_rumah", "harga", "median")),
    "**, dengan 50% data berada antara **", format_rupiah(get_numeric_stat("data_rumah", "harga", "q1")),
    "** dan **", format_rupiah(get_numeric_stat("data_rumah", "harga", "q3")), "**."
  ),
  paste0(
    "- Mean harga sebesar **", format_rupiah(get_numeric_stat("data_rumah", "harga", "mean")),
    "**, juga lebih tinggi daripada median dan menunjukkan kemencengan ke kanan."
  ),
  paste0(
    "- Median luas tanah dan bangunan masing-masing **",
    format_number_id(get_numeric_stat("data_rumah", "luas_tanah", "median"), 0L),
    " m²** dan **", format_number_id(get_numeric_stat("data_rumah", "luas_bangunan", "median"), 1L), " m²**."
  ),
  paste0(
    "- Median harga per m² tanah adalah **",
    format_rupiah(get_numeric_stat("data_rumah", "harga_per_m2_tanah", "median")),
    "/m²**, sedangkan median harga per m² bangunan adalah **",
    format_rupiah(get_numeric_stat("data_rumah", "harga_per_m2_bangunan", "median")), "/m²**."
  ),
  paste0(
    "- Garasi tersedia pada **",
    format_percent(get_garage_stat("data_rumah", "ada", "persentase")),
    "** observasi. Median harga rumah dengan garasi adalah **",
    format_rupiah(get_garage_stat("data_rumah", "ada", "median_harga")),
    "**, dibanding **", format_rupiah(get_garage_stat("data_rumah", "tidak_ada", "median_harga")),
    "** tanpa garasi."
  ),
  paste0(
    "- Hubungan monoton terkuat dengan harga adalah **",
    pretty_variable(names(rumah_spearman)[[1L]]), "** (korelasi Spearman **",
    sprintf("%.2f", rumah_spearman[[1L]]), "**), diikuti **",
    pretty_variable(names(rumah_spearman)[[2L]]), "** (**",
    sprintf("%.2f", rumah_spearman[[2L]]), "**)."
  ),
  paste0(
    "- Sebanyak **", sum(data_rumah$flag_outlier_iqr), " observasi (",
    format_percent(mean(data_rumah$flag_outlier_iqr) * 100),
    ")** memiliki sedikitnya satu flag pencilan IQR dan tetap disertakan."
  ),
  "",
  "![Distribusi harga dan hubungan dengan luas](plots/data_rumah_distribusi_dan_luas.png)",
  "",
  "![Perbandingan berdasarkan garasi](plots/data_rumah_perbandingan_garasi.png)",
  "",
  "![Korelasi Spearman](plots/data_rumah_korelasi_spearman.png)",
  "",
  "## Batas interpretasi",
  "",
  "- Dataset tidak mempunyai tanggal observasi, kecamatan, koordinat, umur bangunan, maupun kondisi rumah yang terstruktur.",
  "- Statistik per garasi tidak mengendalikan perbedaan luas atau karakteristik lain.",
  "- Korelasi menggambarkan hubungan, bukan sebab-akibat.",
  "- Kedua dataset dianalisis terpisah karena cakupan dan struktur sumbernya berbeda.",
  "",
  "Tabel angka lengkap tersedia dalam folder `tabel/`."
)

writeLines(
  report_lines,
  con = file.path(report_dir, "analisis_deskriptif.md"),
  useBytes = TRUE
)

stopifnot(
  nrow(summary_numeric) == length(numeric_columns_jaksel) + length(numeric_columns_rumah),
  !anyNA(summary_numeric),
  all(summary_numeric$n > 0),
  all(abs(correlation_pearson$korelasi) <= 1),
  all(abs(correlation_spearman$korelasi) <= 1),
  file.exists(file.path(report_dir, "analisis_deskriptif.md"))
)

message("Analisis deskriptif selesai.")
message("- Laporan: reports/deskriptif/analisis_deskriptif.md")
message("- Tabel: ", length(list.files(table_dir)), " file")
message("- Plot: ", length(list.files(plot_dir)), " file")
