#!/usr/bin/env Rscript

# Validasi internal model harga rumah Jakarta Selatan.
#
# Validasi menggunakan holdout terstratifikasi 20% yang tidak dipakai saat
# melatih ulang model validasi. Formulasi model ditetapkan dari tahap
# pemodelan sebelumnya. Tahap ini juga mengukur ketidakpastian bootstrap,
# kalibrasi, kesalahan per segmen, sensitivitas pencilan, dan cakupan interval
# prediksi empiris.
#
# Dependensi: base R dan paket randomForest.

options(stringsAsFactors = FALSE, scipen = 999, digits = 15)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Skrip harus dijalankan dengan Rscript.")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop(
    "Paket randomForest belum tersedia. Pasang dengan ",
    "install.packages('randomForest') sebelum menjalankan skrip."
  )
}

script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
project_dir <- dirname(dirname(script_path))
input_path <- file.path(
  project_dir,
  "data",
  "processed",
  "harga_rumah_jaksel_bersih.rds"
)
cv_summary_path <- file.path(
  project_dir,
  "reports",
  "pemodelan",
  "tabel",
  "ringkasan_perbandingan_model.csv"
)
report_dir <- file.path(project_dir, "reports", "validasi_model")
table_dir <- file.path(report_dir, "tabel")
plot_dir <- file.path(report_dir, "plots")
model_dir <- file.path(report_dir, "models")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_path) || !file.exists(cv_summary_path)) {
  stop(
    "Data bersih atau ringkasan pemodelan belum tersedia. Jalankan ",
    "scripts/01_clean_data.R dan scripts/03_pemodelan.R terlebih dahulu."
  )
}

data_clean <- readRDS(input_path)
cv_summary <- read.csv(cv_summary_path, stringsAsFactors = FALSE)
required_columns <- c(
  "id_data", "harga", "luas_tanah", "luas_bangunan", "kamar_tidur",
  "kamar_mandi", "garasi", "flag_data_tidak_valid", "flag_outlier_iqr"
)
if (!all(required_columns %in% names(data_clean))) {
  stop("Struktur data bersih tidak sesuai dengan kebutuhan validasi.")
}

model_data <- data_clean[!data_clean$flag_data_tidak_valid, required_columns]
model_data$garasi <- factor(model_data$garasi, levels = c("tidak_ada", "ada"))
model_data$log_harga <- log(model_data$harga)
model_data$log_luas_tanah <- log(model_data$luas_tanah)
model_data$log_luas_bangunan <- log(model_data$luas_bangunan)

stopifnot(
  nrow(model_data) > 0L,
  !anyNA(model_data),
  all(model_data$harga > 0),
  all(model_data$luas_tanah > 0),
  all(model_data$luas_bangunan > 0)
)

model_ids <- c("linear_raw", "linear_log", "spline_log", "random_forest_log")
model_labels <- c(
  linear_raw = "Linear mentah",
  linear_log = "Log-linear",
  spline_log = "Spline log",
  random_forest_log = "Random Forest log"
)

raw_formula <- harga ~ luas_tanah + luas_bangunan + kamar_tidur + kamar_mandi + garasi
log_formula <- log_harga ~ log_luas_tanah + log_luas_bangunan + kamar_tidur + kamar_mandi + garasi
spline_formula <- log_harga ~
  splines::ns(log_luas_tanah, df = 4) +
  splines::ns(log_luas_bangunan, df = 4) +
  kamar_tidur + kamar_mandi + garasi
rf_formula <- log_harga ~
  log_luas_tanah + log_luas_bangunan + kamar_tidur + kamar_mandi + garasi

write_csv <- function(data, path) {
  write.csv(data, path, row.names = FALSE, na = "")
}

make_stratified_holdout <- function(y, proportion = 0.20, strata = 10L) {
  n <- length(y)
  ordered <- order(y, runif(n))
  stratum <- integer(n)
  stratum[ordered] <- cut(
    seq_len(n),
    breaks = strata,
    labels = FALSE,
    include.lowest = TRUE
  )

  stratum_sizes <- table(stratum)
  desired_total <- round(n * proportion)
  exact_counts <- as.numeric(stratum_sizes) * proportion
  holdout_counts <- floor(exact_counts)
  remainder <- desired_total - sum(holdout_counts)
  if (remainder > 0L) {
    priority <- order(exact_counts - holdout_counts, decreasing = TRUE)
    holdout_counts[priority[seq_len(remainder)]] <-
      holdout_counts[priority[seq_len(remainder)]] + 1L
  }

  is_holdout <- rep(FALSE, n)
  stratum_values <- as.integer(names(stratum_sizes))
  for (index in seq_along(stratum_values)) {
    candidates <- which(stratum == stratum_values[[index]])
    selected <- sample(candidates, size = holdout_counts[[index]], replace = FALSE)
    is_holdout[selected] <- TRUE
  }
  is_holdout
}

smearing_factor <- function(residuals_log) {
  mean(exp(residuals_log), na.rm = TRUE)
}

calculate_metrics <- function(actual, predicted) {
  predicted <- pmax(predicted, 1)
  c(
    mae = mean(abs(actual - predicted)),
    rmse = sqrt(mean((actual - predicted)^2)),
    rmsle = sqrt(mean((log1p(actual) - log1p(predicted))^2)),
    r2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
    mape = mean(abs(actual - predicted) / actual) * 100,
    bias = mean(predicted - actual),
    bias_percent = mean((predicted - actual) / actual) * 100
  )
}

set.seed(20250917)
is_holdout <- make_stratified_holdout(model_data$harga, proportion = 0.20, strata = 10L)
training_data <- model_data[!is_holdout, , drop = FALSE]
holdout_data <- model_data[is_holdout, , drop = FALSE]

split_table <- data.frame(
  id_data = model_data$id_data,
  partisi = ifelse(is_holdout, "holdout", "training")
)

# Model validasi dilatih hanya menggunakan partisi training.
validation_raw <- lm(raw_formula, data = training_data)
validation_log <- lm(log_formula, data = training_data)
validation_spline <- lm(spline_formula, data = training_data)
set.seed(20250917)
validation_rf <- randomForest::randomForest(
  rf_formula,
  data = training_data,
  ntree = 1000,
  mtry = 2,
  nodesize = 5,
  importance = TRUE,
  keep.forest = TRUE
)

raw_prediction_unbounded <- predict(validation_raw, newdata = holdout_data)
log_prediction_log_scale <- predict(validation_log, newdata = holdout_data)
spline_prediction_log_scale <- predict(validation_spline, newdata = holdout_data)
rf_prediction_log_scale <- predict(validation_rf, newdata = holdout_data)

smear_log <- smearing_factor(residuals(validation_log))
smear_spline <- smearing_factor(residuals(validation_spline))
rf_oob_residual_log <- training_data$log_harga - validation_rf$predicted
smear_rf <- smearing_factor(rf_oob_residual_log)

prediction_list <- list(
  linear_raw = pmax(raw_prediction_unbounded, 1),
  linear_log = exp(log_prediction_log_scale) * smear_log,
  spline_log = exp(spline_prediction_log_scale) * smear_spline,
  random_forest_log = exp(rf_prediction_log_scale) * smear_rf
)

holdout_predictions <- do.call(rbind, lapply(model_ids, function(model_id) {
  data.frame(
    id_data = holdout_data$id_data,
    model = model_id,
    aktual = holdout_data$harga,
    prediksi = prediction_list[[model_id]],
    residual = holdout_data$harga - prediction_list[[model_id]],
    absolute_error = abs(holdout_data$harga - prediction_list[[model_id]]),
    absolute_percentage_error =
      abs(holdout_data$harga - prediction_list[[model_id]]) / holdout_data$harga * 100,
    garasi = as.character(holdout_data$garasi),
    flag_outlier_iqr = holdout_data$flag_outlier_iqr
  )
}))
row.names(holdout_predictions) <- NULL

holdout_metrics <- do.call(rbind, lapply(model_ids, function(model_id) {
  metric <- calculate_metrics(holdout_data$harga, prediction_list[[model_id]])
  data.frame(
    model = model_id,
    model_label = unname(model_labels[[model_id]]),
    n_holdout = nrow(holdout_data),
    mae = unname(metric[["mae"]]),
    rmse = unname(metric[["rmse"]]),
    rmsle = unname(metric[["rmsle"]]),
    r2 = unname(metric[["r2"]]),
    mape = unname(metric[["mape"]]),
    bias = unname(metric[["bias"]]),
    bias_percent = unname(metric[["bias_percent"]]),
    prediksi_negatif_sebelum_batas = if (
      model_id == "linear_raw"
    ) sum(raw_prediction_unbounded <= 0) else 0L
  )
}))
holdout_metrics$rank_mae <- rank(holdout_metrics$mae, ties.method = "min")
holdout_metrics$rank_rmsle <- rank(holdout_metrics$rmsle, ties.method = "min")
holdout_metrics <- holdout_metrics[order(holdout_metrics$rank_mae), ]
row.names(holdout_metrics) <- NULL

# Paired bootstrap pada holdout: model dibandingkan pada observasi yang sama.
bootstrap_repetitions <- 2000L
set.seed(20250918)
bootstrap_indices <- replicate(
  bootstrap_repetitions,
  sample(seq_len(nrow(holdout_data)), replace = TRUE)
)
bootstrap_records <- vector("list", length(model_ids))
for (model_index in seq_along(model_ids)) {
  model_id <- model_ids[[model_index]]
  predicted <- prediction_list[[model_id]]
  metric_matrix <- matrix(
    NA_real_,
    nrow = bootstrap_repetitions,
    ncol = 5L,
    dimnames = list(NULL, c("mae", "rmse", "rmsle", "r2", "mape"))
  )
  for (bootstrap_id in seq_len(bootstrap_repetitions)) {
    index <- bootstrap_indices[, bootstrap_id]
    metric <- calculate_metrics(holdout_data$harga[index], predicted[index])
    metric_matrix[bootstrap_id, ] <- metric[colnames(metric_matrix)]
  }

  point_metric <- calculate_metrics(holdout_data$harga, predicted)
  bootstrap_records[[model_index]] <- do.call(rbind, lapply(colnames(metric_matrix), function(metric_name) {
    interval <- quantile(
      metric_matrix[, metric_name],
      probs = c(0.025, 0.975),
      na.rm = TRUE,
      names = FALSE
    )
    data.frame(
      model = model_id,
      model_label = unname(model_labels[[model_id]]),
      metrik = metric_name,
      estimasi = unname(point_metric[[metric_name]]),
      ci_low_95 = interval[[1L]],
      ci_high_95 = interval[[2L]],
      bootstrap_repetitions = bootstrap_repetitions
    )
  }))
}
bootstrap_intervals <- do.call(rbind, bootstrap_records)
row.names(bootstrap_intervals) <- NULL

# Perbandingan dengan repeated CV dari tahap pemodelan.
stability_table <- merge(
  holdout_metrics,
  cv_summary[c(
    "model", "mae_mean", "mae_sd", "rmse_mean", "rmse_sd",
    "rmsle_mean", "rmsle_sd", "r2_mean", "r2_sd"
  )],
  by = "model",
  all.x = TRUE,
  sort = FALSE
)
stability_table$model_label <- unname(model_labels[stability_table$model])
stability_table$mae_delta_percent <-
  (stability_table$mae - stability_table$mae_mean) / stability_table$mae_mean * 100
stability_table$rmse_delta_percent <-
  (stability_table$rmse - stability_table$rmse_mean) / stability_table$rmse_mean * 100
stability_table$rmsle_delta_percent <-
  (stability_table$rmsle - stability_table$rmsle_mean) / stability_table$rmsle_mean * 100
stability_table$r2_delta <- stability_table$r2 - stability_table$r2_mean
stability_table$mae_dalam_dua_sd_cv <-
  abs(stability_table$mae - stability_table$mae_mean) <= 2 * stability_table$mae_sd
stability_table$rmsle_dalam_dua_sd_cv <-
  abs(stability_table$rmsle - stability_table$rmsle_mean) <= 2 * stability_table$rmsle_sd
stability_table <- stability_table[c(
  "model", "model_label", "mae", "mae_mean", "mae_delta_percent",
  "mae_dalam_dua_sd_cv", "rmse", "rmse_mean", "rmse_delta_percent",
  "rmsle", "rmsle_mean", "rmsle_delta_percent",
  "rmsle_dalam_dua_sd_cv", "r2", "r2_mean", "r2_delta"
)]

# Kalibrasi global pada skala log dan kalibrasi menurut desil prediksi.
calibration_global <- do.call(rbind, lapply(model_ids, function(model_id) {
  predicted <- prediction_list[[model_id]]
  calibration_model <- lm(log(holdout_data$harga) ~ log(predicted))
  data.frame(
    model = model_id,
    model_label = unname(model_labels[[model_id]]),
    calibration_intercept = unname(coef(calibration_model)[[1L]]),
    calibration_slope = unname(coef(calibration_model)[[2L]]),
    calibration_r_squared = summary(calibration_model)$r.squared,
    mean_log_error = mean(log(predicted) - log(holdout_data$harga)),
    median_prediction_actual_ratio = median(predicted / holdout_data$harga)
  )
}))

calibration_deciles <- do.call(rbind, lapply(model_ids, function(model_id) {
  predicted <- prediction_list[[model_id]]
  ordered_rank <- rank(predicted, ties.method = "first")
  decile <- cut(
    ordered_rank,
    breaks = 10L,
    labels = FALSE,
    include.lowest = TRUE
  )
  do.call(rbind, lapply(sort(unique(decile)), function(decile_id) {
    selected <- decile == decile_id
    data.frame(
      model = model_id,
      model_label = unname(model_labels[[model_id]]),
      desil_prediksi = decile_id,
      jumlah = sum(selected),
      median_prediksi = median(predicted[selected]),
      median_aktual = median(holdout_data$harga[selected]),
      mean_prediksi = mean(predicted[selected]),
      mean_aktual = mean(holdout_data$harga[selected])
    )
  }))
}))

# Kesalahan menurut kuartil harga, status garasi, dan status pencilan.
training_price_breaks <- quantile(
  training_data$harga,
  probs = c(0.25, 0.50, 0.75),
  names = FALSE
)
price_band <- cut(
  holdout_data$harga,
  breaks = c(-Inf, training_price_breaks, Inf),
  labels = c("Q1_harga_rendah", "Q2", "Q3", "Q4_harga_tinggi"),
  include.lowest = TRUE
)

segment_metrics <- function(model_id, group_type, group_value) {
  selected <- switch(
    group_type,
    kuartil_harga = price_band == group_value,
    garasi = as.character(holdout_data$garasi) == group_value,
    status_outlier = if (group_value == "outlier_iqr") {
      holdout_data$flag_outlier_iqr
    } else {
      !holdout_data$flag_outlier_iqr
    }
  )
  metric <- calculate_metrics(
    holdout_data$harga[selected],
    prediction_list[[model_id]][selected]
  )
  data.frame(
    model = model_id,
    model_label = unname(model_labels[[model_id]]),
    tipe_segmen = group_type,
    segmen = group_value,
    jumlah = sum(selected),
    mae = unname(metric[["mae"]]),
    rmse = unname(metric[["rmse"]]),
    rmsle = unname(metric[["rmsle"]]),
    r2 = unname(metric[["r2"]]),
    mape = unname(metric[["mape"]]),
    bias = unname(metric[["bias"]])
  )
}

segment_definitions <- list(
  kuartil_harga = levels(price_band),
  garasi = c("tidak_ada", "ada"),
  status_outlier = c("non_outlier_iqr", "outlier_iqr")
)
segment_results <- list()
segment_index <- 1L
for (model_id in model_ids) {
  for (group_type in names(segment_definitions)) {
    for (group_value in segment_definitions[[group_type]]) {
      segment_results[[segment_index]] <- segment_metrics(
        model_id,
        group_type,
        group_value
      )
      segment_index <- segment_index + 1L
    }
  }
}
segment_table <- do.call(rbind, segment_results)

# Keseimbangan karakteristik antara training dan holdout.
standardized_mean_difference <- function(train_value, holdout_value) {
  pooled_sd <- sqrt((var(train_value) + var(holdout_value)) / 2)
  (mean(holdout_value) - mean(train_value)) / pooled_sd
}

balance_columns <- c(
  "harga", "luas_tanah", "luas_bangunan", "kamar_tidur", "kamar_mandi"
)
balance_table <- do.call(rbind, lapply(balance_columns, function(column) {
  data.frame(
    variabel = column,
    mean_training = mean(training_data[[column]]),
    mean_holdout = mean(holdout_data[[column]]),
    median_training = median(training_data[[column]]),
    median_holdout = median(holdout_data[[column]]),
    standardized_mean_difference = standardized_mean_difference(
      training_data[[column]],
      holdout_data[[column]]
    )
  )
}))
garage_balance <- data.frame(
  variabel = "proporsi_garasi_ada",
  mean_training = mean(training_data$garasi == "ada"),
  mean_holdout = mean(holdout_data$garasi == "ada"),
  median_training = NA_real_,
  median_holdout = NA_real_,
  standardized_mean_difference =
    (mean(holdout_data$garasi == "ada") - mean(training_data$garasi == "ada")) /
    sqrt(
      (
        mean(training_data$garasi == "ada") *
          (1 - mean(training_data$garasi == "ada")) +
          mean(holdout_data$garasi == "ada") *
          (1 - mean(holdout_data$garasi == "ada"))
      ) / 2
    )
)
balance_table <- rbind(balance_table, garage_balance)

# Interval prediksi empiris pada skala log untuk dua model rekomendasi.
interval_model_data <- list(
  linear_log = list(
    center = log_prediction_log_scale,
    residual = residuals(validation_log)
  ),
  random_forest_log = list(
    center = rf_prediction_log_scale,
    residual = rf_oob_residual_log
  )
)

interval_predictions <- list()
coverage_records <- list()
interval_index <- 1L
for (model_id in names(interval_model_data)) {
  center <- interval_model_data[[model_id]]$center
  residual_log <- interval_model_data[[model_id]]$residual
  for (level in c(0.80, 0.95)) {
    alpha <- 1 - level
    residual_quantiles <- quantile(
      residual_log,
      probs = c(alpha / 2, 1 - alpha / 2),
      names = FALSE
    )
    lower <- exp(center + residual_quantiles[[1L]])
    upper <- exp(center + residual_quantiles[[2L]])
    covered <- holdout_data$harga >= lower & holdout_data$harga <= upper
    coverage_records[[interval_index]] <- data.frame(
      model = model_id,
      model_label = unname(model_labels[[model_id]]),
      nominal_coverage = level,
      empirical_coverage = mean(covered),
      mean_interval_width = mean(upper - lower),
      median_interval_width = median(upper - lower)
    )
    interval_predictions[[interval_index]] <- data.frame(
      id_data = holdout_data$id_data,
      model = model_id,
      nominal_coverage = level,
      aktual = holdout_data$harga,
      lower = lower,
      upper = upper,
      covered = covered
    )
    interval_index <- interval_index + 1L
  }
}
coverage_table <- do.call(rbind, coverage_records)
prediction_intervals <- do.call(rbind, interval_predictions)

write_csv(split_table, file.path(table_dir, "pembagian_training_holdout.csv"))
write_csv(holdout_predictions, file.path(table_dir, "prediksi_holdout.csv"))
write_csv(holdout_metrics, file.path(table_dir, "metrik_holdout.csv"))
write_csv(bootstrap_intervals, file.path(table_dir, "interval_bootstrap_metrik.csv"))
write_csv(stability_table, file.path(table_dir, "perbandingan_cv_dan_holdout.csv"))
write_csv(calibration_global, file.path(table_dir, "kalibrasi_global.csv"))
write_csv(calibration_deciles, file.path(table_dir, "kalibrasi_desil.csv"))
write_csv(segment_table, file.path(table_dir, "kesalahan_per_segmen.csv"))
write_csv(balance_table, file.path(table_dir, "keseimbangan_split.csv"))
write_csv(coverage_table, file.path(table_dir, "cakupan_interval_prediksi.csv"))
write_csv(prediction_intervals, file.path(table_dir, "interval_prediksi_holdout.csv"))

saveRDS(validation_raw, file.path(model_dir, "validasi_model_linear_mentah.rds"))
saveRDS(validation_log, file.path(model_dir, "validasi_model_log_linear.rds"))
saveRDS(validation_spline, file.path(model_dir, "validasi_model_spline_log.rds"))
saveRDS(validation_rf, file.path(model_dir, "validasi_model_random_forest_log.rds"))
saveRDS(
  list(
    random_seed_split = 20250917,
    random_seed_bootstrap = 20250918,
    bootstrap_repetitions = bootstrap_repetitions,
    training_ids = training_data$id_data,
    holdout_ids = holdout_data$id_data,
    smearing_factors = c(
      linear_log = smear_log,
      spline_log = smear_spline,
      random_forest_log = smear_rf
    )
  ),
  file.path(model_dir, "metadata_validasi.rds")
)

plot_actual_prediction <- function(path) {
  selected_models <- c("linear_log", "random_forest_log")
  limits <- range(holdout_data$harga)
  colors <- c(linear_log = "#F28E2B", random_forest_log = "#59A14F")

  png(path, width = 1800, height = 850, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 1.2), oma = c(0, 0, 3, 0), las = 1)

  for (model_id in selected_models) {
    metric <- holdout_metrics[holdout_metrics$model == model_id, ]
    plot(
      holdout_data$harga / 1e9,
      prediction_list[[model_id]] / 1e9,
      log = "xy",
      pch = 16,
      cex = 0.8,
      col = grDevices::adjustcolor(colors[[model_id]], alpha.f = 0.55),
      xlim = limits / 1e9,
      ylim = limits / 1e9,
      main = paste0(
        model_labels[[model_id]],
        "\nMAE = Rp", sprintf("%.2f", metric$mae / 1e9), " miliar"
      ),
      xlab = "Harga aktual (Rp miliar, skala log)",
      ylab = "Prediksi holdout (Rp miliar, skala log)"
    )
    abline(a = 0, b = 1, lty = 2, lwd = 2, col = "#333333")
  }
  mtext("Validasi Holdout: Aktual vs Prediksi", outer = TRUE, line = 1, cex = 1.2, font = 2)
}

plot_calibration <- function(path) {
  selected_models <- c("linear_log", "random_forest_log")
  colors <- c(linear_log = "#F28E2B", random_forest_log = "#59A14F")
  selected_calibration <- calibration_deciles[
    calibration_deciles$model %in% selected_models,
  ]
  limits <- range(c(
    selected_calibration$median_aktual,
    selected_calibration$median_prediksi
  ))

  png(path, width = 1800, height = 850, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 1.2), oma = c(0, 0, 5, 0), las = 1)

  for (model_id in selected_models) {
    selected <- calibration_deciles[calibration_deciles$model == model_id, ]
    global <- calibration_global[calibration_global$model == model_id, ]
    plot(
      selected$median_prediksi / 1e9,
      selected$median_aktual / 1e9,
      log = "xy",
      type = "b",
      pch = 16,
      lwd = 2,
      col = colors[[model_id]],
      xlim = limits / 1e9,
      ylim = limits / 1e9,
      main = paste0(
        model_labels[[model_id]],
        "\nSlope log = ", sprintf("%.2f", global$calibration_slope)
      ),
      xlab = "Median prediksi per desil (Rp miliar, log)",
      ylab = "Median aktual per desil (Rp miliar, log)"
    )
    abline(a = 0, b = 1, lty = 2, lwd = 2, col = "#333333")
  }
  mtext("Kalibrasi Menurut Desil Prediksi", outer = TRUE, line = 2.5, cex = 1.2, font = 2)
}

plot_segment_error <- function(path) {
  selected <- segment_table[
    segment_table$model %in% c("linear_log", "random_forest_log") &
      segment_table$tipe_segmen == "kuartil_harga",
  ]
  matrix_mae <- rbind(
    selected$mae[selected$model == "linear_log"] / 1e9,
    selected$mae[selected$model == "random_forest_log"] / 1e9
  )
  colnames(matrix_mae) <- c("Q1 rendah", "Q2", "Q3", "Q4 tinggi")
  rownames(matrix_mae) <- c("Log-linear", "Random Forest log")

  png(path, width = 1500, height = 900, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mar = c(6, 5, 4, 2), las = 1)
  barplot(
    matrix_mae,
    beside = TRUE,
    col = c("#F28E2B", "#59A14F"),
    border = NA,
    main = "MAE Holdout Menurut Kuartil Harga Aktual",
    xlab = "Segmen harga",
    ylab = "MAE (Rp miliar)",
    legend.text = rownames(matrix_mae),
    args.legend = list(x = "topleft", bty = "n")
  )
}

plot_prediction_intervals <- function(path) {
  model_id <- "random_forest_log"
  interval_80 <- prediction_intervals[
    prediction_intervals$model == model_id &
      prediction_intervals$nominal_coverage == 0.80,
  ]
  interval_95 <- prediction_intervals[
    prediction_intervals$model == model_id &
      prediction_intervals$nominal_coverage == 0.95,
  ]
  order_index <- order(prediction_list[[model_id]])
  x <- seq_along(order_index)

  png(path, width = 1800, height = 900, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mar = c(5, 5, 4, 2), las = 1)
  plot(
    x,
    holdout_data$harga[order_index] / 1e9,
    log = "y",
    pch = 16,
    cex = 0.65,
    col = "#E15759",
    xlab = "Observasi holdout, diurutkan menurut prediksi",
    ylab = "Harga (Rp miliar, skala log)",
    main = "Interval Prediksi Empiris Random Forest"
  )
  segments(
    x,
    interval_95$lower[order_index] / 1e9,
    x,
    interval_95$upper[order_index] / 1e9,
    col = grDevices::adjustcolor("#BAB0AC", alpha.f = 0.35)
  )
  segments(
    x,
    interval_80$lower[order_index] / 1e9,
    x,
    interval_80$upper[order_index] / 1e9,
    col = grDevices::adjustcolor("#4E79A7", alpha.f = 0.60)
  )
  points(
    x,
    prediction_list[[model_id]][order_index] / 1e9,
    pch = 16,
    cex = 0.55,
    col = "#59A14F"
  )
  points(
    x,
    holdout_data$harga[order_index] / 1e9,
    pch = 16,
    cex = 0.65,
    col = "#E15759"
  )
  legend(
    "topleft",
    legend = c("Aktual", "Prediksi", "Interval 80%", "Interval 95%"),
    col = c("#E15759", "#59A14F", "#4E79A7", "#BAB0AC"),
    pch = c(16, 16, NA, NA),
    lty = c(NA, NA, 1, 1),
    lwd = c(NA, NA, 2, 2),
    bty = "n"
  )
}

plot_actual_prediction(file.path(plot_dir, "aktual_vs_prediksi_holdout.png"))
plot_calibration(file.path(plot_dir, "kalibrasi_desil.png"))
plot_segment_error(file.path(plot_dir, "mae_per_segmen_harga.png"))
plot_prediction_intervals(file.path(plot_dir, "interval_prediksi_random_forest.png"))

format_number_id <- function(x, digits = 2L) {
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
  if (abs(x) >= 1e9) {
    return(paste0("Rp", format_number_id(x / 1e9, 2L), " miliar"))
  }
  paste0("Rp", format_number_id(x / 1e6, 2L), " juta")
}

format_percent <- function(x) {
  paste0(format_number_id(x, 1L), "%")
}

get_metric <- function(model_id, metric) {
  holdout_metrics[holdout_metrics$model == model_id, metric][[1L]]
}

get_bootstrap <- function(model_id, metric) {
  bootstrap_intervals[
    bootstrap_intervals$model == model_id & bootstrap_intervals$metrik == metric,
  ]
}

get_calibration <- function(model_id, field) {
  calibration_global[calibration_global$model == model_id, field][[1L]]
}

get_coverage <- function(model_id, nominal, field) {
  coverage_table[
    coverage_table$model == model_id &
      coverage_table$nominal_coverage == nominal,
    field
  ][[1L]]
}

get_segment <- function(model_id, segment, field) {
  segment_table[
    segment_table$model == model_id & segment_table$segmen == segment,
    field
  ][[1L]]
}

markdown_model_rows <- vapply(seq_len(nrow(holdout_metrics)), function(index) {
  row <- holdout_metrics[index, ]
  paste0(
    "| ", row$model_label,
    " | ", format_rupiah(row$mae),
    " | ", format_rupiah(row$rmse),
    " | ", format_number_id(row$rmsle, 3L),
    " | ", format_number_id(row$r2, 3L),
    " | ", format_number_id(row$mape, 1L), "% |",
    " ", format_percent(row$bias_percent), " |"
  )
}, character(1))

best_holdout_mae <- holdout_metrics$model[which.min(holdout_metrics$mae)]
best_holdout_rmsle <- holdout_metrics$model[which.min(holdout_metrics$rmsle)]
rf_mae_bootstrap <- get_bootstrap("random_forest_log", "mae")
rf_rmsle_bootstrap <- get_bootstrap("random_forest_log", "rmsle")
rf_stability <- stability_table[stability_table$model == "random_forest_log", ]
rf_calibration_slope <- get_calibration("random_forest_log", "calibration_slope")
rf_stable <-
  rf_stability$mae_dalam_dua_sd_cv &
  rf_stability$rmsle_dalam_dua_sd_cv
rf_calibrated <- rf_calibration_slope >= 0.80 && rf_calibration_slope <= 1.20

validation_conclusion <- if (rf_stable && rf_calibrated) {
  "Random Forest menunjukkan stabilitas dan kalibrasi internal yang memadai untuk estimasi awal, tetapi belum tervalidasi secara eksternal."
} else if (rf_stable) {
  "Random Forest cukup stabil terhadap hasil cross-validation, tetapi kalibrasinya belum ideal. Prediksi sebaiknya dipakai sebagai kisaran, bukan angka tunggal."
} else {
  "Kinerja Random Forest pada holdout belum cukup stabil dibanding hasil cross-validation. Model belum layak dipakai sebagai estimator tunggal tanpa data tambahan dan validasi eksternal."
}

report_lines <- c(
  "# Validasi Internal Model Harga Rumah",
  "",
  paste0(
    "Validasi menggunakan **", nrow(training_data), " observasi training (",
    format_percent(nrow(training_data) / nrow(model_data) * 100), ")** dan **",
    nrow(holdout_data), " observasi holdout (",
    format_percent(nrow(holdout_data) / nrow(model_data) * 100),
    ")**. Holdout dibentuk secara terstratifikasi berdasarkan harga dan tidak digunakan untuk melatih model validasi."
  ),
  "",
  "## Hasil holdout",
  "",
  "| Model | MAE | RMSE | RMSLE | R² | MAPE | Bias relatif |",
  "|---|---:|---:|---:|---:|---:|---:|",
  markdown_model_rows,
  "",
  paste0(
    "Model dengan MAE holdout terbaik adalah **", model_labels[[best_holdout_mae]],
    "**, sedangkan RMSLE terbaik diperoleh **", model_labels[[best_holdout_rmsle]], "**."
  ),
  paste0(
    "Untuk Random Forest, MAE holdout adalah **", format_rupiah(get_metric("random_forest_log", "mae")),
    "** dengan interval bootstrap 95% **", format_rupiah(rf_mae_bootstrap$ci_low_95),
    "–", format_rupiah(rf_mae_bootstrap$ci_high_95), "**. RMSLE-nya **",
    format_number_id(get_metric("random_forest_log", "rmsle"), 3L),
    "** (95% bootstrap ", format_number_id(rf_rmsle_bootstrap$ci_low_95, 3L),
    "–", format_number_id(rf_rmsle_bootstrap$ci_high_95, 3L), ")."
  ),
  "",
  "![Aktual dan prediksi holdout](plots/aktual_vs_prediksi_holdout.png)",
  "",
  "## Stabilitas dan kalibrasi",
  "",
  paste0(
    "- MAE Random Forest pada holdout berubah **",
    format_percent(rf_stability$mae_delta_percent),
    "** dibanding rata-rata cross-validation dan ",
    if (rf_stability$mae_dalam_dua_sd_cv) "masih" else "tidak",
    " berada dalam rentang dua simpangan baku hasil CV."
  ),
  paste0(
    "- Calibration slope Random Forest pada skala log adalah **",
    format_number_id(rf_calibration_slope, 3L),
    "**; nilai ideal adalah 1."
  ),
  paste0(
    "- Median rasio prediksi terhadap aktual Random Forest adalah **",
    format_number_id(
      get_calibration("random_forest_log", "median_prediction_actual_ratio"),
      3L
    ),
    "**."
  ),
  "",
  "![Kalibrasi menurut desil](plots/kalibrasi_desil.png)",
  "",
  "## Ketidakpastian dan segmen harga",
  "",
  paste0(
    "- Interval prediksi empiris Random Forest 80% mencakup **",
    format_percent(get_coverage("random_forest_log", 0.80, "empirical_coverage") * 100),
    "** harga aktual; interval 95% mencakup **",
    format_percent(get_coverage("random_forest_log", 0.95, "empirical_coverage") * 100),
    "**."
  ),
  paste0(
    "- Median lebar interval prediksi 95% Random Forest mencapai **",
    format_rupiah(get_coverage("random_forest_log", 0.95, "median_interval_width")),
    "**, sehingga cakupan yang baik tetap disertai ketidakpastian yang lebar."
  ),
  paste0(
    "- MAE Random Forest meningkat menjadi **",
    format_rupiah(get_segment("random_forest_log", "Q4_harga_tinggi", "mae")),
    "** pada kuartil harga tertinggi. Ini menunjukkan bahwa rumah supermewah belum diprediksi dengan stabil oleh variabel yang tersedia."
  ),
  "- Tabel sensitivitas memisahkan hasil untuk observasi dengan dan tanpa flag pencilan IQR; tidak ada pencilan yang dihapus dari evaluasi utama.",
  "",
  "![MAE menurut segmen harga](plots/mae_per_segmen_harga.png)",
  "",
  "![Interval prediksi Random Forest](plots/interval_prediksi_random_forest.png)",
  "",
  "## Kesimpulan validasi",
  "",
  validation_conclusion,
  "",
  "Validasi ini masih bersifat **internal**. Formulasi model telah dibandingkan pada dataset yang sama di tahap sebelumnya, sehingga holdout ini bukan pengganti validasi eksternal. Dataset juga tidak memiliki kecamatan, koordinat, umur dan kondisi bangunan, lebar jalan, maupun tanggal observasi. Sebelum dipakai untuk keputusan finansial atau penilaian properti nyata, model perlu diuji pada data Jakarta Selatan lain yang benar-benar berasal dari periode dan sumber berbeda.",
  "",
  "Model pada folder validasi hanya dilatih dengan partisi training. Model akhir dari tahap pemodelan sebelumnya, yang dilatih dengan seluruh data, tidak diubah."
)

writeLines(
  report_lines,
  file.path(report_dir, "laporan_validasi_model.md"),
  useBytes = TRUE
)

stopifnot(
  nrow(training_data) + nrow(holdout_data) == nrow(model_data),
  nrow(holdout_data) == round(nrow(model_data) * 0.20),
  length(intersect(training_data$id_data, holdout_data$id_data)) == 0L,
  nrow(holdout_predictions) == nrow(holdout_data) * length(model_ids),
  nrow(bootstrap_intervals) == length(model_ids) * 5L,
  all(holdout_predictions$prediksi > 0),
  all(is.finite(holdout_metrics$mae)),
  all(coverage_table$empirical_coverage >= 0 & coverage_table$empirical_coverage <= 1),
  max(abs(balance_table$standardized_mean_difference), na.rm = TRUE) < 0.50,
  length(list.files(plot_dir, pattern = "[.]png$")) == 4L,
  file.exists(file.path(report_dir, "laporan_validasi_model.md"))
)

message("Validasi model selesai.")
message("- Training: ", nrow(training_data), " observasi")
message("- Holdout: ", nrow(holdout_data), " observasi")
message("- MAE holdout terbaik: ", model_labels[[best_holdout_mae]])
message("- RMSLE holdout terbaik: ", model_labels[[best_holdout_rmsle]])
message("- Laporan: reports/validasi_model/laporan_validasi_model.md")
