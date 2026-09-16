#!/usr/bin/env Rscript

# Pemodelan harga rumah Jakarta Selatan.
#
# Model yang dibandingkan:
# 1. Regresi linear pada harga asli.
# 2. Regresi log-linear dengan koreksi smearing.
# 3. Regresi spline pada log(harga) dengan koreksi smearing.
# 4. Random Forest pada log(harga) dengan koreksi smearing OOB.
#
# Evaluasi dilakukan memakai repeated stratified 10-fold cross-validation.
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
report_dir <- file.path(project_dir, "reports", "pemodelan")
table_dir <- file.path(report_dir, "tabel")
plot_dir <- file.path(report_dir, "plots")
model_dir <- file.path(report_dir, "models")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_path)) {
  stop("Data bersih tidak ditemukan. Jalankan scripts/01_clean_data.R terlebih dahulu.")
}

data_clean <- readRDS(input_path)
required_columns <- c(
  "id_data", "harga", "luas_tanah", "luas_bangunan", "kamar_tidur",
  "kamar_mandi", "garasi", "flag_data_tidak_valid"
)
if (!all(required_columns %in% names(data_clean))) {
  stop("Struktur data bersih tidak sesuai dengan kebutuhan pemodelan.")
}

model_data <- data_clean[!data_clean$flag_data_tidak_valid, required_columns]
model_data$garasi <- factor(
  model_data$garasi,
  levels = c("tidak_ada", "ada")
)
model_data$log_harga <- log(model_data$harga)
model_data$log_luas_tanah <- log(model_data$luas_tanah)
model_data$log_luas_bangunan <- log(model_data$luas_bangunan)

stopifnot(
  nrow(model_data) > 0L,
  !anyNA(model_data),
  all(model_data$harga > 0),
  all(model_data$luas_tanah > 0),
  all(model_data$luas_bangunan > 0),
  nlevels(model_data$garasi) == 2L
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

make_stratified_folds <- function(y, k = 10L, strata = 10L) {
  n <- length(y)
  ordered <- order(y, runif(n))
  stratum <- integer(n)
  stratum[ordered] <- cut(
    seq_len(n),
    breaks = strata,
    labels = FALSE,
    include.lowest = TRUE
  )
  fold <- integer(n)
  for (value in sort(unique(stratum))) {
    index <- which(stratum == value)
    fold[index] <- sample(rep(seq_len(k), length.out = length(index)))
  }
  fold
}

smearing_factor <- function(residuals_log) {
  mean(exp(residuals_log), na.rm = TRUE)
}

calculate_metrics <- function(actual, predicted) {
  predicted <- pmax(predicted, 1)
  data.frame(
    mae = mean(abs(actual - predicted)),
    rmse = sqrt(mean((actual - predicted)^2)),
    rmsle = sqrt(mean((log1p(actual) - log1p(predicted))^2)),
    r2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
    mape = mean(abs(actual - predicted) / actual) * 100
  )
}

fit_fold_models <- function(train, test, seed) {
  fit_raw <- lm(raw_formula, data = train)
  prediction_raw_unbounded <- predict(fit_raw, newdata = test)
  prediction_raw <- pmax(prediction_raw_unbounded, 1)

  fit_log <- lm(log_formula, data = train)
  smear_log <- smearing_factor(residuals(fit_log))
  prediction_log <- exp(predict(fit_log, newdata = test)) * smear_log

  fit_spline <- lm(spline_formula, data = train)
  smear_spline <- smearing_factor(residuals(fit_spline))
  prediction_spline <- exp(predict(fit_spline, newdata = test)) * smear_spline

  set.seed(seed)
  fit_rf <- randomForest::randomForest(
    rf_formula,
    data = train,
    ntree = 500,
    mtry = 2,
    nodesize = 5,
    importance = TRUE,
    keep.forest = TRUE
  )
  smear_rf <- smearing_factor(train$log_harga - fit_rf$predicted)
  prediction_rf <- exp(predict(fit_rf, newdata = test)) * smear_rf

  list(
    predictions = list(
      linear_raw = prediction_raw,
      linear_log = prediction_log,
      spline_log = prediction_spline,
      random_forest_log = prediction_rf
    ),
    raw_negative_predictions = sum(prediction_raw_unbounded <= 0)
  )
}

set.seed(20250916)
number_folds <- 10L
number_repeats <- 5L
metrics_records <- list()
prediction_records <- list()
record_index <- 1L

for (repeat_id in seq_len(number_repeats)) {
  set.seed(20250916 + repeat_id)
  fold_id <- make_stratified_folds(
    model_data$harga,
    k = number_folds,
    strata = 10L
  )

  for (fold in seq_len(number_folds)) {
    train <- model_data[fold_id != fold, , drop = FALSE]
    test <- model_data[fold_id == fold, , drop = FALSE]
    fold_result <- fit_fold_models(
      train,
      test,
      seed = 20250916 + repeat_id * 100L + fold
    )

    for (model_id in model_ids) {
      predicted <- fold_result$predictions[[model_id]]
      metric <- calculate_metrics(test$harga, predicted)
      metric$model <- model_id
      metric$repeat_id <- repeat_id
      metric$fold_id <- fold
      metric$n_test <- nrow(test)
      metric$prediksi_negatif_sebelum_batas <- if (
        model_id == "linear_raw"
      ) fold_result$raw_negative_predictions else 0L
      metrics_records[[record_index]] <- metric[c(
        "model", "repeat_id", "fold_id", "n_test", "mae", "rmse",
        "rmsle", "r2", "mape", "prediksi_negatif_sebelum_batas"
      )]

      prediction_records[[record_index]] <- data.frame(
        id_data = test$id_data,
        model = model_id,
        repeat_id = repeat_id,
        fold_id = fold,
        aktual = test$harga,
        prediksi = predicted
      )
      record_index <- record_index + 1L
    }
  }
}

cv_metrics <- do.call(rbind, metrics_records)
cv_predictions <- do.call(rbind, prediction_records)

summarize_model_metrics <- function(data, model_id) {
  selected <- data[data$model == model_id, , drop = FALSE]
  data.frame(
    model = model_id,
    model_label = unname(model_labels[[model_id]]),
    jumlah_fold = nrow(selected),
    mae_mean = mean(selected$mae),
    mae_sd = sd(selected$mae),
    rmse_mean = mean(selected$rmse),
    rmse_sd = sd(selected$rmse),
    rmsle_mean = mean(selected$rmsle),
    rmsle_sd = sd(selected$rmsle),
    r2_mean = mean(selected$r2),
    r2_sd = sd(selected$r2),
    mape_mean = mean(selected$mape),
    mape_sd = sd(selected$mape),
    total_prediksi_negatif_sebelum_batas = sum(
      selected$prediksi_negatif_sebelum_batas
    )
  )
}

cv_summary <- do.call(
  rbind,
  lapply(model_ids, function(model_id) summarize_model_metrics(cv_metrics, model_id))
)
cv_summary$rank_mae <- rank(cv_summary$mae_mean, ties.method = "min")
cv_summary$rank_rmsle <- rank(cv_summary$rmsle_mean, ties.method = "min")
cv_summary <- cv_summary[order(cv_summary$rank_mae), ]
row.names(cv_summary) <- NULL

# Fit akhir pada seluruh data. Model akhir disimpan untuk penggunaan berikutnya.
final_raw <- lm(raw_formula, data = model_data)
final_log <- lm(log_formula, data = model_data)
final_spline <- lm(spline_formula, data = model_data)
set.seed(20250916)
final_rf <- randomForest::randomForest(
  rf_formula,
  data = model_data,
  ntree = 1000,
  mtry = 2,
  nodesize = 5,
  importance = TRUE,
  keep.forest = TRUE
)

final_smearing <- data.frame(
  model = c("linear_log", "spline_log", "random_forest_log"),
  smearing_factor = c(
    smearing_factor(residuals(final_log)),
    smearing_factor(residuals(final_spline)),
    smearing_factor(model_data$log_harga - final_rf$predicted)
  )
)

vcov_hc3 <- function(model) {
  design <- model.matrix(model)
  residual_value <- residuals(model)
  leverage <- hatvalues(model)
  bread <- solve(crossprod(design))
  omega <- (residual_value / (1 - leverage))^2
  meat <- crossprod(design, design * omega)
  bread %*% meat %*% bread
}

robust_coefficient_table <- function(model) {
  estimate <- coef(model)
  robust_covariance <- vcov_hc3(model)
  robust_se <- sqrt(diag(robust_covariance))
  degrees_freedom <- df.residual(model)
  critical_value <- qt(0.975, degrees_freedom)
  statistic <- estimate / robust_se
  p_value <- 2 * pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE)
  terms <- names(estimate)

  effect_basis <- rep("intercept", length(terms))
  effect_percent <- rep(NA_real_, length(terms))
  log_terms <- terms %in% c("log_luas_tanah", "log_luas_bangunan")
  discrete_terms <- terms %in% c("kamar_tidur", "kamar_mandi", "garasiada")
  effect_basis[log_terms] <- "persen_y_per_kenaikan_1_persen_x"
  effect_percent[log_terms] <- estimate[log_terms]
  effect_basis[discrete_terms] <- "persen_y_per_kenaikan_1_unit_atau_kategori"
  effect_percent[discrete_terms] <- (exp(estimate[discrete_terms]) - 1) * 100

  data.frame(
    term = terms,
    estimate = unname(estimate),
    se_hc3 = unname(robust_se),
    statistic = unname(statistic),
    p_value = unname(p_value),
    conf_low_95 = unname(estimate - critical_value * robust_se),
    conf_high_95 = unname(estimate + critical_value * robust_se),
    effect_basis = effect_basis,
    effect_percent = effect_percent
  )
}

calculate_vif <- function(model) {
  design <- model.matrix(model)
  design <- design[, colnames(design) != "(Intercept)", drop = FALSE]
  result <- vapply(seq_len(ncol(design)), function(column_index) {
    response <- design[, column_index]
    other <- design[, -column_index, drop = FALSE]
    auxiliary <- lm(response ~ other)
    1 / (1 - summary(auxiliary)$r.squared)
  }, numeric(1))
  data.frame(term = colnames(design), vif = unname(result))
}

log_coefficients <- robust_coefficient_table(final_log)
vif_table <- calculate_vif(final_log)

log_residuals <- residuals(final_log)
log_fitted <- fitted(final_log)
design_log <- model.matrix(final_log)
bp_data <- data.frame(
  residual_squared = log_residuals^2,
  design_log[, colnames(design_log) != "(Intercept)", drop = FALSE],
  check.names = TRUE
)
bp_model <- lm(residual_squared ~ ., data = bp_data)
bp_statistic <- nrow(model_data) * summary(bp_model)$r.squared
bp_df <- ncol(design_log) - 1L
bp_p_value <- pchisq(bp_statistic, df = bp_df, lower.tail = FALSE)
cook_values <- cooks.distance(final_log)

diagnostics <- data.frame(
  metrik = c(
    "jumlah_observasi",
    "jumlah_parameter_termasuk_intercept",
    "r_squared_log",
    "adjusted_r_squared_log",
    "rmse_residual_log",
    "maksimum_vif",
    "breusch_pagan_statistic",
    "breusch_pagan_df",
    "breusch_pagan_p_value",
    "ambang_cooks_distance_4_per_n",
    "jumlah_observasi_di_atas_ambang_cooks",
    "maksimum_cooks_distance",
    "smearing_factor_log_linear"
  ),
  nilai = c(
    nrow(model_data),
    length(coef(final_log)),
    summary(final_log)$r.squared,
    summary(final_log)$adj.r.squared,
    sqrt(mean(log_residuals^2)),
    max(vif_table$vif),
    bp_statistic,
    bp_df,
    bp_p_value,
    4 / nrow(model_data),
    sum(cook_values > 4 / nrow(model_data)),
    max(cook_values),
    final_smearing$smearing_factor[final_smearing$model == "linear_log"]
  )
)

rf_importance_mse <- randomForest::importance(final_rf, type = 1, scale = TRUE)
rf_importance_purity <- randomForest::importance(final_rf, type = 2, scale = FALSE)
rf_importance <- data.frame(
  variabel = rownames(rf_importance_mse),
  peningkatan_mse_persen = as.numeric(rf_importance_mse[, 1L]),
  peningkatan_node_purity = as.numeric(rf_importance_purity[, 1L])
)
rf_importance <- rf_importance[
  order(rf_importance$peningkatan_mse_persen, decreasing = TRUE),
]
row.names(rf_importance) <- NULL

write_csv(cv_metrics, file.path(table_dir, "metrik_cv_per_fold.csv"))
write_csv(cv_summary, file.path(table_dir, "ringkasan_perbandingan_model.csv"))
write_csv(cv_predictions, file.path(table_dir, "prediksi_cross_validation.csv"))
write_csv(log_coefficients, file.path(table_dir, "koefisien_log_linear_hc3.csv"))
write_csv(vif_table, file.path(table_dir, "vif_log_linear.csv"))
write_csv(diagnostics, file.path(table_dir, "diagnostik_log_linear.csv"))
write_csv(rf_importance, file.path(table_dir, "kepentingan_variabel_random_forest.csv"))
write_csv(final_smearing, file.path(table_dir, "smearing_factor.csv"))

saveRDS(final_raw, file.path(model_dir, "model_linear_mentah.rds"))
saveRDS(final_log, file.path(model_dir, "model_log_linear.rds"))
saveRDS(final_spline, file.path(model_dir, "model_spline_log.rds"))
saveRDS(final_rf, file.path(model_dir, "model_random_forest_log.rds"))
saveRDS(
  list(
    random_seed = 20250916,
    folds = number_folds,
    repeats = number_repeats,
    model_ids = model_ids,
    formulas = list(
      linear_raw = raw_formula,
      linear_log = log_formula,
      spline_log = spline_formula,
      random_forest_log = rf_formula
    ),
    smearing_factors = final_smearing,
    training_ids = model_data$id_data,
    selected_by_mae = cv_summary$model[which.min(cv_summary$mae_mean)],
    selected_by_rmsle = cv_summary$model[which.min(cv_summary$rmsle_mean)]
  ),
  file.path(model_dir, "metadata_pemodelan.rds")
)

session_text <- capture.output(sessionInfo())
writeLines(session_text, file.path(model_dir, "session_info.txt"), useBytes = TRUE)

plot_cv_metrics <- function(metrics, path) {
  metrics$model_label <- factor(
    unname(model_labels[metrics$model]),
    levels = unname(model_labels[model_ids])
  )
  png(path, width = 1900, height = 1400, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(
    mfrow = c(2, 2),
    mar = c(9, 5, 3.5, 1.2),
    oma = c(0, 0, 3, 0),
    las = 1
  )
  colors <- c("#A0CBE8", "#FFBE7D", "#8CD17D", "#B6992D")

  boxplot(
    metrics$mae / 1e9 ~ metrics$model_label,
    col = colors,
    border = "#555555",
    las = 2,
    main = "MAE cross-validation",
    xlab = "",
    ylab = "Rp miliar"
  )
  boxplot(
    metrics$rmse / 1e9 ~ metrics$model_label,
    col = colors,
    border = "#555555",
    las = 2,
    main = "RMSE cross-validation",
    xlab = "",
    ylab = "Rp miliar"
  )
  boxplot(
    metrics$rmsle ~ metrics$model_label,
    log = "y",
    col = colors,
    border = "#555555",
    las = 2,
    main = "RMSLE cross-validation",
    xlab = "",
    ylab = "RMSLE (skala log)"
  )
  boxplot(
    metrics$r2 ~ metrics$model_label,
    col = colors,
    border = "#555555",
    las = 2,
    main = "R² cross-validation",
    xlab = "",
    ylab = "R²"
  )
  mtext(
    "Perbandingan Model — Repeated Stratified 10-Fold CV",
    outer = TRUE,
    line = 1,
    cex = 1.2,
    font = 2
  )
}

plot_actual_vs_prediction <- function(predictions, summary_table, path) {
  aggregated <- aggregate(
    cbind(aktual, prediksi) ~ id_data + model,
    data = predictions,
    FUN = mean
  )
  png(path, width = 1800, height = 1500, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(
    mfrow = c(2, 2),
    mar = c(5, 5, 4, 1.2),
    oma = c(0, 0, 3, 0),
    las = 1
  )

  colors <- c(
    linear_raw = "#4E79A7",
    linear_log = "#F28E2B",
    spline_log = "#59A14F",
    random_forest_log = "#B6992D"
  )
  # Batas grafik mengikuti rentang aktual agar prediksi negatif dari model
  # linear mentah (yang dibatasi menjadi Rp1 untuk evaluasi) tidak merusak
  # skala visual seluruh panel.
  limits <- range(aggregated$aktual)
  for (model_id in model_ids) {
    selected <- aggregated[aggregated$model == model_id, , drop = FALSE]
    metric <- summary_table[summary_table$model == model_id, , drop = FALSE]
    plot(
      selected$aktual / 1e9,
      selected$prediksi / 1e9,
      log = "xy",
      pch = 16,
      cex = 0.6,
      col = grDevices::adjustcolor(colors[[model_id]], alpha.f = 0.38),
      xlim = limits / 1e9,
      ylim = limits / 1e9,
      main = paste0(
        model_labels[[model_id]],
        "\nMAE = Rp", sprintf("%.2f", metric$mae_mean / 1e9), " miliar"
      ),
      xlab = "Harga aktual (Rp miliar, skala log)",
      ylab = "Prediksi CV rata-rata (Rp miliar, skala log)"
    )
    abline(a = 0, b = 1, lty = 2, lwd = 2, col = "#333333")
  }
  mtext(
    "Harga Aktual vs Prediksi Cross-Validation",
    outer = TRUE,
    line = 1,
    cex = 1.2,
    font = 2
  )
}

plot_log_diagnostics <- function(model, path) {
  png(path, width = 1800, height = 1400, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(
    mfrow = c(2, 2),
    mar = c(5, 5, 3.5, 1.2),
    oma = c(0, 0, 3, 0),
    las = 1
  )

  fitted_value <- fitted(model)
  residual_value <- residuals(model)
  standardized <- rstandard(model)
  cook <- cooks.distance(model)
  point_color <- grDevices::adjustcolor("#4E79A7", alpha.f = 0.4)

  plot(
    fitted_value,
    residual_value,
    pch = 16,
    cex = 0.6,
    col = point_color,
    xlab = "Fitted log(harga)",
    ylab = "Residual",
    main = "Residual vs fitted"
  )
  abline(h = 0, lty = 2, col = "#E15759", lwd = 2)

  qqnorm(
    standardized,
    pch = 16,
    cex = 0.6,
    col = point_color,
    main = "Normal Q-Q residual"
  )
  qqline(standardized, col = "#E15759", lwd = 2)

  plot(
    fitted_value,
    sqrt(abs(standardized)),
    pch = 16,
    cex = 0.6,
    col = point_color,
    xlab = "Fitted log(harga)",
    ylab = expression(sqrt("|standardized residual|")),
    main = "Scale-location"
  )

  plot(
    cook,
    type = "h",
    col = "#4E79A7",
    xlab = "Indeks observasi",
    ylab = "Cook's distance",
    main = "Observasi berpengaruh"
  )
  abline(h = 4 / length(cook), col = "#E15759", lty = 2, lwd = 2)
  mtext(
    "Diagnostik Regresi Log-Linear",
    outer = TRUE,
    line = 1,
    cex = 1.2,
    font = 2
  )
}

plot_rf_importance <- function(importance_table, path) {
  labels <- c(
    log_luas_tanah = "Log luas tanah",
    log_luas_bangunan = "Log luas bangunan",
    kamar_tidur = "Kamar tidur",
    kamar_mandi = "Kamar mandi",
    garasi = "Garasi"
  )
  ordered <- importance_table[
    order(importance_table$peningkatan_mse_persen),
    ,
    drop = FALSE
  ]
  display_labels <- unname(labels[ordered$variabel])

  png(path, width = 1300, height = 850, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mar = c(5, 12, 4, 2), las = 1)
  barplot(
    ordered$peningkatan_mse_persen,
    names.arg = display_labels,
    horiz = TRUE,
    col = "#59A14F",
    border = NA,
    xlab = "%IncMSE terstandar",
    main = "Kepentingan Variabel Random Forest"
  )
}

plot_cv_metrics(cv_metrics, file.path(plot_dir, "perbandingan_metrik_cv.png"))
plot_actual_vs_prediction(
  cv_predictions,
  cv_summary,
  file.path(plot_dir, "aktual_vs_prediksi_cv.png")
)
plot_log_diagnostics(final_log, file.path(plot_dir, "diagnostik_log_linear.png"))
plot_rf_importance(rf_importance, file.path(plot_dir, "kepentingan_variabel_rf.png"))

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

format_p_value <- function(x) {
  if (x < 0.001) "<0,001" else format_number_id(x, 3L)
}

coefficient_effect <- function(term) {
  row <- log_coefficients[log_coefficients$term == term, , drop = FALSE]
  row$effect_percent[[1L]]
}

coefficient_p <- function(term) {
  row <- log_coefficients[log_coefficients$term == term, , drop = FALSE]
  row$p_value[[1L]]
}

markdown_model_rows <- vapply(seq_len(nrow(cv_summary)), function(index) {
  row <- cv_summary[index, ]
  paste0(
    "| ", row$model_label,
    " | ", format_rupiah(row$mae_mean),
    " | ", format_rupiah(row$rmse_mean),
    " | ", format_number_id(row$rmsle_mean, 3L),
    " | ", format_number_id(row$r2_mean, 3L),
    " | ", format_number_id(row$mape_mean, 1L), "% |"
  )
}, character(1))

best_mae_id <- cv_summary$model[which.min(cv_summary$mae_mean)]
best_rmsle_id <- cv_summary$model[which.min(cv_summary$rmsle_mean)]
best_mae_row <- cv_summary[cv_summary$model == best_mae_id, , drop = FALSE]
best_rmsle_row <- cv_summary[cv_summary$model == best_rmsle_id, , drop = FALSE]

selection_text <- if (best_mae_id == best_rmsle_id) {
  paste0(
    "**", model_labels[[best_mae_id]],
    "** memberikan MAE dan RMSLE terbaik, sehingga menjadi pilihan utama untuk prediksi pada perbandingan ini."
  )
} else {
  paste0(
    "**", model_labels[[best_mae_id]], "** memiliki MAE terbaik, sedangkan **",
    model_labels[[best_rmsle_id]],
    "** memiliki RMSLE terbaik. Pilihan model bergantung pada apakah kesalahan rupiah atau kesalahan relatif lebih penting."
  )
}

report_lines <- c(
  "# Pemodelan Harga Rumah Jakarta Selatan",
  "",
  paste0(
    "Pemodelan menggunakan ", nrow(model_data),
    " observasi pada dataset utama Jakarta Selatan yang telah dibersihkan. Dataset listing tambahan tidak digabungkan karena cakupan sumbernya berbeda."
  ),
  "",
  "## Desain evaluasi",
  "",
  "- Target: `harga`.",
  "- Prediktor: luas tanah, luas bangunan, kamar tidur, kamar mandi, dan status garasi.",
  "- Validasi: repeated stratified 10-fold cross-validation, 5 pengulangan (50 hasil fold per model). Stratifikasi dilakukan berdasarkan urutan harga.",
  "- Semua pencilan tetap disertakan.",
  "- Model berbasis log dikembalikan ke skala rupiah menggunakan smearing correction.",
  "- MAE digunakan sebagai kriteria utama karena lebih mudah ditafsirkan dan tidak sedominan RMSE terhadap rumah supermewah. RMSLE digunakan untuk menilai kesalahan relatif.",
  "",
  "## Perbandingan model",
  "",
  "| Model | MAE | RMSE | RMSLE | R² | MAPE |",
  "|---|---:|---:|---:|---:|---:|",
  markdown_model_rows,
  "",
  selection_text,
  "",
  paste0(
    "Model dengan MAE terbaik menghasilkan MAE rata-rata **",
    format_rupiah(best_mae_row$mae_mean), "**, RMSE **",
    format_rupiah(best_mae_row$rmse_mean), "**, dan R² rata-rata **",
    format_number_id(best_mae_row$r2_mean, 3L), "** pada fold validasi."
  ),
  "",
  "![Perbandingan metrik cross-validation](plots/perbandingan_metrik_cv.png)",
  "",
  "![Harga aktual dan prediksi](plots/aktual_vs_prediksi_cv.png)",
  "",
  "## Interpretasi regresi log-linear",
  "",
  paste0(
    "- Dengan karakteristik lain tetap, kenaikan luas tanah 1% berkaitan dengan perubahan harga sekitar **",
    format_number_id(coefficient_effect("log_luas_tanah"), 3L),
    "%** (p robust ", format_p_value(coefficient_p("log_luas_tanah")), ")."
  ),
  paste0(
    "- Kenaikan luas bangunan 1% berkaitan dengan perubahan harga sekitar **",
    format_number_id(coefficient_effect("log_luas_bangunan"), 3L),
    "%** (p robust ", format_p_value(coefficient_p("log_luas_bangunan")), ")."
  ),
  paste0(
    "- Tambahan satu kamar tidur berkaitan dengan perubahan harga sekitar **",
    format_number_id(coefficient_effect("kamar_tidur"), 2L),
    "%**, setelah variabel lain dikendalikan (p robust ",
    format_p_value(coefficient_p("kamar_tidur")), ")."
  ),
  paste0(
    "- Tambahan satu kamar mandi berkaitan dengan perubahan harga sekitar **",
    format_number_id(coefficient_effect("kamar_mandi"), 2L),
    "%** (p robust ", format_p_value(coefficient_p("kamar_mandi")), ")."
  ),
  paste0(
    "- Rumah dengan garasi berkaitan dengan perbedaan harga sekitar **",
    format_number_id(coefficient_effect("garasiada"), 2L),
    "%** dibanding rumah tanpa garasi (p robust ",
    format_p_value(coefficient_p("garasiada")), ")."
  ),
  "",
  "Interpretasi koefisien menunjukkan asosiasi setelah mengendalikan prediktor yang tersedia, bukan hubungan sebab-akibat.",
  "",
  "## Diagnostik",
  "",
  paste0(
    "- VIF maksimum adalah **", format_number_id(max(vif_table$vif), 2L),
    "**, sehingga tidak terlihat multikolinearitas berat dengan ambang umum 5."
  ),
  paste0(
    "- Uji Breusch–Pagan menghasilkan p-value **", format_p_value(bp_p_value),
    "**. Karena itu, tabel koefisien menggunakan standard error HC3 yang tahan heteroskedastisitas."
  ),
  paste0(
    "- Terdapat **", sum(cook_values > 4 / nrow(model_data)),
    " observasi** dengan Cook's distance di atas 4/n. Observasi tersebut tidak dihapus."
  ),
  "",
  "![Diagnostik regresi log-linear](plots/diagnostik_log_linear.png)",
  "",
  "![Kepentingan variabel Random Forest](plots/kepentingan_variabel_rf.png)",
  "",
  "## Batas penggunaan",
  "",
  "- Prediksi hanya layak untuk profil rumah yang masih berada dalam cakupan data pelatihan.",
  "- Tidak tersedia kecamatan, koordinat, umur/kondisi bangunan, lebar jalan, atau tanggal observasi. Variabel yang tidak tersedia ini dapat menjelaskan bagian harga yang belum tertangkap model.",
  "- Nilai R² cross-validation per fold dapat berubah karena rumah sangat mahal mempunyai pengaruh besar pada kesalahan skala rupiah.",
  "- Harga pada data merupakan harga listing/dataset, bukan transaksi final yang telah diverifikasi.",
  "- Model Random Forest memerlukan paket R `randomForest` saat objek model akan digunakan kembali.",
  "",
  "Tabel rinci, prediksi cross-validation, model akhir, dan informasi sesi tersedia pada subfolder laporan ini."
)

writeLines(
  report_lines,
  file.path(report_dir, "laporan_pemodelan.md"),
  useBytes = TRUE
)

stopifnot(
  nrow(cv_metrics) == length(model_ids) * number_folds * number_repeats,
  nrow(cv_predictions) == nrow(model_data) * length(model_ids) * number_repeats,
  all(is.finite(cv_metrics$mae)),
  all(is.finite(cv_metrics$rmse)),
  all(is.finite(cv_metrics$rmsle)),
  all(cv_predictions$prediksi > 0),
  all(abs(log_coefficients$estimate - coef(final_log)) < 1e-12),
  file.exists(file.path(report_dir, "laporan_pemodelan.md")),
  length(list.files(plot_dir, pattern = "[.]png$")) == 4L
)

message("Pemodelan selesai.")
message("- Model terbaik berdasarkan MAE: ", model_labels[[best_mae_id]])
message("- Model terbaik berdasarkan RMSLE: ", model_labels[[best_rmsle_id]])
message("- Laporan: reports/pemodelan/laporan_pemodelan.md")
