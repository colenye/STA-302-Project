seasons = c(25, 26)
all_vars = list()
all_bpm = list()
for (i in seq_along(seasons)) {
  season = seasons[i]
  
  predict_source = read.csv(paste0("szn", season, ".txt"))
  target_source = read.csv(paste0("szn", season, "Advanced.txt"))
  
  colnames(predict_source)[grep("additional", colnames(predict_source))] = "player_id"
  colnames(target_source)[grep("additional", colnames(target_source))] = "player_id"
  predict_source$Rk = NULL
  
  basic_unique = predict_source[!duplicated(predict_source$player_id, fromLast = TRUE), ]
  advanced_unique = target_source[!duplicated(target_source$player_id, fromLast = TRUE), ]
  
  vars = basic_unique[1:min(nrow(basic_unique), 500),
                      c("player_id", "Age", "Pos", "PTS", "ORB", "AST", "STL", "BLK")]
  bpm = advanced_unique[1:min(nrow(advanced_unique), 500), c("player_id", "BPM")]
  
  vars$season = season
  bpm$season = season
  
  all_vars[[i]] = vars
  all_bpm[[i]] = bpm
}
nba_data = do.call(rbind, all_vars)
BPMTable = do.call(rbind, all_bpm)
nba_data = merge(nba_data, BPMTable, by = c("player_id", "season"))
nba_data$BPM = as.numeric(nba_data$BPM)
nba_data = nba_data[!is.na(nba_data$BPM), ]

# ---- Transformations --------------------------------------------------------
model_raw = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL + BLK, data = nba_data)
png("diagnostic_plots_baseline.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_raw, main = "Baseline Model Diagnostics")
dev.off()

library(MASS)
shift <- abs(min(nba_data$BPM)) + 1
nba_data$BPM_shifted <- nba_data$BPM + shift
model_for_bc = lm(BPM_shifted ~ Age + Pos + PTS + ORB + AST + STL + BLK,
                  data = nba_data)
png("boxcox_plot.png", width = 8, height = 5, units = "in", res = 300)
b <- boxcox(model_for_bc, lambda = seq(-3, 3, by = 0.1))
dev.off()
lambda <- b$x[which.max(b$y)]
cat("Optimal Box-Cox lambda:", lambda, "\n")
lambda_rounded <- round(lambda)
cat("Rounded lambda used for transformation:", lambda_rounded, "\n")
geom_mean <- exp(mean(log(nba_data$BPM_shifted)))
nba_data$BPM_bc <- geom_mean^(1 - lambda_rounded) *
  (nba_data$BPM_shifted^lambda_rounded - 1) / lambda_rounded
model_bc = lm(BPM_bc ~ Age + Pos + PTS + ORB + AST + STL + BLK, data = nba_data)
cat("\nAIC Comparison (raw vs Box-Cox):\n")
AIC(model_raw, model_bc)

# ---- Outlier Screening ------------------------------------------------------
cooks_d = cooks.distance(model_raw)
stud_resid = rstudent(model_raw)
leverage = hatvalues(model_raw)
remove_idx = which(cooks_d > (4 / nrow(nba_data)) & abs(stud_resid) > 3)
nba_data_clean = nba_data[-remove_idx, ]
cat("Observations removed:", length(remove_idx), "\n")
cat("Remaining observations:", nrow(nba_data_clean), "\n")

# ---- Predictor Selection ----------------------------------------------------
model = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL + BLK, data = nba_data_clean)
summary(model)

# Test whether removing STL worsens model
model_noSTL = lm(BPM ~ Age + Pos + PTS + ORB + AST + BLK, data = nba_data_clean)
AIC(model_noSTL, model)
summary(model_noSTL)$adj.r.squared
summary(model)$adj.r.squared

# ---- Diagnostic Plots -------------------------------------------------------
png("cooks_distance.png", width = 8, height = 5, units = "in", res = 300)
plot(cooks_d, type = "h",
     ylab = "Cook's Distance", xlab = "Observation Index",
     main = "Cook's Distance - Baseline Model")
abline(h = 4 / nrow(nba_data), col = "red", lty = 2)
legend("topright", legend = "4/n", col = "red", lty = 2, cex = 0.8)
dev.off()

png("diagnostic_plots_baseline.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_raw, main = "Baseline Model Diagnostics")
dev.off()

png("diagnostic_plots_final.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model)
dev.off()

png("diagnostic_plots_boxcox.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_bc, main = "Box-Cox Transformed Model Diagnostics")
dev.off()

# ---- table for coefficients, standard errors, confidence intervals and p-value --

# Extract coefficient summary
coef_summary <- summary(model)$coefficients

# Compute 95% confidence intervals
conf_int <- confint(model, level = 0.95)

# Combine into one table
final_table <- data.frame(
  Predictor = rownames(coef_summary),
  Coefficient = coef_summary[, "Estimate"],
  Std_Error = coef_summary[, "Std. Error"],
  CI_Lower = conf_int[, 1],
  CI_Upper = conf_int[, 2],
  P_Value = coef_summary[, "Pr(>|t|)"]
)

# Print table
print(final_table)

write.csv(final_table, "final_model_summary_table.csv", row.names = FALSE)

# ---- performance metrics ---------------------------------------------

model_summary <- summary(model)

# R-squared
r_squared <- model_summary$r.squared

# Adjusted R-squared
adj_r_squared <- model_summary$adj.r.squared

# AIC
aic_value <- AIC(model)

# BIC
bic_value <- BIC(model)

performance_metrics <- data.frame(
  Metric = c("R-squared", "Adjusted R-squared", "AIC", "BIC"),
  Value = c(r_squared, adj_r_squared, aic_value, bic_value)
)

print(performance_metrics)

# Actual observed BPM values
actual <- nba_data_clean$BPM

# Predicted BPM values from the model
pred <- predict(model)

# Mean Absolute Error (MAE)
MAE <- mean(abs(actual - pred))

# Mean Squared Error (MSE)
MSE <- mean((actual - pred)^2)

# Root Mean Squared Error (RMSE)
RMSE <- sqrt(MSE)

# Median Absolute Error (MedAE)
MedAE <- median(abs(actual - pred))

# Mean Absolute Percentage Error (MAPE)
MAPE <- mean(abs((actual - pred) / actual)) * 100

# Root Mean Squared Log Error (RMSLE)
# Requires positive values, so shift if necessary
shift_log <- abs(min(actual, pred)) + 1
RMSLE <- sqrt(mean((log(pred + shift_log) - log(actual + shift_log))^2))

# Print results
cat("\nModel Performance Metrics:\n")
cat("MAE:", MAE, "\n")
cat("MSE:", MSE, "\n")
cat("RMSE:", RMSE, "\n")
cat("Median Absolute Error:", MedAE, "\n")
cat("MAPE:", MAPE, "%\n")
cat("RMSLE:", RMSLE, "\n")

var(bpm)
sqrt(var(bpm))
