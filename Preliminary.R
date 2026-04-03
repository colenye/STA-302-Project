seasons = c(25, 26)

all_vars = list()
all_bpm = list()

for (i in seq_along(seasons)) {
  season = seasons[i]
  
  predict_source = read.csv(paste0("szn", season, ".txt"))
  target_source = read.csv(paste0("szn", season, "Advanced.txt"))
  
  # Rename player ID column (exact name varies by file)
  colnames(predict_source)[grep("additional", colnames(predict_source))] = "player_id"
  colnames(target_source)[grep("additional", colnames(target_source))] = "player_id"
  predict_source$Rk = NULL
  
  # Avoid duplicate players within a season
  basic_unique = predict_source[!duplicated(predict_source$player_id, fromLast = TRUE), ]
  advanced_unique = target_source[!duplicated(target_source$player_id, fromLast = TRUE), ]
  
  # Added STL and DRB to predictors (DRB needed to test and exclude)
  vars = basic_unique[1:min(nrow(basic_unique), 500),
                      c("player_id", "Age", "Pos", "PTS", "ORB", "AST", "STL", "DRB")]
  bpm = advanced_unique[1:min(nrow(advanced_unique), 500), c("player_id", "BPM")]
  
  all_vars[[i]] = vars
  all_bpm[[i]] = bpm
}

nba_data = do.call(rbind, all_vars)
BPMTable = do.call(rbind, all_bpm)

# Merge by player_id to ensure correct matching
nba_data = merge(nba_data, BPMTable, by = "player_id")
nba_data$BPM = as.numeric(nba_data$BPM)
nba_data = nba_data[!is.na(nba_data$BPM), ]


# ---- Transformations --------------------------------------------------------

# Step 1: Fit baseline model first
model_raw = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data)

# Step 2: Inspect diagnostic plots BEFORE deciding on transformations
# Look for: non-linearity (residuals vs fitted), heteroscedasticity (scale-location),
# and non-normality (QQ plot)
png("diagnostic_plots_baseline.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_raw, main = "Baseline Model Diagnostics")
dev.off()

# Step 3: Test log1p transforms on predictors to address any non-linearity
# PTS, AST, and ORB are right-skewed
nba_data$logPTS = log1p(nba_data$PTS)
nba_data$logAST = log1p(nba_data$AST)
nba_data$logORB = log1p(nba_data$ORB)

model_log = lm(BPM ~ Age + Pos + logPTS + logORB + logAST + STL, data = nba_data)

# Compare AIC - lower is better
AIC(model_raw, model_log)
# Log model offers no meaningful improvement over raw units for predictors

# Step 4: Box-Cox transformation on the response (BPM) to address
# non-normality and heteroscedasticity seen in the diagnostic plots.
# Box-Cox requires strictly positive values, so we shift BPM first.
library(MASS)

shift <- abs(min(nba_data$BPM)) + 1
nba_data$BPM_shifted <- nba_data$BPM + shift

model_for_bc = lm(BPM_shifted ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data)

# Find optimal lambda via maximum likelihood
png("boxcox_plot.png", width = 8, height = 5, units = "in", res = 300)
b <- boxcox(model_for_bc, lambda = seq(-3, 3, by = 0.1))
dev.off()

lambda <- b$x[which.max(b$y)]
cat("Optimal Box-Cox lambda:", lambda, "\n")

# Step 5: Apply Box-Cox transformation using the optimal lambda.
# prod() overflows to Inf with ~1800 observations, so we compute the
# geometric mean via the log-sum-exp trick instead: exp(mean(log(y))).
# Since lambda ≈ 1.9 ≈ 2, we round to lambda = 2 (a square transformation)
# for numerical stability and interpretability — lambda = 2 is a standard
# power transform and is consistent with the 95% CI on the Box-Cox plot.
lambda_rounded <- round(lambda)
cat("Rounded lambda used for transformation:", lambda_rounded, "\n")

geom_mean <- exp(mean(log(nba_data$BPM_shifted)))
nba_data$BPM_bc <- geom_mean^(1 - lambda_rounded) *
  (nba_data$BPM_shifted^lambda_rounded - 1) / lambda_rounded

# Step 6: Fit model on Box-Cox transformed response
model_bc = lm(BPM_bc ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data)

# Step 7: Compare AIC across all three models - lower is better
cat("\nAIC Comparison:\n")
AIC(model_raw, model_log, model_bc)

# Note: coefficients from model_bc are on the transformed scale and cannot
# be interpreted directly in original BPM units


# ---- Outlier Screening ------------------------------------------------------

# Outlier screening is based on the raw model (untransformed BPM)
# to keep Cook's D, leverage, and studentized residuals on the original scale
cooks_d = cooks.distance(model_raw)
stud_resid = rstudent(model_raw)
leverage = hatvalues(model_raw)

# Only remove observations that are flagged as extreme on both
# Cook's D and studentized residuals simultaneously
remove_idx = which(cooks_d > (16 / nrow(nba_data)) & abs(stud_resid) > 3)
nba_data_clean = nba_data[-remove_idx, ]

cat("Observations removed:", length(remove_idx), "\n")
cat("Remaining observations:", nrow(nba_data_clean), "\n")


# ---- Predictor Selection ----------------------------------------------------

# Final model on cleaned data
model = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data_clean)
summary(model)

# Compare against model without STL to justify its inclusion
model_noSTL = lm(BPM ~ Age + Pos + PTS + ORB + AST, data = nba_data_clean)

AIC(model_noSTL, model)
summary(model_noSTL)$adj.r.squared
summary(model)$adj.r.squared

# DRB was also tested and excluded - p-value was 0.971
model_DRB = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL + DRB, data = nba_data_clean)
summary(model_DRB)$coefficients["DRB", ]


# ---- Diagnostic Plots -------------------------------------------------------

# Cook's distance on baseline (pre-cleaning) model
png("cooks_distance.png", width = 8, height = 5, units = "in", res = 300)
plot(cooks_d, type = "h",
     ylab = "Cook's Distance", xlab = "Observation Index",
     main = "Cook's Distance - Baseline Model")
abline(h = 4 / nrow(nba_data), col = "red", lty = 2)
abline(h = 16 / nrow(nba_data), col = "orange", lty = 2)
legend("topright", legend = c("4/n", "16/n"),
       col = c("red", "orange"), lty = 2, cex = 0.8)
dev.off()

# Standard diagnostic plots for final model (raw BPM)
png("diagnostic_plots_final.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model)
dev.off()

# Diagnostic plots for Box-Cox transformed model - compare against baseline
# to assess whether the transformation improved normality and homoscedasticity
png("diagnostic_plots_boxcox.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_bc, main = "Box-Cox Transformed Model Diagnostics")
dev.off()
