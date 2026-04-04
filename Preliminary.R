# STA302 Final Project - Preliminary Analysis
# NBA Player Statistics vs Box Plus/Minus (BPM)
# Data: Basketball-Reference, 2024-25 and 2025-26 seasons

# ---- Data Loading -----------------------------------------------------------
seasons = c(25, 26)
all_vars = list()
all_bpm = list()

for (i in seq_along(seasons)) {
  season = seasons[i]
  
  predict_source = read.csv(paste0("szn", season, ".txt"))
  target_source = read.csv(paste0("szn", season, "Advanced.txt"))
  
  # Standardize player ID column name
  colnames(predict_source)[grep("additional", colnames(predict_source))] = "player_id"
  colnames(target_source)[grep("additional", colnames(target_source))] = "player_id"
  predict_source$Rk = NULL
  
  # Keep last entry per player to handle mid-season trades
  basic_unique = predict_source[!duplicated(predict_source$player_id, fromLast = TRUE), ]
  advanced_unique = target_source[!duplicated(target_source$player_id, fromLast = TRUE), ]
  
  # Take top 500 players and select relevant columns
  vars = basic_unique[1:min(nrow(basic_unique), 500),
                      c("player_id", "Age", "Pos", "PTS", "ORB", "AST", "STL", "BLK")]
  bpm = advanced_unique[1:min(nrow(advanced_unique), 500), c("player_id", "BPM")]
  
  # Tag each row with season so merge works correctly across two seasons
  vars$season = season
  bpm$season = season
  
  all_vars[[i]] = vars
  all_bpm[[i]] = bpm
}

# Combine seasons and merge basic stats with BPM
nba_data = do.call(rbind, all_vars)
BPMTable = do.call(rbind, all_bpm)
nba_data = merge(nba_data, BPMTable, by = c("player_id", "season"))
nba_data$BPM = as.numeric(nba_data$BPM)
nba_data = nba_data[!is.na(nba_data$BPM), ]

# ---- Transformations --------------------------------------------------------
# Fit baseline model to assess diagnostics before any transformation
model_raw = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL + BLK, data = nba_data)
png("diagnostic_plots_baseline.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_raw, main = "Baseline Model Diagnostics")
dev.off()

# Box-Cox requires strictly positive response, so shift BPM up
library(MASS)
shift = abs(min(nba_data$BPM)) + 1
nba_data$BPM_shifted = nba_data$BPM + shift
model_for_bc = lm(BPM_shifted ~ Age + Pos + PTS + ORB + AST + STL + BLK,
                  data = nba_data)

# Run Box-Cox to find optimal lambda
png("boxcox_plot.png", width = 8, height = 5, units = "in", res = 300)
b = boxcox(model_for_bc, lambda = seq(-3, 3, by = 0.1))
dev.off()
lambda = b$x[which.max(b$y)]
cat("Optimal Box-Cox lambda:", lambda, "\n")
lambda_rounded = round(lambda)
cat("Rounded lambda:", lambda_rounded, "\n")

# Apply Box-Cox transformation and fit transformed model
geom_mean = exp(mean(log(nba_data$BPM_shifted)))
nba_data$BPM_bc = geom_mean^(1 - lambda_rounded) *
  (nba_data$BPM_shifted^lambda_rounded - 1) / lambda_rounded
model_bc = lm(BPM_bc ~ Age + Pos + PTS + ORB + AST + STL + BLK, data = nba_data)

# Compare raw vs transformed - transformation improves AIC but diagnostics
# show violations persist, so untransformed model is retained
cat("\nAIC - raw vs Box-Cox:\n")
AIC(model_raw, model_bc)

png("diagnostic_plots_boxcox.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_bc, main = "Box-Cox Transformed Model Diagnostics")
dev.off()

# ---- Outlier Screening ------------------------------------------------------
# Compute all three influence diagnostics on the baseline model
cooks_d = cooks.distance(model_raw)
stud_resid = rstudent(model_raw)
leverage = hatvalues(model_raw)

# Dual criterion: must exceed both Cook's 4/n threshold AND studentized
# residual > 3 to be removed - guards against over-pruning
remove_idx = which(cooks_d > (4 / nrow(nba_data)) & abs(stud_resid) > 3)
nba_data_clean = nba_data[-remove_idx, ]
cat("Observations removed:", length(remove_idx), "\n")
cat("Remaining observations:", nrow(nba_data_clean), "\n")

# Cook's distance plot showing 4/n threshold
png("cooks_distance.png", width = 8, height = 5, units = "in", res = 300)
plot(cooks_d, type = "h",
     ylab = "Cook's Distance", xlab = "Observation Index",
     main = "Cook's Distance - Baseline Model")
abline(h = 4 / nrow(nba_data), col = "red", lty = 2)
legend("topright", legend = "4/n", col = "red", lty = 2, cex = 0.8)
dev.off()

# ---- Predictor Selection ----------------------------------------------------
# Full model with all seven predictors on cleaned data
model = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL + BLK, data = nba_data_clean)
summary(model)

# Test 1: Does removing STL worsen the model?
# STL is defensive so worth checking if it adds beyond offensive stats
model_noSTL = lm(BPM ~ Age + Pos + PTS + ORB + AST + BLK, data = nba_data_clean)
cat("\nAIC - STL removal test:\n")
AIC(model_noSTL, model)
cat("Adj R-squared without STL:", summary(model_noSTL)$adj.r.squared, "\n")
cat("Adj R-squared with STL:   ", summary(model)$adj.r.squared, "\n")
# Result: full model wins, STL retained

# Test 2: Does adding BLK improve on the six-predictor model?
# BLK theoretically captures interior defense not covered by STL
model_noBLK = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data_clean)
cat("\nAIC - BLK addition test:\n")
AIC(model_noBLK, model)
cat("BLK p-value:", summary(model)$coefficients["BLK", "Pr(>|t|)"], "\n")
cat("Adj R-squared without BLK:", summary(model_noBLK)$adj.r.squared, "\n")
cat("Adj R-squared with BLK:   ", summary(model)$adj.r.squared, "\n")
# Result: BLK significant and AIC improves, BLK retained

# Test 3: Does adding DRB further improve the model?
# DRB tested as a complementary defensive stat to BLK
model_DRB = lm(BPM ~ Age + Pos + PTS + ORB + DRB + AST + STL + BLK,
               data = nba_data_clean)
cat("\nAIC - DRB addition test:\n")
AIC(model, model_DRB)
cat("DRB p-value:", summary(model_DRB)$coefficients["DRB", "Pr(>|t|)"], "\n")
# Result: DRB not significant, AIC does not improve, DRB excluded

# Final model: BPM ~ Age + Pos + PTS + ORB + AST + STL + BLK

# ---- Diagnostic Plots - Final Model -----------------------------------------

png("diagnostic_plots_final.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model)
dev.off()

# ---- Summary Statistics for Report -----------------------------------------
# Verify correct model is being used
cat("\n=== MODEL VERIFICATION ===\n")
cat("Observations:", nrow(model$model), "\n")
cat("Predictors:", paste(names(coef(model)), collapse = ", "), "\n")

# Performance metrics
model_summary = summary(model)
cat("\nR-squared:         ", round(model_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
cat("AIC:               ", round(AIC(model), 3), "\n")
cat("BIC:               ", round(BIC(model), 3), "\n")

actual = nba_data_clean$BPM
pred = predict(model)
cat("MAE:               ", round(mean(abs(actual - pred)), 4), "\n")
cat("RMSE:              ", round(sqrt(mean((actual - pred)^2)), 4), "\n")

# Coefficient table for Section 5
coef_table = data.frame(
  Predictor = rownames(summary(model)$coefficients),
  Coefficient = round(summary(model)$coefficients[, "Estimate"], 4),
  Std_Error = round(summary(model)$coefficients[, "Std. Error"], 4),
  CI_Lower = round(confint(model)[, 1], 4),
  CI_Upper = round(confint(model)[, 2], 4),
  P_Value = round(summary(model)$coefficients[, "Pr(>|t|)"], 6)
)
write.csv(coef_table, "section5_coef_table.csv", row.names = FALSE)

# Predictor summary table for Section 2
vars_summary = nba_data_clean[, c("BPM", "Age", "PTS", "ORB", "AST", "STL", "BLK")]
for (var in colnames(vars_summary)) {
  cat(var, "\n")
  cat("  Mean:", round(mean(vars_summary[[var]], na.rm = TRUE), 2), "\n")
  cat("  SD:  ", round(sd(vars_summary[[var]], na.rm = TRUE), 2), "\n")
  cat("  Min: ", round(min(vars_summary[[var]], na.rm = TRUE), 2), "\n")
  cat("  Max: ", round(max(vars_summary[[var]], na.rm = TRUE), 2), "\n\n")
}
