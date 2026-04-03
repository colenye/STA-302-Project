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
                      c("player_id", "Age", "Pos", "PTS", "ORB", "AST", "STL")]
  bpm = advanced_unique[1:min(nrow(advanced_unique), 500), c("player_id", "BPM")]
  
  all_vars[[i]] = vars
  all_bpm[[i]] = bpm
}

nba_data = do.call(rbind, all_vars)
BPMTable = do.call(rbind, all_bpm)

nba_data = merge(nba_data, BPMTable, by = "player_id")
nba_data$BPM = as.numeric(nba_data$BPM)
nba_data = nba_data[!is.na(nba_data$BPM), ]


# ---- Transformations --------------------------------------------------------

model_raw = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data)

png("diagnostic_plots_baseline.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_raw, main = "Baseline Model Diagnostics")
dev.off()

library(MASS)

shift <- abs(min(nba_data$BPM)) + 1
nba_data$BPM_shifted <- nba_data$BPM + shift

model_for_bc = lm(BPM_shifted ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data)

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

model_bc = lm(BPM_bc ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data)

cat("\nAIC Comparison:\n")
AIC(model_raw, model_bc)


# ---- Outlier Screening ------------------------------------------------------

cooks_d = cooks.distance(model_raw)
stud_resid = rstudent(model_raw)
leverage = hatvalues(model_raw)

remove_idx = which(cooks_d > (16 / nrow(nba_data)) & abs(stud_resid) > 3)
nba_data_clean = nba_data[-remove_idx, ]

cat("Observations removed:", length(remove_idx), "\n")
cat("Remaining observations:", nrow(nba_data_clean), "\n")


# ---- Predictor Selection ----------------------------------------------------

model = lm(BPM ~ Age + Pos + PTS + ORB + AST + STL, data = nba_data_clean)
summary(model)

model_noSTL = lm(BPM ~ Age + Pos + PTS + ORB + AST, data = nba_data_clean)

AIC(model_noSTL, model)
summary(model_noSTL)$adj.r.squared
summary(model)$adj.r.squared


# ---- Diagnostic Plots -------------------------------------------------------

png("cooks_distance.png", width = 8, height = 5, units = "in", res = 300)
plot(cooks_d, type = "h",
     ylab = "Cook's Distance", xlab = "Observation Index",
     main = "Cook's Distance - Baseline Model")
abline(h = 4 / nrow(nba_data), col = "red", lty = 2)
abline(h = 16 / nrow(nba_data), col = "orange", lty = 2)
legend("topright", legend = c("4/n", "16/n"),
       col = c("red", "orange"), lty = 2, cex = 0.8)
dev.off()

png("diagnostic_plots_final.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model)
dev.off()

png("diagnostic_plots_boxcox.png", width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model_bc, main = "Box-Cox Transformed Model Diagnostics")
dev.off()
