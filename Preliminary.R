seasons = c(19,24,25,26)

all_data = list()

for (i in seq_along(seasons)) {
  season = seasons[i]
  
  predict_source = read.csv(paste0("szn", season, ".txt"))
  target_source = read.csv(paste0("szn", season, "Advanced.txt"))
  
  # Rename column after reading
  colnames(predict_source)[colnames(predict_source) == "Player..additional"] = "player_id"
  colnames(target_source)[colnames(target_source) == "Player..additional"] = "player_id"
  predict_source$Rk = NULL
  
  # Filter out players with less than 10 games.
  predict_source = predict_source[predict_source$G >= 20, ]
  target_source = target_source[target_source$player_id %in% predict_source$player_id, ]
  
  # Avoid duplicate players within a season
  basic_unique = predict_source[!duplicated(predict_source$player_id, fromLast = TRUE), ]
  advanced_unique = target_source[!duplicated(target_source$player_id, fromLast = TRUE), ]
  
  vars = basic_unique[1:nrow(basic_unique), 
                      c("player_id", "Age", "Pos", "PTS", "ORB", "AST")]
  bpm = advanced_unique[1:nrow(advanced_unique), c("player_id", "BPM")]
  
  season_data = merge(vars, bpm, by = "player_id")
  all_data[[i]] = season_data
}

table = do.call(rbind, all_data)

model <- lm(BPM ~ Age + Pos + PTS + ORB + AST, data = table)
summary(model)

predicted_bpm <- predict(model, data.frame(Age=50, Pos="PG", PTS=3, ORB=1, AST=5))

# Combined diagnostics
png("diagnostic_plots_for_proposal.png", 
    width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model)
dev.off()
