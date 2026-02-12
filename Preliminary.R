seasons = c(25, 26)

all_vars = list()
all_bpm = list()

for (i in seq_along(seasons)) {
  season = seasons[i]
  
  predict_source = read.csv(paste0("szn", season, ".txt"))
  target_source = read.csv(paste0("szn", season, "Advanced.txt"))
  
  # Rename column after reading
  colnames(predict_source)[colnames(predict_source) == "Player..additional"] = "player_id"
  colnames(target_source)[colnames(target_source) == "Player..additional"] = "player_id"
  predict_source$Rk = NULL
  
  # Avoid duplicate players within a season
  basic_unique = predict_source[!duplicated(predict_source$player_id, fromLast = TRUE), ]
  advanced_unique = target_source[!duplicated(target_source$player_id, fromLast = TRUE), ]
  
  vars = basic_unique[1:min(nrow(basic_unique), 500), 
                      c("player_id", "Age", "Pos", "PTS", "ORB", "AST")]
  bpm = advanced_unique[1:min(nrow(advanced_unique), 500), c("player_id", "BPM")]
  
  all_vars[[i]] = vars
  all_bpm[[i]] = bpm
}

table = do.call(rbind, all_vars)
BPMTable = do.call(rbind, all_bpm)

if (nrow(table) == nrow(BPMTable)) {
  table = cbind(table, BPMTable)
}

model <- lm(BPM ~ Age + Pos + PTS + ORB + AST, data = table)
summary(model)

predicted_bpm <- predict(model, data.frame(Age=50, Pos="PG", PTS=3, ORB=1, AST=5))

par(mfrow = c(2, 2))
plot(model)
