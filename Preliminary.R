szn25 = read.csv("szn25.txt")
szn26 = read.csv("szn26.txt")
szn25Advanced = read.csv("szn25Advanced.txt")
szn26Advanced = read.csv("szn26Advanced.txt")

szn26$Rk = NULL
szn25$Rk = NULL

szn26Vars = szn26[1:min(nrow(szn26), 500), c("Age", "Pos", "PTS", "ORB", "AST")]
szn25Vars = szn25[1:min(nrow(szn25), 500), c("Age", "Pos", "PTS", "ORB", "AST")]
szn26BPM = data.frame(BPM = szn26Advanced[1:min(nrow(szn26Advanced), 500), c("BPM")])
szn25BPM = data.frame(BPM = szn25Advanced[1:min(nrow(szn25Advanced), 500), c("BPM")])


table = rbind(szn26Vars, szn25Vars)
BPMTable =  rbind(szn26BPM, szn25BPM)



if (nrow(table) == nrow(BPMTable)){
  table = cbind(table, BPMTable)  
}



model <- lm(BPM ~ Age + Pos + PTS + ORB + AST,data = table)

summary(model)
# Predict mpg for the new data
predicted_mpg <- predict(model, data.frame(Age=50, Pos="PG", PTS = 3, ORB = 1, AST =5))

