#initial PCA code
#pca2 and pca3 have more PCA code with more elaborate analysis
#creates a matrix of user/problem successes, failure = -1, success = 1, na = 0
#runs PCA on it, runs regression based on result

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(reshape2)
library(nsprcomp)

chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))

games_played = chess_data%>% group_by(user_hash) %>% summarise(userGamesPlayed = last(userGamesPlayed))
user_elo = chess_data %>% group_by(user_hash) %>% summarise(elo = last(ratingUser))
user_last_game_date = chess_data %>% group_by(user_hash) %>% summarise(last_game = last(date))
user_first_game_date = chess_data %>% group_by(user_hash) %>% summarise(first_game = first(date))

#7500 games -> 1450 players
n = 7500

games_played = subset(games_played, games_played$userGamesPlayed > n)
games_played$enough_games = 1
games_played$userGamesPlayed = NULL
chess_data = left_join(chess_data, games_played)
chess_data = left_join(chess_data, user_elo)
chess_data = subset(chess_data, chess_data$enough_games == 1)
chess_data$enough_games = NULL
#chess_data = subset(chess_data, chess_data$elo > 1950)
#chess_data = subset(chess_data, chess_data$elo < 2000)




games_played = chess_data %>% group_by(tactics_problem_id) %>% summarise(problemGamesPlayed = n())
#1050 attempts -> 1515 problems if n = 7500
#700 attempts -> 1615 problems if elo range of 1950-2000

m = 1050


games_played = subset(games_played, games_played$problemGamesPlayed > m)
games_played$enough_games = 1
games_played$problemGamesPlayed = NULL
chess_data = left_join(chess_data, games_played)
chess_data = subset(chess_data, chess_data$enough_games == 1)
chess_data$enough_games = NULL

##### filling in dataset
temp = select(chess_data, user_hash, tactics_problem_id, is_passed)
temp = spread(temp, tactics_problem_id, is_passed)
temp2 = melt(data = temp)
colnames(temp2) = c("user_hash", "tactics_problem_id", "is_passed")

temp3 = select(chess_data, user_hash, ratingUser)
temp3 = temp3 %>% group_by(user_hash) %>% summarise(ratingUser = last(ratingUser))
temp2 = left_join(temp2, temp3)

chess_data = ungroup(chess_data)

temp3 = select(chess_data, tactics_problem_id, ratingProblem)
temp3 = temp3 %>% group_by(tactics_problem_id) %>% summarise(ratingProblem = last(ratingProblem))
temp3$tactics_problem_id = as.factor(temp3$tactics_problem_id)
temp2 = left_join(temp2, temp3)

temp2$ratingDiff=temp2$ratingProblem - temp2$ratingUser
temp2$predicted_success_rate = 1/(1 + 10^((temp2$ratingDiff)/400))

temp3 = temp2
temp3$is_passed[temp3$is_passed == 0] <--1
temp3$is_passed[is.na(temp3$is_passed)] <- 0
temp = select(temp3, user_hash, tactics_problem_id, is_passed)


#temp2$is_passed[is.na(temp2$is_passed)] <- temp2$predicted_success_rate[is.na(temp2$is_passed)]
#temp = select(temp2, user_hash, tactics_problem_id, is_passed)

temp = spread(temp, tactics_problem_id, is_passed)
#####



#temp[temp==0]<--1
#temp[is.na(temp)]<-0

temp = temp %>% remove_rownames %>% column_to_rownames(var="user_hash")
pca_results <- prcomp(temp, center = TRUE,scale. = TRUE)
#pca_results <- nscumcomp(temp, retx = TRUE, center = TRUE, scale. = TRUE)

std_dev <- pca_results$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b",  xlim = c(1, 20))

scores = as.data.frame(pca_results$x) 
scores$user_hash = row.names(scores)
scores = left_join(scores, user_elo)
p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = elo)) + 
  geom_point(size=2) + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
)
p 

scores$user_hash = row.names(scores)
scores = left_join(scores, user_last_game_date)  
scores = left_join(scores, user_first_game_date)  
scores$last_game2 = as.numeric(scores$last_game - min(scores$last_game))
scores$first_game2 = as.numeric(scores$first_game - min(scores$first_game))

###
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))

chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate

problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL

#attatch tags to problems
chess_data = left_join(chess_data, problemData)

####
tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

data_user_tags = data.frame(scores$user_hash)
colnames(data_user_tags) = c("user_hash")

for(i in 1:length(tags)){
  tag = tags[i]
  tag_data = subset(chess_data, chess_data[[tag]] == TRUE)
  tag_data2 = tag_data %>% group_by(user_hash) %>% summarise(strength_tag = mean(point_diff), n = n())
  colnames(tag_data2) = c("user_hash", tag, paste0("n_", tag))
  data_user_tags = left_join(data_user_tags, tag_data2)
}
data_user_tags[is.na(data_user_tags)] <- 0
scores = left_join(scores, data_user_tags)

form_ = paste(tags, collapse = "`+`")

form_1 = as.formula(paste0("PC1 ~ elo+`",form_, "`"))
model_pc1 = lm(data = scores, form_1)
form_2 = as.formula(paste0("PC2 ~ elo+`",form_, "`"))
model_pc2 = lm(data = scores, form_2)
