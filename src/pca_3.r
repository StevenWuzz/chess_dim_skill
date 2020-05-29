#PCA3, maybe see PCA and PCA2 first
#Looking at pca as related to user/tag matrix rather than user/problem matrix

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(reshape2)
library(nsprcomp)
library(cluster)


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

#
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))

chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate

problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL

#attatch tags to problems
chess_data = left_join(chess_data, problemData)

data_user_tags = data.frame(unique(chess_data$user_hash))
colnames(data_user_tags) = c("user_hash")

tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")
for(i in 1:length(tags)){
  tag = tags[i]
  tag_data = subset(chess_data, chess_data[[tag]] == TRUE)
  tag_data2 = tag_data %>% group_by(user_hash) %>% summarise(strength_tag = mean(point_diff), n = n())
  tag_data2$strength_tag = tag_data2$strength_tag / max(20 - tag_data2$n, 1)
  tag_data2$n = NULL
  colnames(tag_data2) = c("user_hash", tag)#, paste0("n_", tag))
  data_user_tags = left_join(data_user_tags, tag_data2)
}
data_user_tags[is.na(data_user_tags)] <- 0

data_user_tags = data_user_tags %>% remove_rownames %>% column_to_rownames(var="user_hash")
pca_results <- prcomp(data_user_tags, center = TRUE,scale. = TRUE)
pca_results_user_tag = pca_results

std_dev <- pca_results$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

scores = as.data.frame(pca_results$x) 
scores$user_hash = row.names(scores)
scores = left_join(scores, user_elo)
p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = elo)) + 
  geom_point(size=2) + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
  )
p 

data_user_tags$user_hash = row.names(data_user_tags)
scores = left_join(scores, data_user_tags)

for(i in 1:length(tags)){
  tag = tags[[i]]
  scores[[tag]] <- (scores[[tag]] - mean(scores[[tag]])) / sd(scores[[tag]])
}

form_ = paste(tags, collapse = "`+`")

form_1 = as.formula(paste0("PC1 ~ elo+`",form_, "`"))
model_pc1 = lm(data = scores, form_1)


x = kmeans(scores[,1:10], 20, iter.max = 100)
scores$clusters = as.factor(x$cluster)

p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = clusters)) + 
  geom_point(size=2) +   coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
  )
p 
####
g = scores[,22:ncol(scores)]
score_by_cluster = g %>% group_by(clusters) %>% summarise_all(mean)
g = score_by_cluster$clusters
score_by_cluster = data.matrix(score_by_cluster)
score_by_cluster = data.frame(score_by_cluster)
score_by_cluster = scale(score_by_cluster)

g2 = score_by_cluster
g2 = data.frame(cbind(g2, g))
g2$clusters = g2$g
g2$g = NULL

melted_score_by_cluster = melt(g2, id.vars = "clusters")
ggplot(data = melted_score_by_cluster, aes(x=clusters, y=variable, fill=value)) + 
  geom_tile() + scale_fill_gradient2(low = "red", high = "blue", mid = "white")

#####

y = pam(as.matrix(data_user_tags), k = 15)
y2 = data.frame(y$clustering)
colnames(y2) = c("cluster_tag_data")
y2$user_hash = row.names(y2)
scores = left_join(scores, y2)

temp = select(chess_data, user_hash, tactics_problem_id, is_passed)
temp = spread(temp, tactics_problem_id, is_passed)
temp[temp == 0] <- -1
temp[is.na(temp)] <- 0

temp = temp%>% column_to_rownames(var = "user_hash")
temp$user_hash = NULL

z = pam(as.matrix(temp), k = 15)
z2 = data.frame(z$clustering)
colnames(z2) = c("cluster_all_data")
z2$user_hash = row.names(z2)
scores = left_join(scores, z2)

scores$cluster_all_data = as.factor(scores$cluster_all_data)
p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = cluster_all_data)) + 
  geom_point(size=2) +   coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
  )
p 

scores$cluster_tag_data = as.factor(scores$cluster_tag_data)
p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = cluster_tag_data)) + 
  geom_point(size=2) +   coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
  )
p 