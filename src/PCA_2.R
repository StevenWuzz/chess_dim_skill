#See PCA and PC3 for more pca code
#firstly, PCA identitical problem verification
#runs PCA on the dataset of identitical problems to ensure PCA is better than random
#Finally, we create our own tags and test their contibution to model accuracy

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(reshape2)
library(nsprcomp)
library(gbm)

chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")
chess_data_backup = chess_data


chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))

games_played = chess_data%>% group_by(user_hash) %>% summarise(userGamesPlayed = last(userGamesPlayed))
user_elo = chess_data %>% group_by(user_hash) %>% summarise(elo = last(ratingUser))
problem_elo = chess_data %>% group_by(tactics_problem_id) %>% summarise(elo = last(ratingProblem))
problem_creation = chess_data %>% group_by(tactics_problem_id) %>% summarise(first_attempt = first(date))


##
duplicate_data = read_csv("/w/225/1/chess/tactics/duplicate_FENs.csv")

dd2 = select(duplicate_data, ID1)
dd2$id = 1:nrow(dd2)
colnames(dd2) = c("tactics_problem_id", "unique_combo_id")
chess_data = left_join(chess_data, dd2)


dd2 = select(duplicate_data, ID2)
dd2$id = 1:nrow(dd2)
colnames(dd2) = c("tactics_problem_id", "unique_combo_id2")
chess_data = left_join(chess_data, dd2)

chess_data$unique_combo_id[is.na(chess_data$unique_combo_id)] <- chess_data$unique_combo_id2[is.na(chess_data$unique_combo_id)]
cd2 = subset(chess_data, !is.na(chess_data$unique_combo_id))
##cd2 is a subset of chess_data with all the combos exlusively
duplicates = unique(cd2$tactics_problem_id)
duplicates =  data.frame(duplicates, 0)

most_common_users = cd2 %>% group_by(user_hash) %>% summarise(n = n())
#users_to_consider = subset(most_common_users, most_common_users$n > 30) #aprx 15000 users
#users_to_consider = subset(most_common_users, most_common_users$n > 50) #aprx 3000 users
users_to_consider = subset(most_common_users, most_common_users$n > 68) #aprx 1500 users
#users_to_consider = subset(most_common_users, most_common_users$n > 75) #smaller for sgd


temp = left_join(chess_data, users_to_consider)
temp = subset(temp, !is.na(temp$n))
most_common_problems = temp %>% group_by(tactics_problem_id) %>% summarise(n2 = n())

#most_common_problems = subset(most_common_problems, most_common_problems$n2 > 1750) #aprx 14000 users
#most_common_problems = subset(most_common_problems, most_common_problems$n2 > 2200) #aprx 5000 users
most_common_problems = subset(most_common_problems, most_common_problems$n2 > 2600) #aprx 1500 users
#most_common_problems = subset(most_common_problems, most_common_problems$n2 > 2200) #for sgd



colnames(duplicates) = c("tactics_problem_id", "n2")
most_common_problems = rbind(most_common_problems, duplicates)
most_common_problems = most_common_problems %>% group_by(tactics_problem_id) %>% summarise(n2 = first(n2))
temp = left_join(temp, most_common_problems)
temp = subset(temp, !is.na(temp$n2))
###




#n = 7500

#games_played = subset(games_played, games_played$userGamesPlayed > n)
#games_played$enough_games = 1
#games_played$userGamesPlayed = NULL
#chess_data = left_join(chess_data, games_played)
#chess_data = left_join(chess_data, user_elo)
#chess_data = subset(chess_data, chess_data$enough_games == 1)
#chess_data$enough_games = NULL
#chess_data = subset(chess_data, chess_data$elo > 1950)
#chess_data = subset(chess_data, chess_data$elo < 2000)




#games_played = chess_data %>% group_by(tactics_problem_id) %>% summarise(problemGamesPlayed = n())
#1050 attempts -> 1515 problems if n = 7500
#700 attempts -> 1615 problems if elo range of 1950-2000

# m = 1050
# 
# 
# games_played = subset(games_played, games_played$problemGamesPlayed > m)
# games_played$enough_games = 1
# games_played$problemGamesPlayed = NULL
# chess_data = left_join(chess_data, games_played)
# chess_data = subset(chess_data, chess_data$enough_games == 1)
# chess_data$enough_games = NULL
# 
# temp = select(chess_data, user_hash, tactics_problem_id, is_passed)

##### filling in dataset
temp = select(temp, user_hash, tactics_problem_id, is_passed)

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

##
temp_backup = temp2

##
temp3 = temp2
temp3$is_passed[temp3$is_passed == 0] <--1
temp3$is_passed[is.na(temp3$is_passed)] <- 0
temp = select(temp3, user_hash, tactics_problem_id, is_passed)

##
#library(sgd)
#sgd.data <- sgd(is_passed ~ tactics_problem_id + user_hash, model="glm", data = temp)

sgd.data <- gbm(is_passed ~ tactics_problem_id + user_hash, distribution = "pairwise", data = temp)

##

#temp2$is_passed[is.na(temp2$is_passed)] <- temp2$
predicted_success_rate[is.na(temp2$is_passed)]
#temp = select(temp2, user_hash, tactics_problem_id, is_passed)



###
###

# 
#temp = select(temp, user_hash, tactics_problem_id, is_passed)
# temp = spread(temp, tactics_problem_id, is_passed)
# temp2 = melt(data = temp)
# colnames(temp2) = c("user_hash", "tactics_problem_id", "is_passed")
# 
# temp3 = select(chess_data, user_hash, ratingUser)
# temp3 = temp3 %>% group_by(user_hash) %>% summarise(ratingUser = last(ratingUser))
# temp2 = left_join(temp2, temp3)
# 
# chess_data = ungroup(chess_data)
# 
# temp3 = select(chess_data, tactics_problem_id, ratingProblem)
# temp3 = temp3 %>% group_by(tactics_problem_id) %>% summarise(ratingProblem = last(ratingProblem))
# temp3$tactics_problem_id = as.factor(temp3$tactics_problem_id)
# temp2 = left_join(temp2, temp3)
# 
# temp2$ratingDiff=temp2$ratingProblem - temp2$ratingUser
# temp2$predicted_success_rate = 1/(1 + 10^((temp2$ratingDiff)/400))
# temp2$is_passed[is.na(temp2$is_passed)] <- temp2$predicted_success_rate[is.na(temp2$is_passed)]
# 
# 
# temp = select(temp2, user_hash, tactics_problem_id, is_passed)
print(sum(temp$is_passed > .99) + sum(temp$is_passed <.01))/nrow(temp)
print(sum(temp$is_passed == 0)/nrow(temp))
#temp$is_passed[temp$is_passed == 0] <- -1
#temp$is_passed[is.na(temp$is_passed)] <- 0


#pca on a problem level
temp_n = spread(temp, user_hash, is_passed)

temp_n = temp_n %>% remove_rownames %>% column_to_rownames(var="tactics_problem_id")

temp_n = temp_n[ , apply(temp_n, 2, var) != 0]

pca_results <- prcomp(temp_n, center = TRUE,scale. = TRUE)
#pca_results <- nscumcomp(temp, retx = TRUE, center = TRUE, scale. = TRUE)
pca_results_by_problem = pca_results

std_dev <- pca_results$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b", xlim = c(1, 20))

scores = as.data.frame(pca_results$x)[1:10]
scores$tactics_problem_id = row.names(scores)
problem_elo$tactics_problem_id = as.character(problem_elo$tactics_problem_id)
scores = left_join(scores, problem_elo)
p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = elo)) + 
  geom_point(size=2) + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
  )
p 
p2 <- ggplot(data = scores, aes(x = PC1, y = elo)) + 
  geom_point(size=2) + 
  scale_colour_gradientn(colours = terrain.colors(10)) 
p2 


##here's where we check the duplicates##
get_distance <- function(row1, row2){
  return(sum((scores[row1,2:10] - scores[row2,2:10])**2))
}
scores_row = scores %>% select(tactics_problem_id)
scores_row$row = rownames(scores_row)
duplicate_data$tactics_problem_id = as.character(duplicate_data$ID1)
duplicate_data = left_join(duplicate_data, scores_row)
duplicate_data$tactics_problem_id = as.character(duplicate_data$ID2)
scores_row$row2 = rownames(scores_row)
scores_row$row = NULL
duplicate_data = left_join(duplicate_data, scores_row)
duplicate_data$tactics_problem_id = NULL
duplicate_data$distance = NA
for(i in 1:nrow(duplicate_data)){
  duplicate_data$distance[i] = get_distance(duplicate_data$row[i], duplicate_data$row2[i])
}
duplicate_data = duplicate_data[complete.cases(duplicate_data), ]
dist = mean(duplicate_data$distance)

dd2 = duplicate_data %>% select(ID1, row)
dd3 = duplicate_data %>% select(ID2, row2)
dd2 <- dd2[sample(nrow(dd2)),]
dd3 = cbind(dd2, dd3)
dd3$distance = NA
for(i in 1:nrow(dd3)){
  dd3$distance[i] = get_distance(dd3$row[i], dd3$row2[i])
}
dist_control = mean(dd3$distance)
print(c(dist, dist_control, dist_control-dist))


scores_backup = scores
duplicate_data_backup = duplicate_data
##here's where we cluster the problems based on first 10 PCA
scores = scores_backup
duplicate_data = duplicate_data_backup


scores2 = select(scores, "tactics_problem_id", "PC1","PC2","PC3","PC4","PC5","PC6", "PC7","PC8","PC9","PC10" )

#x = kmeans(scores2[,2:11], 150, iter.max = 100)
library(lsa)

c = scores2[,2:11]
row.names(c) = scores2$tactics_problem_id
c = as.matrix(c)
c = t(c)
c = cosine(c)
c[is.na(c)] <- 0
c = 1-c
c = as.dist(c)
x1 = hclust(c)
x = data.frame(cutree(x1,20)) ########
colnames(x) = c("clusters")
x$tactics_problem_id = row.names(x)

scores = left_join(scores, x)##start from here to check cluster/tag groupings
#scores$clusters = as.factor(x$cluster)

###
problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL
scores$tactics_problem_id = as.numeric(scores$tactics_problem_id)
scores = left_join(scores, problemData)
row.names(scores) = scores$tactics_problem_id
Toremove = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","tactics_problem_id","elo","average_seconds","move_count")

scores_t = scores %>% select(.dots = -Toremove)
scores_t[is.na(scores_t)] <- FALSE

temp = scores_t %>% mutate_at(vars(-clusters), sum)
temp = temp[1,]

temp2 = scores_t %>% group_by(clusters) %>% summarise_all(sum)
temp2$tagsInCluster = rowSums(temp2[,2:ncol(temp2)])

for(i in 2:(ncol(temp2)-1)){
  temp2[,i]=temp2[,i]/temp2$tagsInCluster
}
temp2$tagsInCluster = NULL
temp2 = gather(temp2, "tag", "value", 2:ncol(temp2))
Toremove2 = c("Opposition", "Underpromotion", "Windmill", "En passant", "Desperado", "Zugzwang", "Stalemate", "Perpetual Check", "X-Ray Attack", "Interference", "Smothered Mate", "Double Check")
temp2 = temp2 %>% filter(!(tag %in% Toremove2))
ggplot(temp2, aes(x = clusters, y = tag, fill = value)) + geom_tile()

#temp2 = scores_t %>% group_by(clusters) %>% summarise_all(mean)
#temp2 = rbind(temp, temp2)

f <- function(col){
  return(col/max(col))
}


temp3 = temp2 %>% mutate_all(f)
temp3 = temp3[2:nrow(temp3),]
temp3$clusters = temp3$clusters * 20
temp4 = gather(temp3, "tag", "value", 2:ncol(temp3))

Toremove2 = c("Opposition", "Underpromotion", "Windmill", "En passant", "Desperado", "Zugzwang", "Stalemate", "Perpetual Check", "X-Ray Attack", "Interference", "Smothered Mate", "Double Check")

temp4 = temp4 %>% filter(!(tag %in% Toremove2))

ggplot(temp4, aes(x = clusters, y = tag, fill = value)) + geom_tile()

###
#check if duplicates are in same cluster
t1 = select(scores, tactics_problem_id, clusters)
colnames(t1) = c("tactics_problem_id", "cluster_1")
duplicate_data$tactics_problem_id = as.character(duplicate_data$ID1)
duplicate_data = left_join(duplicate_data, t1)
duplicate_data$tactics_problem_id = as.character(duplicate_data$ID2)
colnames(t1) = c("tactics_problem_id", "cluster_2")
duplicate_data = left_join(duplicate_data, t1)
duplicate_data$tactics_problem_id = NULL

duplicate_data = na.omit(duplicate_data)

sum(duplicate_data$cluster_1 == duplicate_data$cluster_2)/nrow(duplicate_data)

p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = clusters)) + 
  geom_point(size=2, aes(text = tactics_problem_id)) +  
  coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)) +  theme(legend.position = "none") #+
  
 # scale_color_gradient2(low = "red", high = "green", mid = "gold", midpoint = 1850)
p 

library(plotly)
ggplotly(p)

#run these 4 lines if you haven't already.
#This is for the cluster/tag heatmap
problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL
problemData$tactics_problem_id = as.character(problemData$tactics_problem_id)
scores = left_join(scores, problemData)
scores = ungroup(scores)
scores$clusters = as.factor(scores$clusters)
clusterTags = scores %>% select(-c(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, average_seconds, move_count))
clusterTags <- data.frame(sapply( clusterTags, as.numeric ))

#how often does each tag come up
r2 = clusterTags %>% select(-tactics_problem_id)%>% summarise_all(funs(sum(., na.rm = TRUE)))
r2 = melt(r2, id.vars = "clusters")
r2$clusters = NULL
colnames(r2) = c("variable", "n")


#we use mean so we don't skew for clusters with more problems
r = clusterTags %>% select(-tactics_problem_id)%>% group_by(clusters)  %>% summarise_all(funs(mean(., na.rm = TRUE)))
melted_score_by_cluster = melt(r, id.vars = "clusters")


melted_score_by_cluster = left_join(melted_score_by_cluster, r2)
melted_score_by_cluster$value_controlled = melted_score_by_cluster$value/melted_score_by_cluster$n
melted_score_by_cluster = subset(melted_score_by_cluster, melted_score_by_cluster$n > 10)


ggplot(data = melted_score_by_cluster, aes(x=clusters, y=variable, fill=value_controlled)) + 
  geom_tile() + scale_fill_gradient2(low = "red", high = "blue", mid = "white")

###
#now we pull out a narrow range to understand pc2
thin_band = subset(scores, scores$PC1 > -1)
thin_band = subset(thin_band, thin_band$PC1 < 1)
thin_band = thin_band %>% select(-c(clusters, PC1, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, average_seconds, move_count))
problem_elo$tactics_problem_id = as.character(problem_elo$tactics_problem_id)
problem_creation$tactics_problem_id = as.character(problem_creation$tactics_problem_id)

thin_band = left_join(thin_band, problem_elo)
thin_band = left_join(thin_band, problem_creation)
thin_band$first_attempt = thin_band$first_attempt - min(thin_band$first_attempt)
thin_band$elo_squared = thin_band$elo * thin_band$elo

model = glm(data = thin_band, PC2~.-tactics_problem_id)
ggplot(data = thin_band, aes(y = PC2, x = elo_squared, color = elo)) + geom_jitter()  + scale_color_gradient2(low = "red", high = "green", mid = "gold", midpoint = 1850) + geom_smooth()


###





##Here's where we see if PC2-5 are correlated with tags
#run these 4 lines if you haven't already
problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL
problemData$tactics_problem_id = as.character(problemData$tactics_problem_id)
scores = left_join(scores, problemData)


problem_creation$tactics_problem_id = as.character(problem_creation$tactics_problem_id)
scores = left_join(scores, problem_creation)


tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")


form_ = paste(tags, collapse = "`+`")

form_1 = as.formula(paste0("PC2 ~ elo+first_attempt+move_count+`",form_, "`"))

summary(lm(data = scores, form_1))

###
#normalizing stuff
for(i in 1:length(tags)){
  tag = tags[[i]]
  scores[[tag]] <- (scores[[tag]] - mean(scores[[tag]])) / sd(scores[[tag]])
}


#########
#lets see if the clusters from "scores" make sense as a tag
clusters = scores %>% select(tactics_problem_id, clusters)
clusters$tactics_problem_id = as.numeric(clusters$tactics_problem_id)

chess_data = chess_data_backup
chess_data = left_join(chess_data, clusters)
chess_data = subset(chess_data, !is.na(chess_data$clusters)) #roughly 10% of the data when using the duplicates dataset

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 500) 
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

#chess_data = chess_data %>% group_by(clusters, user_hash) %>% summarise(n= n()) %>% group_by(clusters) %>% summarise(clusteravg = mean(n)) %>% right_join(chess_data) %>% filter(clusteravg>4)
chess_data = chess_data %>% group_by(clusters, user_hash) %>% summarise(user_cluster_attempts = n()) %>% right_join(chess_data)
chess_data = chess_data %>% filter(user_cluster_attempts > 4)
chess_data = chess_data %>% group_by(clusters, user_hash) %>% mutate(is_last = (last(user_tactics_problem_id) == user_tactics_problem_id) + 0)

tags = c("Decoy / Deflection", "Double", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")
problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL
problemData = select(problemData, c(tactics_problem_id, tags))
chess_data = left_join(chess_data, problemData)

test = subset(chess_data, chess_data$is_last == 1)
chess_data = subset(chess_data, chess_data$is_last == 0)

chess_data = chess_data %>% group_by(user_hash) %>% filter( userGamesPlayed < (totalGamesPlayed-50))
chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))
chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate
chess_data = chess_data %>% ungroup() %>% group_by(user_hash, clusters) %>% mutate(user_cluster_success = mean(point_diff))

user_tag_strength = data.frame(unique(chess_data$user_hash))
colnames(user_tag_strength) = c("user_hash")

print(length(tags))
for(i in 1:length(tags)){
  print(i)
  tag_name = tags[i]
  temp = chess_data %>% ungroup() %>% filter(!!sym(tag_name) == 1) %>% group_by(user_hash) %>% summarise(temp = mean(point_diff))
  colnames(temp) = c("user_hash", paste0(tag_name, "1"))
  chess_data = left_join(chess_data, temp)
  chess_data[[paste0(tag_name, "1")]] = chess_data[[tag_name]]* chess_data[[paste0(tag_name, "1")]]
  chess_data[[paste0(tag_name, "1")]][is.na(chess_data[[paste0(tag_name, "1")]])] <- 0
  
  user_tag_strength = left_join(user_tag_strength, temp)
  
}

user_tag_strength[is.na(user_tag_strength)] <- 0


user_cluster_success = chess_data %>% ungroup() %>% group_by(user_hash, clusters) %>% summarise(user_cluster_success = mean(point_diff))
test = left_join(test, user_cluster_success)
test = left_join(test, user_tag_strength)


#run from here, fixing loop or something that causes test to be empty
for(i in 1:length(tags)){
  tag_name = tags[i]
  
  test[[paste0(tag_name, "1")]] = test[[tag_name]]* test[[paste0(tag_name, "1")]]
  test[[paste0(tag_name, "1")]][is.na(test[[paste0(tag_name, "1")]])] <- 0
   
}

model_baseline = glm(data = chess_data, family = binomial(), is_passed ~ ratingDiff)
model_cluster = glm(data = chess_data, family = binomial(), is_passed ~ ratingDiff+user_cluster_success )
model_tag = glm(data = chess_data, family = binomial(), is_passed ~ ratingDiff+ `Decoy / Deflection1` +  `Double1`+`Fork / Double Attack1`+`Hanging Piece1`+`Mate in 3+1`+`Mating Net1`+`Pin1` +`Remove the Defender1`+`Sacrifice1`+`Vulnerable King1`)
model_all = glm(data = chess_data, family = binomial(), is_passed ~ ratingDiff+user_cluster_success+`Decoy / Deflection1` +  `Double1`+`Fork / Double Attack1`+`Hanging Piece1`+`Mate in 3+1`+`Mating Net1`+`Pin1` +`Remove the Defender1`+`Sacrifice1`+`Vulnerable King1`)



test$model_baseline = predict(model_baseline, newdata = test, type = "response")
test$model_cluster = predict(model_cluster, newdata = test, type = "response")
test$model_tag = predict(model_tag, newdata = test, type = "response")
test$model_all = predict(model_all, newdata = test, type = "response")


test = test[!is.na(test$model_cluster),]


a = mean(abs(test$is_passed - test$model_baseline)) #.4480
b = mean(abs(test$is_passed - test$model_cluster)) #.4416
c = mean(abs(test$is_passed - test$model_tag)) #.4451
d = mean(abs(test$is_passed - test$model_all)) #.4401
