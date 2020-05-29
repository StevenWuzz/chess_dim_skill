#Does some analysis on the user tag ELOs.
#repeating analysis from when we used user's strength above/below expected for tag strenth
#Most notably makes plot of how good the model is if you just use more extreme subsets of users.

data1 = read_csv("/h/224/subramanian/user_elo_for_each_tag_randomized.csv")

data1 = subset(data1, data1$n > 500)

most_common_tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

columns_to_keep = c("user_hash", "n", "elo", most_common_tags)

data1 = data1 %>% select(columns_to_keep)

#use select and cbind
temp1 = data1 %>% select(c("user_hash", "n", "elo"))
temp2 = data1 %>% select(most_common_tags)

temp2$means = rowMeans(temp2)
temp2 = temp2 - temp2$means
temp2$means = NULL

data2 = cbind(temp1, temp2)
########

chess_data = chess_data_backup
chess_data = left_join(chess_data, data2)
chess_data = subset(chess_data, !is.na(chess_data$Defense))

first_games = subset(chess_data, chess_data$userGamesPlayed <= 50)
chess_data = subset(chess_data, chess_data$userGamesPlayed > 50)
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))
chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate

chess_data$totalGamesPlayed = chess_data$n


problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL

problemData = select(problemData, -average_seconds, -move_count)
colnames(problemData) = paste0(colnames(problemData), "tf")
colnames(problemData)[1] = "tactics_problem_id"

chess_data = left_join(chess_data, problemData)

tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

df = data.frame(matrix(,nrow = length(tags), ncol = 5))
colnames(df) = c("tag", "z_value", "MAE_model", "MAE_baseline", "n_users")

for(i in 1:length(tags)){
  tag1 = tags[i]
  tag1TF = paste0(tag1, "tf")
  data_tag1 = subset(chess_data, chess_data[[tag1TF]] == TRUE)
  
  last_100 = subset(data_tag1, data_tag1$userGamesPlayed > data_tag1$totalGamesPlayed - 11)
  data_tag1 =  subset(data_tag1, data_tag1$userGamesPlayed <= data_tag1$totalGamesPlayed - 11)
  
  user_strength = data_tag1 %>% group_by(user_hash) %>% summarise(tag_strength = last(!!sym(tag1)), user_attempts = n())
  data_tag1 = left_join(data_tag1, user_strength)
  last_100 = left_join(last_100, user_strength)
  data_tag1 = subset(data_tag1, data_tag1$user_attempts > 100)
  last_100 = subset(last_100, last_100$user_attempts > 100)
  
  
  d = sum(last_100$is_passed == 1) - sum(last_100$is_passed == 0)
  if(d > 0){
    last_100 = last_100[-sample(which(last_100$is_passed==1), d),]
  }
  if(d < 0){
    last_100 = last_100[-sample(which(last_100$is_passed==0), abs(d)),]
  }
  
  d = sum(data_tag1$is_passed == 1) - sum(data_tag1$is_passed == 0)
  if(d > 0){
    data_tag1 = data_tag1[-sample(which(data_tag1$is_passed==1), d),]
  }
  if(d < 0){
    data_tag1 = data_tag1[-sample(which(data_tag1$is_passed==0), abs(d)),]
  }
  
  #not rating user, rating diff
  mdl = glm(data=data_tag1, is_passed ~ tag_strength+ ratingDiff, family = binomial())
  mdl_baseline = glm(data=data_tag1, is_passed ~ ratingDiff, family = binomial())
  
  n = length(unique(last_100$user_hash))
  
  last_100$mdl = predict(mdl, newdata = last_100, type = "response")
  last_100$mdl_baseline = predict(mdl_baseline, newdata = last_100, type = "response")
  
  last_100 = last_100[!is.na(last_100$mdl),]
  
  a = mean(abs(last_100$is_passed - last_100$mdl))
  b = mean(abs(last_100$is_passed - last_100$mdl_baseline))
  
  df[i, ] = c(tag1, summary(mdl)$coefficients[3,3], a, b, n)
}
df$MAE_model = as.numeric(df$MAE_model)
df$MAE_baseline = as.numeric(df$MAE_baseline)
df$z_value = as.numeric(df$z_value)

df$improvement = df$MAE_model - df$MAE_baseline

ggplot(data = df, aes(x = tag, y = -1 * improvement)) + geom_bar(stat = "identity") + ylab("MAE improvement")  + theme(axis.text.x = element_text(angle = 90, hjust = 1))



tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

df = data.frame(matrix(,nrow = length(tags), ncol = 5))
colnames(df) = c("tag", "z_value", "MAE_model", "MAE_baseline", "n_users")

for(i in 1:length(tags)){
  tag1 = tags[i]
  data_tag1 = subset(chess_data, chess_data[[tag1]] == TRUE)
  last_100 = subset(data_tag1, data_tag1$userGamesPlayed > data_tag1$totalGamesPlayed - 11)
  data_tag1 =  subset(data_tag1, data_tag1$userGamesPlayed <= data_tag1$totalGamesPlayed - 11)
  user_strength = data_tag1 %>% group_by(user_hash) %>% summarise(tag_strength = mean(point_diff), user_attempts = n())
  data_tag1 = left_join(data_tag1, user_strength)
  last_100 = left_join(last_100, user_strength)
  data_tag1 = subset(data_tag1, data_tag1$user_attempts > 100)
  last_100 = subset(last_100, last_100$user_attempts > 100)
  
  
  d = sum(last_100$is_passed == 1) - sum(last_100$is_passed == 0)
  if(d > 0){
    last_100 = last_100[-sample(which(last_100$is_passed==1), d),]
  }
  if(d < 0){
    last_100 = last_100[-sample(which(last_100$is_passed==0), abs(d)),]
  }
  
  d = sum(data_tag1$is_passed == 1) - sum(data_tag1$is_passed == 0)
  if(d > 0){
    data_tag1 = data_tag1[-sample(which(data_tag1$is_passed==1), d),]
  }
  if(d < 0){
    data_tag1 = data_tag1[-sample(which(data_tag1$is_passed==0), abs(d)),]
  }
  
  #not rating user, rating diff
  mdl = glm(data=data_tag1, is_passed ~ tag_strength+ ratingDiff, family = binomial())
  mdl_baseline = glm(data=data_tag1, is_passed ~ ratingDiff, family = binomial())
  
  n = length(unique(last_100$user_hash))
  
  last_100$mdl = predict(mdl, newdata = last_100, type = "response")
  last_100$mdl_baseline = predict(mdl_baseline, newdata = last_100, type = "response")
  
  last_100 = last_100[!is.na(last_100$mdl),]
  
  a = mean(abs(last_100$is_passed - last_100$mdl))
  b = mean(abs(last_100$is_passed - last_100$mdl_baseline))
  
  df[i, ] = c(tag1, summary(mdl)$coefficients[3,3], a, b, n)
}
df$MAE_model = as.numeric(df$MAE_model)
df$MAE_baseline = as.numeric(df$MAE_baseline)
df$z_value = as.numeric(df$z_value)

df$improvement = df$MAE_model - df$MAE_baseline

####
#this makes the cool plot that measures MAE as you take a more extreme subset of users
tags = c("Endgame Tactics", "Defense", "Trapped Piece", "Basic Checkmates", "Simplification")

df = data.frame(matrix(,nrow = 50, ncol = 6))
colnames(df) = c("tag", "percentile", "z_value", "MAE_model", "MAE_baseline", "n_users")
k = 0

for(j in 1:5){
  
  
  tag1 = tags[j]
  data_tag1 = subset(chess_data, chess_data[[tag1TF]] == TRUE)
  last_100 = subset(data_tag1, data_tag1$userGamesPlayed > data_tag1$totalGamesPlayed - 11)
  data_tag1 =  subset(data_tag1, data_tag1$userGamesPlayed <= data_tag1$totalGamesPlayed - 11)
  user_strength = data_tag1 %>% group_by(user_hash) %>% summarise(tag_strength = last(!!sym(tag1)), user_attempts = n())
  user_strength = subset(user_strength, user_strength$user_attempts > 100)
  user_strength = user_strength %>% mutate(PCT = ntile(user_strength$tag_strength, 20))
  
  
  data_tag1 = left_join(data_tag1, user_strength)
  last_100 = left_join(last_100, user_strength)
  data_tag1 = data_tag1[!is.na(data_tag1$PCT),]
  
  
  
  
  for(i in 1:10){
    k = k + 1
    pct_included = c(1:i, (20-i+1):20)
    data_tag2 = subset(data_tag1, data_tag1$PCT %in% pct_included)
    last_100_2 = subset(last_100, last_100$PCT %in% pct_included)
    
    
    
    d = sum(last_100_2$is_passed == 1) - sum(last_100_2$is_passed == 0)
    if(d > 0){
      last_100_2 = last_100_2[-sample(which(last_100_2$is_passed==1), d),]
    }
    if(d < 0){
      last_100_2 = last_100_2[-sample(which(last_100_2$is_passed==0), abs(d)),]
    }
    
    d = sum(data_tag2$is_passed == 1) - sum(data_tag2$is_passed == 0)
    if(d > 0){
      data_tag2 = data_tag2[-sample(which(data_tag2$is_passed==1), d),]
    }
    if(d < 0){
      data_tag2 = data_tag2[-sample(which(data_tag2$is_passed==0), abs(d)),]
    }
    
    #not rating user, rating diff
    mdl = glm(data=data_tag2, is_passed ~ tag_strength+ ratingDiff, family = binomial())
    mdl_baseline = glm(data=data_tag2, is_passed ~ ratingDiff, family = binomial())
    
    n = length(unique(last_100_2$user_hash))
    
    last_100_2$mdl = predict(mdl, newdata = last_100_2, type = "response")
    last_100_2$mdl_baseline = predict(mdl_baseline, newdata = last_100_2, type = "response")
    
    last_100_2 = last_100_2[!is.na(last_100_2$mdl),]
    
    a = mean(abs(last_100_2$is_passed - last_100_2$mdl))
    b = mean(abs(last_100_2$is_passed - last_100_2$mdl_baseline))
    
    df[k, ] = c(tag1, i*5, summary(mdl)$coefficients[3,3], a, b, n)
    
  }}
df$percentile = as.numeric(df$percentile)
df$MAE_model = as.numeric(df$MAE_model)
df$MAE_baseline = as.numeric(df$MAE_baseline)
df$improvement = df$MAE_model - df$MAE_baseline

ggplot(data = df, aes(x = percentile, y = MAE_baseline, color = tag)) + geom_point() + geom_line() + ylab("MAE") + theme(legend.position = "none") + ylim(c(.428, .49))
ggplot(data = df, aes(x = percentile, y = improvement, color = tag)) + geom_point() + geom_line() + ylab("MAE improvement")
ggplot(data = df, aes(x = percentile, y = MAE_model, color = tag)) + geom_point() + geom_line() + ylab("MAE") + theme(legend.position = "none") + ylim(c(.428, .49))
######3

chess_data = chess_data_backup

problemRatings = chess_data %>% group_by(tactics_problem_id) %>% summarise(rating = last(ratingProblem), n = n(), firstDay = first(date), mc = max(correct_move_count))
problemRatings = subset(problemRatings, problemRatings$mc <= 8)
problemRatings = subset(problemRatings, problemRatings$mc != 0)

ggplot(problemRatings, aes(rating)) + geom_histogram() + facet_wrap(.~mc)


mround <- function(x,base){
  base*round(x/base)
} 

problemRatings$nRounded = mround(problemRatings$n, 500)
problemRatings$ratingRounded = mround(problemRatings$rating, 500)

