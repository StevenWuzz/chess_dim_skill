#model showing weak correlation between tags
#that doesn't add to predictive power

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(reshape2)
library(cluster)

chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")
chess_data_backup = chess_data

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 2000) 
#chess_data = subset(chess_data, chess_data$totalGamesPlayed < 10000)
chess_data = subset(chess_data, chess_data$totalGamesPlayed < 50000)


first_games = subset(chess_data, chess_data$userGamesPlayed <= 50)
chess_data = subset(chess_data, chess_data$userGamesPlayed > 50)
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser
#
#chess_data = subset(chess_data, chess_data$ratingDiff <= 30)
#chess_data = subset(chess_data, chess_data$ratingDiff >= -30)
#
chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))

chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate

problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL

#attatch tags to problems
chess_data = left_join(chess_data, problemData)





#predict each of them given the other and their own rating (and ELO)

#first lets only look at problems with close ELOs
#top 20
#tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")
#top 10
tags = c("Decoy / Deflection", "Double", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")
#specific tags
#tags = c("Defense", "Sacrifice")


df = data.frame(matrix(,nrow = length(tags) * (length(tags)+1), ncol = 7))
colnames(df) = c("dependent_tag", "independent_tag", "n_users", "MAE_Tag1", "MAE_Both_Tags", "MAE_Tag1_and_ELO", "MAE_Both_and_ELO")

row_num = 1
#step one, iterate through all pairs of tags
for(i in 1:length(tags)){
  for(j in i:length(tags)){
    tag1 = tags[i]
    tag2 = tags[j]
    print(paste(tag1, ",", tag2, ",", row_num))
    if(i == j){
      next
    }
    #subset dataset into tags with just one of the two
    correlated_tag_subset = chess_data %>% filter((!!as.symbol(tag1)) + (!!as.symbol(tag2)) == 1)
    subset_tag1 = subset(correlated_tag_subset, correlated_tag_subset[[tag1]] == TRUE)
    subset_tag2 = subset(correlated_tag_subset, correlated_tag_subset[[tag2]] == TRUE)
    
    #make sure users have at least 100 problems in both tags
    t1 = subset_tag1 %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 100)
    t2 = subset_tag2 %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 100)
    t3 = inner_join(t1, t2, by = "user_hash")
    if(nrow(t3) == 0){
      df[row_num,] = c(tag1, tag2, 0,  NA, NA, NA, NA)   
      row_num = row_num + 1
      df[row_num,] = c(tag2, tag1, 0,  NA, NA, NA, NA)   
      row_num = row_num + 1
      next
    }
    t3$n.y = NULL
    t3$n.x = 1
    subset_tag1 = left_join(subset_tag1, t3)
    subset_tag2 = left_join(subset_tag2, t3)
    subset_tag1 = subset(subset_tag1, subset_tag1$n.x == 1)
    subset_tag2 = subset(subset_tag2, subset_tag2$n.x == 1)
    subset_tag1$n.x = NULL
    subset_tag2$n.x = NULL
    #split into problems that are close by elo and not close
    subset_tag1_close = subset(subset_tag1, abs(subset_tag1$ratingDiff) <= 40) 
    #subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) > 30)
    subset_tag2_close = subset(subset_tag2, abs(subset_tag2$ratingDiff) <= 40)
    #subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) > 30)

    #figure out how many people have >10 close attempts of each
    t1 = subset_tag1_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t2 = subset_tag2_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t3 = inner_join(t1, t2, by = "user_hash")
    
    if(nrow(t3) == 0){
      df[row_num,] = c(tag1, tag2, 0,  NA, NA, NA, NA)   
      row_num = row_num + 1
      df[row_num,] = c(tag2, tag1, 0,  NA, NA, NA, NA)   
      row_num = row_num + 1
      next
    }
    #subset into just those users
    t3$n.y = NULL
    t3$n.x = 1
    subset_tag1_close = left_join(subset_tag1_close, t3)
    subset_tag2_close = left_join(subset_tag2_close, t3)
    subset_tag1_close = subset(subset_tag1_close, subset_tag1_close$n.x == 1)
    subset_tag2_close = subset(subset_tag2_close, subset_tag2_close$n.x == 1)
    #take 10 random attempts from each of those
    validation_1 = subset_tag1_close %>% group_by(user_hash) %>% sample_n(10)
    validation_2 = subset_tag2_close %>% group_by(user_hash) %>% sample_n(10)
    train_set_1 = anti_join(subset_tag1_close, validation_1)
    train_set_2 = anti_join(subset_tag2_close, validation_2)
    #calculate how people did on those tags alone
    t1 = train_set_1 %>% group_by(user_hash) %>% summarise(tag1_ability = mean(point_diff))
    t2 = train_set_2 %>% group_by(user_hash) %>% summarise(tag2_ability = mean(point_diff))
    validation_1 = left_join(validation_1, t1)
    validation_1 = left_join(validation_1, t2)
    validation_2 = left_join(validation_2, t1)
    validation_2 = left_join(validation_2, t2)
    train_set_1 = left_join(train_set_1, t1)
    train_set_2 = left_join(train_set_2, t1)
    train_set_1 = left_join(train_set_1, t2)
    train_set_2 = left_join(train_set_2, t2)
    
    ###only look at users in the margins
    t1 = ungroup(t1)
    t2 = ungroup(t2)

    

    ###
    #build predictive model using diffElo (or not) and how they did on the other tags
    model_1_tag1 = glm(data=train_set_1, is_passed~tag1_ability, family = binomial())
    model_1_both = glm(data=train_set_1, is_passed~tag1_ability+tag2_ability, family = binomial())
    model_1_elo = glm(data=train_set_1, is_passed~tag1_ability+ratingDiff, family = binomial())
    model_1_all = glm(data=train_set_1, is_passed~tag1_ability+tag2_ability+ratingDiff, family = binomial())
    model_2_tag2 = glm(data=train_set_2, is_passed~tag2_ability, family = binomial())
    model_2_both = glm(data=train_set_2, is_passed~tag2_ability+tag1_ability, family = binomial())
    model_2_elo = glm(data=train_set_2, is_passed~tag2_ability+ratingDiff, family = binomial())
    model_2_all = glm(data=train_set_2, is_passed~tag2_ability+tag1_ability+ratingDiff, family = binomial())
    
    #also keep n in mind and store that
    n = length(unique(validation_1$user_hash))
    #predict on prediction set
    validation_1$model_1_tag1 = predict(model_1_tag1, newdata = validation_1, type = "response")
    validation_1$model_1_both = predict(model_1_both, newdata = validation_1, type = "response")
    validation_1$model_1_elo = predict(model_1_elo, newdata = validation_1, type = "response")
    validation_1$model_1_all = predict(model_1_all, newdata = validation_1, type = "response")
    validation_2$model_2_tag2 = predict(model_2_tag2, newdata = validation_2, type = "response")
    validation_2$model_2_both = predict(model_2_both, newdata = validation_2, type = "response")   
    validation_2$model_2_elo = predict(model_2_elo, newdata = validation_2, type = "response")
    validation_2$model_2_all = predict(model_2_all, newdata = validation_2, type = "response")   
    
    #calculate MAE and record that for all three models
    #colnames(df) = c("dependent_tag", "independent_tag", "n_users", "MAE_ELO", "MAE_Tag1", "MAE_Both_Tags")
    df[row_num,] = c(tag1, tag2, n,  mean(abs(validation_1$is_passed - validation_1$model_1_tag1)), mean(abs(validation_1$is_passed - validation_1$model_1_both)), mean(abs(validation_1$is_passed - validation_1$model_1_elo)), mean(abs(validation_1$is_passed - validation_1$model_1_all)))   
    row_num = row_num + 1
    df[row_num,] = c(tag2, tag1, n,  mean(abs(validation_2$is_passed - validation_2$model_2_tag2)), mean(abs(validation_2$is_passed - validation_2$model_2_both)), mean(abs(validation_2$is_passed - validation_2$model_2_elo)), mean(abs(validation_2$is_passed - validation_2$model_2_all)))   
    row_num = row_num + 1
    #dataset is tag1, tag2, n, mae
    
  }
}

write.csv(df, file = "predicting_multiple_tags.csv")
