#prediction task for paper

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)


chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")
chess_data_backup = chess_data

#step 1, cull users
chess_data = chess_data_backup
chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 500) #55972 users
#length(unique(chess_data$user_hash))

#step 2, calculate session info
chess_data = chess_data %>% group_by(user_hash) %>% arrange(create_date, .by_group = TRUE) %>% mutate(lastAttempt = lag(create_date))
chess_data$time_since_last_attempt = chess_data$create_date - chess_data$lastAttempt

chess_data$isNewSession = chess_data$time_since_last_attempt > 3600
chess_data$isNewSession[is.na(chess_data$isNewSession)] <- TRUE

chess_data = chess_data %>% group_by(user_hash) %>% mutate(sessionNumber = cumsum(isNewSession))
chess_data = chess_data %>% group_by(user_hash, sessionNumber) %>% mutate(sessionGame = 1:n())

chess_data$unique_session_id = paste0(chess_data$user_hash, chess_data$sessionNumber)

chess_data = ungroup(chess_data)
chess_data = chess_data %>% mutate(time_til_next_started = lead(time_since_last_attempt, default = NA_integer_), is_ending_session = lead(isNewSession)) 
chess_data$time_til_next_started[which(chess_data$is_ending_session==TRUE)] = NA
chess_data = chess_data %>% group_by(unique_session_id) %>% mutate(gamesInSession = last(sessionGame))
chess_data$is_ending_session[which(is.na(chess_data$is_ending_session))] = TRUE

chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))

chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate

SES_INFO = chess_data %>% select(user_tactics_problem_id, isNewSession, sessionNumber, sessionGame, unique_session_id, ratingDiff, predicted_success_rate, point_diff)


#step 3, take last 50 problems from each user 
chess_data = chess_data_backup
chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 500) 
chess_data$ratingDiff=chess_data_$ratingProblem - chess_data$ratingUser
test = chess_data %>% group_by(user_hash) %>% filter( userGamesPlayed >= (totalGamesPlayed-50))
chess_data = chess_data %>% group_by(user_hash) %>% filter( userGamesPlayed < (totalGamesPlayed-50))
chess_data$ratingDiff=chess_data_$ratingProblem - chess_data$ratingUser
chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))
chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate
chess_data_train = chess_data
#things to include in prediction task
#user ELO, problem ELO (already included)
#user tag ability * is_tag for problem

user_tag_data = read_csv("/h/224/subramanian/user_elo_for_each_tag.csv")
user_tag_data = user_tag_data %>% select(c(user_hash, elo:Opposition))
user_hashes = user_tag_data$user_hash
user_tag_data$user_hash = NULL

#for(i in 1:nrow(user_tag_data)){ #takes forever
#  user_tag_data[i,] = user_tag_data[i,]-as.numeric(user_tag_data[i,1])
#  if(i%%15288 == 1){
#    print(i/1528863)
#  }
#}
user_tag_data$user_hash = user_hashes
#write.csv(x = user_tag_data, file = "user_tag_elos_mean_subtracted_all.csv")

problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL
problemData = problemData %>% select(-average_seconds)
problemData = problemData %>% select(-move_count)
problemData = problemData + 0

chess_data = chess_data %>% select(user_tactics_problem_id, user_hash, tactics_problem_id)
#join with problem data
#then join with user tag data, but rename columns first?
#then find way to multiply by column, or just loop through names
#And I think that's it!

#game in session, exp number of games in user's session
#did they get previous probelm in session correct? (remove if no time)