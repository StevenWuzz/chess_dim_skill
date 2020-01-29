#An earlier look into time analysis


library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)

#plan to get time stuff figured out

#goals
#1. see if time spent between questions is correlated with improvement, or improvement on a specific tag
#This could be modeled as "time spent after last problem incorrect w tag" 
#


chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")


chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 2000) 
chess_data = subset(chess_data, chess_data$totalGamesPlayed < 4000) #only regular users
users = sample(chess_data$user_hash, 200)
chess_data = subset(chess_data, chess_data$user_hash %in% users)


chess_data = chess_data %>% group_by(user_hash) %>% arrange(create_date, .by_group = TRUE) %>% mutate(lastAttempt = lag(create_date))
chess_data$time_since_last_attempt = chess_data$create_date - chess_data$lastAttempt

chess_data$isNewSession = chess_data$time_since_last_attempt > 3600
chess_data$isNewSession[is.na(chess_data$isNewSession)] <- TRUE

chess_data = chess_data %>% group_by(user_hash) %>% mutate(sessionNumber = cumsum(isNewSession))
chess_data = chess_data %>% group_by(user_hash, sessionNumber) %>% mutate(sessionGame = 1:n())
chess_data = chess_data %>% group_by(user_hash, sessionNumber, isNewSession) %>% mutate(sessionLength = cumsum(as.numeric(time_since_last_attempt)))
chess_data$sessionLength[which(chess_data$isNewSession==TRUE)] = 0

chess_data %>% head(100) %>% View()

chess_data$unique_session_id = paste0(chess_data$user_hash, chess_data$sessionNumber)
#Lead the time since last attempt column, subtract from seconds
#then you get time spent after thinking
chess_data = ungroup(chess_data)
chess_data = chess_data %>% mutate(time_til_next_started = lead(time_since_last_attempt, default = NA_integer_), is_ending_session = lead(isNewSession)) 
chess_data$time_til_next_started[which(chess_data$is_ending_session==TRUE)] = NA

Last_In_Session = subset(chess_data, chess_data$is_ending_session)
chess_data = subset(chess_data, chess_data$is_ending_session == FALSE)
chess_data$time_between_attempts = chess_data$time_til_next_started - chess_data$seconds


chess_data = subset(chess_data, chess_data$time_between_attempts >= 0)
chess_data$log_time_between_attempts = log(as.numeric(chess_data$time_between_attempts))
chess_data = subset(chess_data, chess_data$log_time_between_attempts > -10)

f = ggplot(chess_data,aes (ratingUser, log_time_between_attempts)) + geom_jitter() + geom_smooth()
f
g = ggplot(chess_data,aes( rating_change,log_time_between_attempts)) + geom_jitter() + geom_smooth() + xlim(-50, 50)
g

chess_data = chess_data %>% group_by(user_hash) %>% mutate(log_normal_time_spent = mean(log_time_between_attempts))
to_join = chess_data %>% group_by(unique_session_id) %>% summarise(log_time_spent_this_session = mean(log_time_between_attempts), log_normal_time_spent = first(log_normal_time_spent))


Last_In_Session = left_join(Last_In_Session, to_join)
Last_In_Session$Log_diff = Last_In_Session$log_time_spent_this_session - Last_In_Session$log_normal_time_spent
Last_In_Session$diff_elo = Last_In_Session$ratingUser - Last_In_Session$ratingProblem


model1 = lm(is_passed~diff_elo+Log_diff+log_normal_time_spent , data = Last_In_Session)
summary(model1)
model2 = lm(is_passed~ratingUser+ratingProblem+Log_diff+log_normal_time_spent , data = Last_In_Session)
summary(model2)
model3 = lm(Log_diff~is_passed+diff_elo+log_normal_time_spent , data = Last_In_Session)
summary(model3)

#2. The other idea is reverse engineering improvement
#take out the first 100 games for each player, then look for periods of sustained improvement
#where they never get below a certain rating again (min 50?)?
#calc that by going in reverse and keeping track of lowest rating so far and mark lowest
#and also keep track of games left


chess_data2 = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")


chess_data2 = chess_data2 %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data2 = subset(chess_data2, chess_data2$totalGamesPlayed > 2000) 
chess_data2 = subset(chess_data2, chess_data2$totalGamesPlayed < 4000) #only regular users
#users = sample(chess_data2$user_hash, 200)
users = unique(chess_data$user_hash)
chess_data2 = subset(chess_data2, chess_data2$user_hash %in% users)

chess_data2 = subset(chess_data2, chess_data2$userGamesPlayed > 200)

chess_data2 = chess_data2 %>% group_by(user_hash) %>% arrange(desc(userGamesPlayed), .by_group = TRUE) 
minFutureRating2 = 100000 
chess_data2$minFutureRating = 100000

for(i in 1:nrow(chess_data2)){
  if(chess_data2$ratingUser[i] < minFutureRating2){
    minFutureRating2 = chess_data2$ratingUser[i]
  }
  chess_data2$minFutureRating[i] = minFutureRating2
  if(chess_data2$userGamesPlayed[i] == 201){
    minFutureRating2 = 100000
  }
}

chess_data2 = chess_data2 %>% group_by(user_hash) %>% arrange(userGamesPlayed, .by_group = TRUE) 
last_100 = chess_data2 %>% group_by(user_hash) %>% filter(userGamesPlayed > .9*totalGamesPlayed)
train_set = chess_data2 %>% group_by(user_hash) %>% filter(userGamesPlayed < .9*totalGamesPlayed)

train_set$is_min_future = train_set$minFutureRating == train_set$ratingUser
periods = train_set %>% filter(is_min_future) %>% mutate(next_min = lead(userGamesPlayed), prev_min =  lag(userGamesPlayed))

periods$next_min[is.na(periods$next_min)] <- 5000
periods$prev_min[is.na(periods$prev_min)] <- 100
periods$is_start_of_improvement = ifelse(periods$prev_min + 20 < periods$userGamesPlayed, TRUE, FALSE)
periods$is_end_of_improvement = ifelse(periods$next_min - 20 > periods$userGamesPlayed, TRUE, FALSE)

starts = subset(periods, periods$is_start_of_improvement) %>% select(user_hash,userGamesPlayed, ratingUser)

ends = subset(periods, periods$is_end_of_improvement) %>% select(user_hash,userGamesPlayed, ratingUser)
colnames(starts) = c("user_hash", "games_played_start", "rating_start")
colnames(ends) = c("user_hash", "games_played_end", "rating_end")
starts$row <- seq.int(nrow(starts))
ends$row <- seq.int(nrow(ends))

start_ends = left_join(starts, ends)
start_ends$row = NULL
start_ends$length_of_streak = start_ends$games_played_end - start_ends$games_played_start
start_ends$rating_change = start_ends$rating_end - start_ends$rating_start


h = ggplot(start_ends, aes(length_of_streak)) + geom_histogram(binwidth = 5) + xlab("Length of improvement streak")
h
k = ggplot(start_ends, aes(length_of_streak, rating_change)) + geom_jitter() + geom_smooth() + xlim(0, 50)
k

real_streaks = subset(start_ends, start_ends$length_of_streak > 9)
real_streaks = subset(real_streaks, real_streaks$rating_change > 50)

tgp = periods %>% group_by(user_hash) %>% summarise(totalGamesPlayed = last(totalGamesPlayed))
real_streaks = left_join(real_streaks, tgp)

train_set$is_improving = 0

for(i in 1:nrow(real_streaks)){
  start = real_streaks$games_played_start[i]
  end = real_streaks$games_played_end[i]
  user = real_streaks$user_hash[i]
  for(j in which(train_set$user_hash == user)){
    if(train_set$userGamesPlayed[j] >= start && train_set$userGamesPlayed[j] <= end){
      train_set$is_improving[j] = 1
    } else if (train_set$userGamesPlayed[j] < start && train_set$userGamesPlayed[j] >= start - 50){
      train_set$is_improving[j] = 2
    }
  }
}

time_data = chess_data %>% select(user_tactics_problem_id, time_between_attempts, log_time_between_attempts, log_normal_time_spent) %>% ungroup()
train_set = left_join(train_set, time_data)

prior_to_improvement = subset(train_set, train_set$is_improving == 2)
is_improving = subset(train_set, train_set$is_improving == 1)
not_improving = subset(train_set, train_set$is_improving == 0)

train_set$streak_status = 0
train_set$streak_status[train_set$is_improving == 0] <- "No Streak"
train_set$streak_status[train_set$is_improving == 1] <- "Improving"
train_set$streak_status[train_set$is_improving == 2] <- "Just Prior To Improvement"

no_nas = train_set[complete.cases(train_set[ , 20:22]),]
nrow(no_nas)

m = no_nas %>% ggplot(aes(streak_status, log_time_between_attempts)) + geom_boxplot(aes(group = streak_status)) 
m 
#hypothesis: improve more common near ending, may take less time once used to it
length(unique(is_improving$user_hash))
