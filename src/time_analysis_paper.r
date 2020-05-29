#The session/streak analysis used in the paper

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)


chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")
chess_data_backup = chess_data

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 2500) 
#chess_data = subset(chess_data, chess_data$totalGamesPlayed < 5250) 

print(length(unique(chess_data$user_hash)))


#users = sample(chess_data$user_hash, 1500)
#chess_data = subset(chess_data, chess_data$user_hash %in% users)


chess_data = chess_data %>% group_by(user_hash) %>% arrange(create_date, .by_group = TRUE) %>% mutate(lastAttempt = lag(create_date))
chess_data$time_since_last_attempt = chess_data$create_date - chess_data$lastAttempt

chess_data$isNewSession = chess_data$time_since_last_attempt > 3600
chess_data$isNewSession[is.na(chess_data$isNewSession)] <- TRUE

chess_data = chess_data %>% group_by(user_hash) %>% mutate(sessionNumber = cumsum(isNewSession))
chess_data = chess_data %>% group_by(user_hash, sessionNumber) %>% mutate(sessionGame = 1:n())

#this line takes a long time
#started at 12:29, 625 users
#don't run this it's for session length
#chess_data = chess_data %>% group_by(user_hash, sessionNumber, isNewSession) %>% mutate(sessionLength = cumsum(as.numeric(time_since_last_attempt)))
#chess_data$sessionLength[which(chess_data$isNewSession==TRUE)] = 0

chess_data %>% head(100) %>% View()

chess_data$unique_session_id = paste0(chess_data$user_hash, chess_data$sessionNumber)
#Lead the time since last attempt column, subtract from seconds
#then you get time spent after thinking
chess_data = ungroup(chess_data)
chess_data = chess_data %>% mutate(time_til_next_started = lead(time_since_last_attempt, default = NA_integer_), is_ending_session = lead(isNewSession)) 
chess_data$time_til_next_started[which(chess_data$is_ending_session==TRUE)] = NA
chess_data = chess_data %>% group_by(unique_session_id) %>% mutate(gamesInSession = last(sessionGame))
chess_data$is_ending_session[which(is.na(chess_data$is_ending_session))] = TRUE


#Last_In_Session = subset(chess_data, chess_data$is_ending_session)
#chess_data = subset(chess_data, chess_data$is_ending_session == FALSE)
#chess_data$time_between_attempts = chess_data$time_til_next_started - chess_data$seconds

chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

chess_data$predicted_success_rate = 1/(1 + 10^((chess_data$ratingDiff)/400))

chess_data$point_diff = chess_data$is_passed - chess_data$predicted_success_rate

#chess_data = subset(chess_data, chess_data$time_between_attempts >= 0)
#chess_data$log_time_between_attempts = log(as.numeric(chess_data$time_between_attempts))
#chess_data = subset(chess_data, chess_data$log_time_between_attempts > -10)

sessionData = chess_data %>% group_by(gamesInSession) %>% summarise(mean(point_diff))#, totalGamesInSession = last(gamesInSession))
ggplot(sessionData, aes(x = gamesInSession, y  = `mean(point_diff)`)) + geom_bar(stat = "identity") + xlim(c(0, 50)) + labs(x = "games in session", y = "mean success minus expected success")
sessionData = chess_data %>% group_by(gamesInSession, sessionGame) %>% summarise(diff_success = mean(point_diff))
ggplot(sessionData, aes(x = gamesInSession, y =sessionGame, color = diff_success )) + geom_point(size = 2) + xlim(c(0, 50))+ ylim(c(0, 50)) +
  scale_colour_gradient2(low = "red", mid = "blue",
                         high = "black", midpoint = 0, aesthetics = "colour", limits = c(-.1, .1)) + labs(x = "Games in Session", y = "Game number",  color = "success")


ggplot(sessionData, aes(x = gamesInSession, y =sessionGame,  fill = diff_success )) + geom_tile() + xlim(c(0, 50))+ ylim(c(0, 50)) +
  scale_colour_gradient2(low = "red", mid = "blue",
                         high = "black", midpoint = 0, aesthetics = c( "fill"), limits = c(-.1, .1)) + labs(x = "Games in Session", y = "Game number",  fill = "success") 
  



sessionData = chess_data %>% group_by(unique_session_id) %>% summarise(user_hash = first(user_hash), tactics_problem_id = first(tactics_problem_id), ratingStart = first(ratingUser), ratingEnd = last(ratingUser), gamesInSession = first(gamesInSession), date = first(date))
sessionData$month <- format(as.Date(sessionData$date), "%Y-%m")
byMonth = sessionData %>% group_by(user_hash, month) %>% summarise(ratingStart = first(ratingStart), ratingEnd = last(ratingEnd), gamesInMonth = sum(gamesInSession), sessions = n())
byMonth$month_as_int = month(dym(byMonth$month)) + (year(dym(byMonth$month)) - 2008)*12

byMonth = byMonth %>% group_by(user_hash) %>% mutate(nextMonth = lead(month_as_int))
byMonth$nextMonthIsStreak = byMonth$month_as_int+1 == byMonth$nextMonth

#
temp = subset(byMonth, byMonth$gamesInMonth > 50)
temp2 = temp
temp = split(temp$month_as_int, cumsum(c(1, diff(temp$month_as_int) != 1)))
temp2$streakID = 0
#temp2$streakLength = 0
row = 0
for(i in 1:length(temp)){
  for(j in 1:length(temp[[i]])){
    row = row + 1
    temp2$streakID[row] = i
    #temp2$streakLength = length(temp[[i]])
    if(row %% 1000 == 1){
      print(row)
    }
  }
}
temp2 = temp2 %>% group_by(streakID) %>% mutate(streakLength = n())
temp2 = subset(temp2, temp2$streakLength > 6)
byStreak = temp2 %>% group_by(streakID) %>% summarise(streakLength = first(streakLength), ratingStart = first(ratingStart), ratingEnd = last(ratingEnd), games = sum(gamesInMonth), user_hash = first(user_hash))
byStreak$ratingDiff = byStreak$ratingEnd - byStreak$ratingStart

ggplot(byStreak, aes( x=ratingDiff, y = games)) + geom_jitter() + xlab("rating Improvement") + ggtitle("Improvement after 6+ consecutive months of playing")
#
temp2$nextMonthIsStreak[which(is.na(temp2$nextMonthIsStreak))] = TRUE
temp2 = temp2 %>% group_by(streakID) %>% mutate(gamesPlayedInStreak = cumsum(gamesInMonth), monthInStreak = cumsum(nextMonthIsStreak))
temp2 = temp2 %>% group_by(streakID) %>% mutate(startStreakRating = first(ratingStart)) 
#BY MONTH DATA is temp2 and temp3

temp2$ratingDiff = temp2$ratingEnd - temp2$ratingStart


temp3 = subset(temp2, temp2$monthInStreak <= 6)
temp3 = subset(temp3, temp3$startStreakRating != 1720 )

temp3 = ungroup(temp3)
t = temp3 %>% group_by(monthInStreak) %>% summarise(improvement = mean(ratingDiff))
t = subset(t, t$monthInStreak!= 0)
ggplot(t, aes(x = monthInStreak, y = improvement)) + geom_bar(stat = "identity")


byStreak = temp3 %>% group_by(streakID) %>% summarise(ratingStart = first(ratingStart), ratingEnd = last(ratingEnd), games = sum(gamesInMonth), user_hash = first(user_hash))
byStreak$ratingDiff = byStreak$ratingEnd - byStreak$ratingStart

ggplot(byStreak, aes( x=ratingDiff, y = games, color = ratingStart)) + geom_jitter() + xlab("rating Improvement") + ggtitle("Improvement after 6 consecutive months of playing")+ scale_colour_gradient2( low = "red", mid= "grey",
                                                                                                                                                                                                          high = "blue", midpoint = 1840, space = "Lab",
                                                                                                                                                                                                          na.value = "grey50", guide = "colourbar", aesthetics = "colour") + labs(color = "Initial Rating" )
ggplot(byStreak, aes(ratingDiff)) + geom_histogram(binwidth = 50) + xlim(c(-500, 750)) + xlab("Rating Improvement")

model1 = (glm(data = byStreak, ratingDiff~ratingStart+games))

temp3$changeInRatingOverStreak = temp3$ratingEnd - temp3$startStreakRating
ggplot(temp3, aes(x = gamesPlayedInStreak, y = changeInRatingOverStreak, group = streakID, color = startStreakRating)) + geom_line(alpha = .3) +xlim(c(0, 7500))+ylim(c(-1000, 1000)) + scale_colour_gradient2( low = "red", mid= "grey",
                                                                                                                                                                        high = "blue", midpoint = 1840, space = "Lab",
                                                                                                                                                                        na.value = "grey50", guide = "colourbar", aesthetics = "colour") + labs(title = "Rating Progression for Regular Users over 6 month period", y = "Improvement in Rating Since Start", x= "Games Played", color = "Initial Rating" )


ggplot(temp3, aes(x = monthInStreak, y = changeInRatingOverStreak, group = streakID, color = startStreakRating)) + geom_line(alpha = .3) +xlim(c(1, 6))+ylim(c(-1000, 1000)) + scale_colour_gradient2( low = "red", mid= "grey",
                                                                                                                                                                                                          high = "blue", midpoint = 1840, space = "Lab",
                                                                                                                                                                                                          na.value = "grey50", guide = "colourbar", aesthetics = "colour")
                                                                                                                                                                                                                

chess_data = ungroup(chess_data)
user_overall_data = chess_data %>% group_by(user_hash) %>% summarise(overall_pass_rate = mean(is_passed)) 
sessionData = ungroup(sessionData)
user_session_data = sessionData %>% group_by(user_hash) %>% summarise(normal_session_length = mean(gamesInSession))

user_game_data = chess_data %>% group_by(user_hash, sessionGame) %>% summarise(pass_rate = mean(is_passed)) 
user_game_data = left_join(user_game_data, user_overall_data)
user_game_data = left_join(user_game_data, user_session_data)
ugd2 = subset(user_game_data, user_game_data$sessionGame == 1)
ugd2$firstProblemSuccess = ugd2$pass_rate - ugd2$overall_pass_rate

ggplot(ugd2, aes(x = normal_session_length, y = firstProblemSuccess)) + geom_bin2d() + geom_smooth(color = "black") + xlim(0, 50) + ylim(-.2, .2) + ylab("Success on First Puzzle") + xlab("Average Session Length")



##now to get longer streaks. 24 months, at least 100 problems per month
temp_ = subset(byMonth, byMonth$gamesInMonth > 50)
temp_2 = temp_
temp_ = split(temp_$month_as_int, cumsum(c(1, diff(temp_$month_as_int) != 1)))
temp_2$streakID = 0
#temp_2$streakLength = 0
row = 0
for(i in 1:length(temp_)){
  for(j in 1:length(temp_[[i]])){
    row = row + 1
    temp_2$streakID[row] = i
    #temp_2$streakLength = length(temp_[[i]])
    if(row %% 1000 == 1){
      print(row)
    }
  }
}
temp_2 = temp_2 %>% group_by(streakID) %>% mutate(streakLength = n())
temp_2 = subset(temp_2, temp_2$streakLength > 23)
byStreak_ = temp_2 %>% group_by(streakID) %>% summarise(streakLength = first(streakLength), ratingStart = first(ratingStart), ratingEnd = last(ratingEnd), games = sum(gamesInMonth), user_hash = first(user_hash))
byStreak_$ratingDiff = byStreak_$ratingEnd - byStreak_$ratingStart

#
temp_2$nextMonthIsStreak[which(is.na(temp_2$nextMonthIsStreak))] = TRUE
temp_2 = temp_2 %>% group_by(streakID) %>% mutate(gamesPlayedInStreak = cumsum(gamesInMonth), monthInStreak = cumsum(nextMonthIsStreak))
temp_2 = temp_2 %>% group_by(streakID) %>% mutate(startStreakRating = first(ratingStart)) 

temp_2$ratingDiff = temp_2$ratingEnd - temp_2$ratingStart


temp_3 = subset(temp_2, temp_2$monthInStreak <= 24)
temp_3 = subset(temp_3, temp_3$startStreakRating != 1720 )

temp_3 = ungroup(temp_3)
t = temp3 %>% group_by(monthInStreak) %>% summarise(improvement = mean(ratingDiff))
t = subset(t, t$monthInStreak!= 0)
ggplot(t, aes(x = monthInStreak, y = improvement)) + geom_bar(stat = "identity") + xlab("Month In Streak")


##
byStreak_ = temp_3 %>% group_by(streakID) %>% summarise(ratingStart = first(ratingStart), ratingEnd = last(ratingEnd), games = sum(gamesInMonth), user_hash = first(user_hash))
byStreak_$ratingDiff = byStreak_$ratingEnd - byStreak_$ratingStart

ggplot(byStreak_, aes( x=ratingDiff, y = games, color = ratingStart)) + geom_jitter() + xlab("rating Improvement") + ggtitle("Improvement after 6 consecutive months of playing")+ scale_colour_gradient2( low = "red", mid= "grey",
                                                                                                                                                                                                          high = "blue", midpoint = 1840, space = "Lab",
                                                                                                                                                                                                          na.value = "grey50", guide = "colourbar", aesthetics = "colour") + labs(color = "Initial Rating" )
ggplot(byStreak_, aes(ratingDiff)) + geom_histogram(binwidth = 50) + xlim(c(-500, 750)) + xlab("Rating Improvement")

model2 = (glm(data = byStreak_, ratingDiff~ratingStart+games))

temp_3$changeInRatingOverStreak = temp_3$ratingEnd - temp_3$startStreakRating
ggplot(temp_3, aes(x = gamesPlayedInStreak, y = changeInRatingOverStreak, group = streakID, color = startStreakRating)) + geom_line(alpha = .3) +xlim(c(0, 7500))+ylim(c(-1000, 1000)) + scale_colour_gradient2( low = "red", mid= "grey",
                                                                                                                                                                                                                high = "blue", midpoint = 1840, space = "Lab",
                                                                                                                                                                                                                na.value = "grey50", guide = "colourbar", aesthetics = "colour") + labs(title = "Rating Progression for Regular Users over 24 month period", y = "Improvement in Rating Since Start", x= "Games Played", color = "Initial Rating" )


ggplot(temp_3, aes(x = monthInStreak, y = changeInRatingOverStreak, group = streakID, color = startStreakRating)) + geom_line(alpha = .3) +xlim(c(1, 24))+ylim(c(-500, 1000)) + scale_colour_gradient2( low = "red", mid= "grey",
                                                                                                                                                                                                       high = "blue", midpoint = 1840, space = "Lab",
                                                                                                                                                                                                       na.value = "grey50", guide = "colourbar", aesthetics = "colour") + labs(title = "Rating Progression for Regular Users over 24 month period", y = "Improvement in Rating Since Start", x= "Months", color = "Initial Rating" )


