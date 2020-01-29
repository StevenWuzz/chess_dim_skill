#used to create tsvs to use here: https://projector.tensorflow.org/
#the output of this script goes through what was here: https://bitbucket.org/yoavgo/word2vecf/src/default/
#The actual github with the data output to be plugged in directly 
#can be found here: https://github.com/adisubramanian/ChessEmbedding


library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(smooth)
library(broom)

chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")

chess_data = chess_data %>% group_by(user_hash) %>% mutate(finalRatingUser = last(ratingUser))
chess_data = chess_data %>% group_by(tactics_problem_id) %>% mutate(finalRatingProblem = last(ratingProblem))


user_data = chess_data %>% group_by(user_hash) %>% summarise(attempts_user = n(), successes_user = sum(is_passed), finalRatingUser = last(finalRatingUser))

chess_data = left_join(chess_data, user_data)
chess_data = subset(chess_data, chess_data$attempts_user > 1000)

user_data = subset(user_data, user_data$attempts_user > 1000)
problem_data = chess_data %>% group_by(tactics_problem_id) %>% summarise(attempts_problem = n(), successes_problem = sum(is_passed), finalRatingProblem = last(ratingProblem))
problem_data$successes_problem = problem_data$attempts_problem - problem_data$successes_problem

chess_data$user_hash = paste(chess_data$finalRatingUser, chess_data$user_hash, sep = "I")
user_data$user_hash = paste(user_data$finalRatingUser, user_data$user_hash, sep = "I")
chess_data$tactics_problem_id = paste(chess_data$finalRatingProblem, chess_data$tactics_problem_id, sep = "I")
problem_data$tactics_problem_id = paste(problem_data$finalRatingProblem, problem_data$tactics_problem_id, sep = "I")

user_data$finalRatingUser = NULL
problem_data$finalRatingProblem = NULL

user_problem_data = chess_data %>% select(user_hash, tactics_problem_id, is_passed)
user_problem_data = subset(user_problem_data, user_problem_data$is_passed == 1)

#user_problem_data$is_passed = NULL

user_data_s = user_data
user_data_s$attempts_user = NULL
problem_data_s = problem_data
problem_data_s$attempts_problem = NULL

user_problem_data_2 = user_problem_data[,c(2,1,3)] #this one gives me user distances

write.csv2(user_data_s, file  = "/u/subramanian/yoavgo-word2vecf-0d8e19d2f2c6/user_data_s.txt", sep = ", ", row.names = F, col.names = F)

write.csv2(user_problem_data_2, file = "/u/subramanian/yoavgo-word2vecf-0d8e19d2f2c6/user_problem_2.txt", sep = ", ", row.names = F, col.names = F)
write.csv2(user_problem_data, file = "/u/subramanian/yoavgo-word2vecf-0d8e19d2f2c6/user_problem.txt", sep = ", ", row.names = F, col.names = F)

write.csv2(problem_data_s, file = "/u/subramanian/yoavgo-word2vecf-0d8e19d2f2c6/problem_data_s.txt", sep = ", ", row.names = F, col.names = F)
