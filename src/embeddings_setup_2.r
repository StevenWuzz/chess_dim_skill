library(data.table)
library(dplyr)

#Load chess_data (first 10 million)
chess_data = fread("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00",nrows=10000000,data.table = FALSE,
                   showProgress = TRUE)

#Filter out attempts that were unsuccessful
passed = subset(chess_data,is_passed == 1)

#passed = passed %>% group_by(user_hash) %>% mutate(count = n()) %>% ungroup(passed)

#Filter out users that have completed less than 50 puzzles
#passed = subset(passed,passed$count >= 50)

#Create the train data that looks like: vocab_word context_word
train_dat = select(passed,tactics_problem_id,user_hash)

#Treat the puzzles as the vocab words and save to the proper format required for the word2vecf vocabl file (vocab_word count)
vocab_dat = train_dat %>% group_by(tactics_problem_id) %>% summarise(count = n()) %>% ungroup()

#Treat the users as the context words and save to the proper format required for the word2vecf vocabl file (context_word count)
context_dat =  train_dat %>% group_by(user_hash) %>% summarise(count = n()) %>% ungroup()

#Save files with spaces seperating values
write.table(context_dat, file = "cv", row.names = FALSE, col.names=FALSE, sep = " ",quote=FALSE)
write.table(vocab_dat, file = "wv", row.names = FALSE, col.names=FALSE, sep = " ")
write.table(train_dat, file = "train_data", row.names = FALSE, col.names=FALSE, sep = " ", quote=FALSE)
