library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(reshape)
library(schoolmath)

chess_data <- fread("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00",data.table = FALSE,
                    showProgress = TRUE)

passed = subset(chess_data,is_passed == 1)
train_dat = select(passed,tactics_problem_id,user_hash)

#count #(w,c)
trans_data <- train_dat %>% group_by(tactics_problem_id, user_hash) %>% mutate(count= n()) #everything is 1???
#count #(w)
trans_data_vocab <- train_dat %>% group_by(tactics_problem_id) %>% mutate(count_vocab = n())
#count #(c)
trans_data_context <- train_dat %>% group_by(user_hash) %>% mutate(count_context = n())

#check whether there are any NA in vocab_dat$count
vec <- is.na(vocab_dat$count)
vec <- !vec
all(vec) #TRUE

#check whether there are any NA in context_dat$count
vec1 <- is.na(context_dat$count)
vec1 <- !vec1
all(vec1) #TRUE

#Hence, there are no NA in both vocab_dat$count and context_dat$count

#find |D|
cardinality_D <- sum(vocab_dat$count) + sum(context_dat$count)
#make a vector of |D| with length of trans_data's row
vec_cardinality_D <- rep(cardinality_D, nrow(trans_data))
#make a dataframe of the aforementioned vector
cardinality_D_data <- data.frame("car_D" = vec_cardinality_D)

#compute the PMI
pmi_data <- log(trans_data$count) + log(cardinality_D_data$car_D) - log(trans_data_vocab$count_vocab) - log(trans_data_context$count_context)
#make a dataframe of the PMI
pmi_data_frame <- data.frame("PMI_value" = pmi_data)
#if there are any negative PMI values, replace it with 0. This is to generate the PPMI value
pmi_data_frame$PMI_value[pmi_data_frame$PMI_value < 0] <- 0 #indeed, there are some negative values

#merge the train data and the PPMI values together
complete_data <- train_dat %>% mutate(PPMI_value = pmi_data_frame$PMI_value)
