#Self explanatory, just some SVM code on the user/problem matrix

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(smooth)
library(broom)
library(recommenderlab)	



chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")




user_data = chess_data %>% group_by(user_hash) %>% summarise(attempts_user = n(), successes_user = sum(is_passed))

chess_data = left_join(chess_data, user_data)
chess_data = subset(chess_data, chess_data$attempts_user > 5000)

chess_data = chess_data %>% select(user_hash, tactics_problem_id, is_passed)#, ratingUser, ratingProblem)
#chess_data$is_passed[chess_data$is_passed==0]<--1

problems = chess_data %>% group_by(tactics_problem_id) %>% summarise(n = n())
#6000th problem is around 1500 attempts
chess_data = left_join(chess_data, problems)
chess_data = subset(chess_data, chess_data$n > 1500)

#remove data so pass rate is 50%
pass = sum(chess_data$is_passed)
fail = nrow(chess_data) - pass
numToRemove = pass-fail
pass = subset(chess_data, chess_data$is_passed == 1)
pass = sample_n(pass, nrow(pass) - numToRemove)
fail = subset(chess_data, chess_data$is_passed == 0)
chess_data = rbind(pass, fail)

#error_rates = c()
#for(i in 1:10){

chess_data$n = NULL

svm_matrix = spread(chess_data, tactics_problem_id, is_passed)

#svm_matrix[is.na(svm_matrix)]<-0

#svm_matrix = as.matrix(svm_matrix)

#svm_matrix = svm_matrix[,1:6000]

row_names_svm = svm_matrix[,1]
#row.names(svm_matrix) = svm_matrix[,1]

svm_matrix = svm_matrix[,-1]
#svm_matrix$user_hash = NULL


svm_matrix<-sapply(data.frame(svm_matrix),as.numeric)
row.names(svm_matrix) = row_names_svm$user_hash
svm_2 <- as(svm_matrix,"realRatingMatrix")

sparsity = ((ncol(svm_matrix)*nrow(svm_matrix)) - sum(is.na(svm_matrix)))/(ncol(svm_matrix)*nrow(svm_matrix))

e <- evaluationScheme(svm_2, method="split", train=0.9, given = -1)

Rec.ibcf <- Recommender(getData(e, "train"), "UBCF") #better than IBCF
p.ibcf <- predict(Rec.ibcf, getData(e, "known"), type="ratings")

error.ibcf<-calcPredictionAccuracy(p.ibcf, getData(e, "unknown"))
#error_rates = c(error_rates, data.frame(error.ibcf)$error.ibcf[3])

#}

View(head(svm_matrix[,1:100]))

### SVD

svm_matrix = data.frame(svm_matrix)
svm_matrix = sapply(svm_matrix, as.numeric)
svm_matrix = svm_matrix[-which(rowSums(svm_matrix, na.rm = TRUE) == 0),]
s = svd(svm_matrix)

##

t = gather(data.frame(x), key = "problem",value = "prediction", na.rm = TRUE)
#convert both to factor, remove "X" from name using gsub, see if equal. 
#correct prediction rate ends up being equal to 1-MAE

###
#testing how a logistic regression fits

## 75% of the sample size
smp_size <- floor(0.9 * nrow(chess_data))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(chess_data)), size = smp_size)

train <- chess_data[train_ind, ]
test <- chess_data[-train_ind, ]

lin_model1 = glm(data = train, formula = is_passed ~ ratingUser + ratingProblem, family=binomial())
lin_model2 = glm(data = train, formula = is_passed ~ diff_elo, family=binomial())

test$prediction_1 = (predict(lin_model1, test, type = "response") > .5) + 0
test$prediction_2 = (predict(lin_model2, test, type = "response") > .5) + 0
sum(abs(test$prediction_1 - test$is_passed))/nrow(test) #65.4% correct
sum(abs(test$prediction_2 - test$is_passed))/nrow(test)

####
predictions = data.frame(row = integer(), problem = character(), prediction = integer())
for (i in 1:length(x)) {
   p = data.frame(x[[i]])
   problem = row.names(p)
   rows = rep(i, nrow(p))
   p = cbind(rows, problem, p)
   row.names(p) = NULL
   predictions = rbind(predictions, p)
}
colnames(predictions) = c("row", "problem", "prediction")

actual_values = c()
m_svm2 = as(svm_2, "matrix")
for(i in 1:nrow(predictions)){
  row = predictions[i,]
  actual_values = c(actual_values, m_svm2[as.numeric(row$row),as.character(row$problem)])
}
predictions = cbind(predictions, actual_values)
predictions$prediction = as.integer(predictions$prediction)
