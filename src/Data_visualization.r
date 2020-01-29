#Very old code most of which was used for the data visualizations 
#in the plots folder
#Also a couple early prediction tasks
#other than plots most notable section is code to create user play sessions

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
#install.packages("smooth")
library(smooth)
library(broom)
library(zoo)




chess_data = read_csv("/ada/data/chess/chessdotcom/tactics/glicko_user_tactics_problem.csv_00")

#chess_data2 = filter(chess_data2, tactics_problem_id==793)

#print(nrow(unique(chess_data["user_hash"])))
#print(nrow(unique(chess_data["tactics_problem_id"])))

head(chess_data)
colnames(chess_data)
length(colnames(chess_data))



problemFinal = chess_data %>% group_by(tactics_problem_id) %>% summarise(valueProb = last(ratingProblem), varProb = last(varianceProblem), attempts = n(), move_count = max(correct_move_count))
problemFinal$logAttempts = log(problemFinal$attempts)

head(problemFinal)

#problemFinal$move_count = factor(problemFinal$move_count)
#unique(problemFinal$move_count)
#ggplot(problemFinal, aes(group = move_count, x =move_count, y = valueProb)) + geom_boxplot() + ylab("problem rating")

temp = problemFinal %>% group_by(attempts) %>% summarise(n = n()) %>% mutate(cumulative = cumsum(n), oneminus = sum(n) - cumsum(n))
temp = temp[order(-temp$attempts),]
temp$logAttempts = log10(temp$attempts)
temp$logCDF = log10(temp$oneminus)
head(temp)

#+ scale_x_discrete(limits = rev(levels(temp$attempts)))
#%>% mutate(cumulative = cumsum(n))
#ggplot(problemFinal) + #stat_ecdf(aes(logAttempts))
#geom_line(aes(10-logAttempts), stat='ecdf') + xlim(0, 7) + ggtitle("problem data")



ggplot(temp, aes(x = attempts, y = oneminus)) + geom_line()  + ylab("cdf") + xlab("attempts") + scale_x_log10() + scale_y_log10()

ggplot(problemFinal, aes(valueProb)) + geom_histogram() + xlab("Problem Rating") + ylab("n")
ggplot(problemFinal, aes(varProb)) + geom_histogram(binwidth = 5) + xlab("Problem Variance") + ylab("n")
ggplot(problemFinal, aes(attempts)) + geom_histogram(binwidth = 50) + xlab("Times Attempted") + xlim(0, 10000) + ylab("n")

#max(problemFinal["attempts"])

userFinal = chess_data %>% group_by(user_hash) %>% summarise(valueUser = last(ratingUser), varUser = last(varianceUser), attempts = last(userGamesPlayed))
userFinal$logAttempts = log(userFinal$attempts)
head(userFinal)

temp = userFinal %>% group_by(attempts) %>% summarise(n = n()) %>% mutate(cumulative = cumsum(n), oneminus = sum(n) - cumsum(n))
temp = temp[order(-temp$attempts),]
temp$logAttempts = log(temp$attempts)
temp$logCDF = log(temp$oneminus)
head(temp)
ggplot(temp, aes(x = attempts, y = oneminus)) + geom_line()  + ylab("cdf") + xlab("attempts") + scale_x_log10() + scale_y_log10()

ggplot(userFinal, aes(valueUser)) + geom_histogram() + xlab("User Rating")


ggplot(userFinal, aes(varUser)) + geom_histogram(binwidth = 10) + xlab("User Variance")


##
#

chess_data = chess_data%>% group_by(tactics_problem_id) %>% mutate(problemAttemptedCount = cumsum(is_passed - is_passed + 1))
chess_data["problemAttemptedCount"] = chess_data["problemAttemptedCount"] - 1

userFinal = userFinal[order(-userFinal$attempts),]
#subset(userFinal, userFinal$attempts > 100)
#row(userFinal)
#andom_users =  userFinal[sample(nrow(userFinal), 50), ]
#row(random_users)

head(userFinal)

nrow(chess_data)
high_user_data = subset(chess_data, chess_data$user_hash %in% head(userFinal,5)$user_hash )
#head(high_user_data)

#high_user_data$smoothed_rating  = group_by(high_user_data, user_hash) %>% smooth.spline()#(smooth.spline(high_user_data$ratingUser)$y)


fnrollmean <- function (x) {
  if (length(x) < 7) {
    rep(NA,length(x)) 
  } else {
    rollmean(x,1000,align="center",na.pad=TRUE)
  }
}

high_user_data <- high_user_data %>% group_by(user_hash) %>% 
  mutate(rollavg=fnrollmean(ratingUser))
test2 = NULL
#test2<-group_by(high_user_data, user_hash) %>% mutate(rolling_mean = rollmean(ratingUser, 3))
head(high_user_data, 15)

head(high_user_data, 3)
nrow(high_user_data)

#smoothed_rating_over_time
high_user_data %>% ggplot(aes(x = userGamesPlayed, y = rollavg, color = user_hash)) + geom_line() + theme(legend.position="none")  + ggtitle("Ratings of 5 users with most attempts over time") + ylab("user rating, smoothed") + xlab("games played by user")#+ xlim(0, 20000) + ylim(1000, 4000)

high_user_data %>% ggplot(aes(x = userGamesPlayed, y = ratingUser, color = user_hash)) + geom_line() + theme(legend.position="none")  + ggtitle("Ratings of 5 users with most attempts over time") + ylab("user rating") + xlab("games played by user")#+ xlim(0, 20000) + ylim(1000, 4000)

nrow(chess_data)
high_user_data = subset(chess_data, chess_data$user_hash %in% head(userFinal,1000)$user_hash )
head(high_user_data)

ggplot(high_user_data, aes(userGamesPlayed, ratingUser, color = user_hash)) + 
  geom_smooth(method='lm')  + theme(legend.position="none")

results = high_user_data %>% 
  group_by(user_hash) %>%
  do(tidy(lm(ratingUser ~ userGamesPlayed, data = .))) %>% 
  filter(term == "userGamesPlayed") 

head(results, 10)
ggplot(results, aes(estimate)) + geom_histogram(binwidth = .005) + xlab("slope of rating ~ games played") + ggtitle("1000 users with most attempts")

###breaking things into sessions
chess_data = chess_data %>% group_by(user_hash) %>% arrange(create_date, .by_group = TRUE) %>% mutate(lastAttempt = lag(create_date))
chess_data$time_since_last_attempt = chess_data$create_date - chess_data$lastAttempt

chess_data$isNewSession = chess_data$time_since_last_attempt > 7200
chess_data$isNewSession[is.na(chess_data$isNewSession)] <- TRUE

chess_data = chess_data %>% group_by(user_hash) %>% mutate(sessionNumber = cumsum(isNewSession))
chess_data = chess_data %>% group_by(user_hash, sessionNumber) %>% mutate(sessionGame = 1:n())
chess_data = chess_data %>% group_by(user_hash, sessionNumber, isNewSession) %>% mutate(sessionLength = cumsum(as.numeric(time_since_last_attempt)))
chess_data$sessionLength[which(chess_data$isNewSession==TRUE)] = 0

chess_data %>% head(100) %>% View()

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
onlyRegularUsers = subset(chess_data, chess_data$totalGamesPlayed > 2000) 

#success rate by session plot
onlyRegularUsers %>% group_by(sessionGame) %>% summarise(successRate = mean(is_passed)) %>% ggplot( aes(x = sessionGame, y = successRate, group = sessionGame)) + geom_bar(stat = "identity") + xlim(0, 150) + xlab("Games played in session") + ylab("Sucess rate minus expected success rate")


roundUp <- function(x,to=50)
{
  to*(x%/%to + as.logical(x%%to))
}

chess_data["user_rounded"] = roundUp(unlist(chess_data["ratingUser"], use.names=FALSE))
chess_data["problem_rounded"] = roundUp(unlist(chess_data["ratingProblem"], use.names=FALSE))

chess_data["diffELO"] = chess_data["ratingUser"] - chess_data["ratingProblem"]
chess_data["diff_rounded"] = roundUp(unlist(chess_data["diffELO"], use.names=FALSE), 50)


#summary(glm( is_passed ~ diffELO, data = chess_data, family=binomial()))

model_success_simple =  glm(chess_data, is_passed ~ diffELO, family=binomial())

onlyRegularUsers["diffELO"] = onlyRegularUsers["ratingUser"] - onlyRegularUsers["ratingProblem"]
onlyRegularUsers$prediction = predict(model_success_simple, onlyRegularUsers, type = "response")

onlyRegularUsers %>% mutate(diffOverExpectation = is_passed - prediction) %>% group_by(sessionGame) %>% summarise(AboveExpected = mean(diffOverExpectation)) %>% ggplot( aes(x = sessionGame, y = AboveExpected )) + geom_bar(stat = "identity") + xlim(0, 50) + xlab("Games played in session") + ylab("Difference in success rate and expected success rate")

model_success_simple2 =  glm(is_passed ~ diffELO, data = onlyRegularUsers, family=binomial())
onlyRegularUsers$prediction2 = predict(model_success_simple2, onlyRegularUsers, type = "response")
onlyRegularUsers %>% mutate(diffOverExpectation2 = is_passed - prediction2) %>% group_by(sessionGame) %>% summarise(AboveExpected = mean(diffOverExpectation2)) %>% ggplot( aes(x = sessionGame, y = AboveExpected )) + geom_bar(stat = "identity") + xlim(0, 50) + xlab("Games played in session") + ylab("Difference in success rate and expected success rate") + geom_smooth()


onlyRegularUsers %>% mutate(diffOverExpectation2 = is_passed - prediction2) %>% group_by(sessionLength) %>% summarise(AboveExpected = mean(diffOverExpectation2)) %>% ggplot( aes(x = sessionLength, y = AboveExpected )) + geom_bar(stat = "identity") + xlim(0, 1000) + xlab("Session Time before start of problem") + ylab("Difference in success rate and expected success rate") + geom_smooth()

onlyRegularUsers = onlyRegularUsers %>% group_by(user_hash, sessionNumber) %>% mutate(hotHand = lag(is_passed))
onlyRegularUsers$hotHand <- ifelse(is.na(onlyRegularUsers$hotHand), 'First Game', onlyRegularUsers$hotHand)
onlyRegularUsers %>% mutate(diffOverExpectation2 = is_passed - prediction2) %>% ggplot(aes(x = as.factor(hotHand), y = diffOverExpectation2, group = as.factor(hotHand))) + geom_boxplot()


problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")

problemData$X1 = NULL
#head(chess_data, 3)
#head(problemData, 3)
chess_data = left_join(chess_data, problemData, by = "tactics_problem_id")

colnames(chess_data)

#write.csv(x=chess_data, file = "/w/225/1/chess/tactics/chess_data_tags_updated.csv")
head(chess_data, 3)

problemFinal$varProb = NULL
head(problemFinal)

problemFinal = left_join(problemFinal, problemData)
problemFinal$attempts = NULL
problemFinal$logAttempts = NULL
problemFinal$average_seconds = NULL

onlyRegularUsers = left_join(onlyRegularUsers, problemFinal, by = "tactics_problem_id")

newcolnames = c("user_hash")

cname = colnames(onlyRegularUsers)[30]
temp = onlyRegularUsers %>%  filter(!!as.name(colnames(onlyRegularUsers)[30]) == TRUE) %>% group_by(user_hash) %>% summarise(winPCT = mean(is_passed), gp = n())
newcolnames = c(newcolnames, paste(cname, "winPCT", sep = "_"),  paste(cname, "GP", sep = "_"))
user_tag_info = temp
colnames(user_tag_info) = newcolnames

for(i in 31:length(colnames(onlyRegularUsers))){
  print(i)
  cname = colnames(onlyRegularUsers)[i]
  temp = onlyRegularUsers %>%  filter(!!as.name(colnames(onlyRegularUsers)[i]) == TRUE) %>% group_by(user_hash) %>% summarise(winPCT = mean(is_passed), gp = n())
  newcolnames = c(newcolnames, paste(cname, "winPCT", sep = "_"),  paste(cname, "GP", sep = "_"))
  user_tag_info = left_join(user_tag_info, temp, by = "user_hash")
  colnames(user_tag_info) = newcolnames
  
}
View(head(user_tag_info, 100))
#nrow(onlyRegularUsers) #52062558
#length(unique(onlyRegularUsers$user_hash)) #12109


#chess_data %>% group_by(user_hash, sessionGame) %>% 
#ggplot(chess_data, aes(x = sessionGame, y = sessionLength, group = sessionGame)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) + xlab("tag")

#sum(as.numeric(chess_data$create_date - chess_data$lastAttempt) < 0, na.rm = TRUE)
#temp = chess_data %>% filter(create_date - lastAttempt < 1)
#head(temp)

temp = chess_data %>% group_by(user_hash, sessionNumber) %>% summarise(total_time = last(sessionLength) + last(seconds), numGames = last(sessionGame))
t = ggplot(temp, aes(y = total_time, x = numGames, group = numGames)) + geom_boxplot() + xlim(0, 25)
t + ylim(0, 2000)

ggplot(temp, aes(numGames, total_time)) + geom_jitter() + geom_smooth()

#chess_data %>% filter(seconds < 0) %>% head(100)
temp %>% filter(total_time < 0) %>% head(100)


medianWithoutNA<-function(x) {
  median(x[which(!is.na(x))])
}
medianWithoutNA(chess_data$time_since_last_attempt)

chess_data = chess_data %>% filter(userGamesPlayed > 14 & problemAttemptedCount > 14)

nrow(chess_data2)
nrow(unique(chess_data2["tactics_problem_id"]))
nrow(unique(chess_data2["user_hash"]))

#head(chess_data)

#chess_data2 %>% ggplot( aes(x = ratingUser, y = ratingProblem, color = is_passed)) + geom_point()
#chess_data2 %>% ggplot( aes(x = ratingUser, y = ratingProblem, color = is_passed)) + geom_raster()

ggplot(chess_data, aes(x = ratingUser, y = ratingProblem, z = is_passed)) +
  stat_summary_2d(bins = 20, color = "grey", fun = mean) +        
  theme_classic()


head(chess_data, 3)

#chess_data2 %>% group_by(user_rounded, problem_rounded) %>% summarise(percent_correct = mean(is_passed)) %>% ggplot( aes(x = user_rounded, y = percent_correct, color = problem_rounded, group = problem_rounded)) + geom_line()

topProblems = chess_data %>% group_by(tactics_problem_id) %>% summarise(n = n(), rating = last(ratingProblem)) %>% arrange(desc(n)) %>% head(50)
#793, 33571, 746, 51998, 33508

head(topProblems)

library(IRdisplay)
problems = c(746, 793, 33571, 27530)
ratings = c(1598, 1800, 1917, 2128)


for(i in 98:200){
  print(i)
  problem = topProblems$tactics_problem_id[i]
  rating = topProblems$rating[i]
  t = chess_data %>% filter(tactics_problem_id %in% c(problem)) %>% ggplot(aes(x = ratingUser, y = is_passed)) + geom_smooth() + ggtitle(toString(rating))#+ facet_wrap(~tactics_problem_id)
  filename = paste0("/w/225/1/chess/tactics/images/", toString(problem), ".png")
  ggsave(filename, plot = t)
}

#chess_data2 %>% ggplot(aes(x = seconds, y = is_passed, color = problem_rounded)) + geom_smooth()

#chess_data2 %>% filter(seconds < 121, varianceUser < 100) %>% ggplot(aes(x = seconds, y = is_passed)) + geom_smooth()



chess_data["user_rounded_500"] = roundUp(unlist(chess_data["ratingUser"], use.names=FALSE), 500)
chess_data$user_rounded_500 = factor(chess_data$user_rounded_500)


chess_data$user_rounded = factor(chess_data$user_rounded)
chess_data$diff_rounded = factor(chess_data$diff_rounded)
chess_data$problem_rounded = factor(chess_data$problem_rounded)

head(chess_data)

ggplot(chess_data, aes(diffELO)) + geom_histogram(binwidth = 15) + xlim(-600, 600)

chess_data %>% filter(seconds < 121, varianceUser < 100) %>% ggplot(aes(x = seconds, y = is_passed)) + geom_smooth(method = "glm") + facet_wrap(~user_rounded_500)

chess_data$interaction_seconds_rating = chess_data$seconds * chess_data$ratingUser
#chess_data %>% filter(seconds < 121, varianceUser < 100) %>% ggplot(aes(x = seconds, y = is_passed)) + geom_smooth() + facet_wrap(~user_rounded_500)


smp_size <- floor(0.75 * nrow(chess_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(chess_data)), size = smp_size)

train <- chess_data[train_ind, ]
test <- chess_data[-train_ind, ]

#install.packages("biglm")
#library(biglm)



logitDiff <- glm(is_passed ~ seconds + diffELO, data = train, family = "binomial")
logitRatings <- glm(is_passed ~ seconds + ratingUser + ratingProblem, data = train, family = "binomial")


logitDiff <- glm(is_passed ~ interaction_seconds_rating + diffELO, data = train, family = "binomial")

#write.csv(chess_data, file = "chess_data_for_model.csv")

summary(logitDiff)

summary(logitRatings)

predictDiff = predict(logitDiff, test)
predictRatings = predict(logitRatings, test)

predictDiff = ((predictDiff > 0) + 0)
predictRatings = ((predictRatings > 0) + 0)

predictionTable = data.frame(test$is_passed, predictDiff, predictRatings)

head(predictionTable)
nrow(predictionTable)

predictionTable$DiffCorrect = (predictionTable$test.is_passed == predictionTable$predictDiff) + 0
predictionTable$RatingCorrect = (predictionTable$test.is_passed == predictionTable$predictRating) +0
predictionTable$samePredict = (predictionTable$predictDiff == predictionTable$predictRating)+0



sum(predictionTable$DiffCorrect)/nrow(predictionTable)
sum(predictionTable$RatingCorrect)/nrow(predictionTable)
sum(predictionTable$samePredict)/nrow(predictionTable)

sum(chess_data$is_passed)/nrow(chess_data)

predictDiff = predict(logitDiff, test) #with interaction term
predictDiff = ((predictDiff > 0) + 0)
predictionTable = data.frame(test$is_passed, predictDiff)
predictionTable$DiffCorrect = (predictionTable$test.is_passed == predictionTable$predictDiff) + 0
sum(predictionTable$DiffCorrect)/nrow(predictionTable)


problems = c(746, 793, 33571, 27530)
ratings = c(1598, 1800, 1917, 2128)

for(i in 1:length(problems)){
  problem = problems[i]
  
  chess_problem = filter(chess_data, tactics_problem_id  == problem)
  smp_size <- floor(0.75 * nrow(chess_problem))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(chess_problem)), size = smp_size)
  
  train <- chess_problem[train_ind, ]
  test <- chess_problem[-train_ind, ]
  
  
  
  logitDiff <- glm(is_passed ~ seconds + diffELO, data = train, family = "binomial")
  
  predictDiff = predict(logitDiff, test)
  predictDiff = ((predictDiff > 0) + 0)
  
  print("rating:")
  print(ratings[i])
  print("accuracy")
  print(sum(predictDiff == test$is_passed)/nrow(test))
  
}

chess_data %>% group_by(diff_rounded, user_rounded_500) %>% summarise(percent_correct = mean(is_passed)) %>% ggplot( aes(x = diff_rounded, y = percent_correct, color = user_rounded_500, group = user_rounded_500)) +xlim(-500, 500) + geom_line()
#positive means rating user > rating problem

unique(chess_data$diff_rounded)

#problemData = read_csv("/w/225/1/chess/tactics/tactics_problem.csv")
#head(problemData)

#colnames(problemData)

#problemData$pgn = NULL
#problemData$attempt_count = NULL
#problemData$rd = NULL
#problemData$rating = NULL


#head(problemData, 3)

#library(stringr)
#max(str_count(problemData$tags, ","))
#tagList = unique(unlist(str_split(problemData$tags, ",")))



#for(i in 1:length(tagList)){
#    word = tagList[i]
#    temp = (grepl(word, problemData$tags))
#    problemData = cbind(problemData, temp)
#}
#colnames(problemData) = c("tactics_problem_id", "average_seconds", "move_count", "tags", tagList)
#problemData$tags = NULL
#head(problemData)

problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")

problemData$X1 = NULL
#head(chess_data, 3)
#head(problemData, 3)
chess_data = left_join(chess_data, problemData, by = "tactics_problem_id")

colnames(chess_data)

#write.csv(x=chess_data, file = "/w/225/1/chess/tactics/chess_data_tags_updated.csv")
head(chess_data, 3)

problemFinal$varProb = NULL
head(problemFinal)

problemFinal = left_join(problemFinal, problemData)

#head(problemFinal)
#nrow(problemFinal)
#colnames(problemFinal)

corrData = problemFinal
corrData$tactics_problem_id = NULL
corrData$valueProb = NULL
corrData$attempts = NULL
corrData$logAttempts = NULL
corrData$average_seconds = NULL
corrData$move_count = NULL
corrData = data.matrix(corrData)
head(corrData)

#corrData = NULL

matrix1 = rbind(colnames(corrData))
colnames(matrix1) = colnames(corrData)
i = 1
for(i in 1:length(colnames(corrData))){
  testvar = colnames(corrData)[i]
  tempData = corrData %>% subset(corrData[,colnames(corrData)[i]] == 1) 
  row = colMeans(tempData)
  matrix1 = rbind(matrix1, row)
  #head(corrData)
  #testvar
  
}
rownames(matrix1) = c("", colnames(matrix1))
head(matrix1[-1,])

library(reshape2)
melted_cormat <- melt(matrix1)
melted_cormat = (subset(melted_cormat,! melted_cormat$value %in% colnames(matrix1)))
melted_cormat = (subset(melted_cormat,! melted_cormat$Var2  == "NULL"))
melted_cormat = (subset(melted_cormat,! melted_cormat$Var1  == "NULL"))
melted_cormat$value = as.numeric(as.character(melted_cormat$value))


ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  theme_minimal()+ 
  theme( axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1,
                                    size = 10))+ 
  scale_fill_gradient2(low = "white", high = "red", mid = "violet", 
                       midpoint = .2, limit = c(0,1), space = "Lab", 
                       name="Percentage \nAppearing \nTogether") +
  coord_fixed() + ylab("") + xlab("")


#ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile()  + theme(legend.position="none", axis.text.x = element_text(angle = 90))

problemFinal =  (gather(problemFinal, "tag", "tf", 7:ncol(problemFinal)))

problemFinal = subset(problemFinal, problemFinal$tf == TRUE)
problemFinal$tf = NULL

head(problemFinal)

ggplot(problemFinal, aes(x =reorder(tag, valueProb, FUN = median), y = valueProb)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) + xlab("tag") + ylab("ProblemRating")

ggplot(problemFinal, aes(x =reorder(tag, logAttempts, FUN = median), y = logAttempts)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) + xlab("tag")

object.size(chess_data)
#object.size(chess_data)
#chess_data = head(chess_data, 1000000)
#t= NULL

cd2 = chess_data
cd2$user_tactics_problem_id = NULL
cd2$user_hash = NULL
cd2$create_date = NULL
cd2$date = NULL 
cd2$last_game_date = NULL
cd2$correct_move_count = NULL
cd2$rating_change=NULL 
cd2$tactics_problem_id = NULL
cd2$ratingUser = NULL
cd2$varianceUser = NULL
cd2$ratingProblem = NULL
cd2$varianceProblem = NULL
cd2$userGamesPlayed = NULL
cd2$problemAttemptedCount = NULL
cd2$user_rounded = NULL 
cd2$problem_rounded = NULL
cd2$diff_rounded = NULL 
cd2$user_rounded_500 = NULL


smp_size <- floor(0.75 * nrow(cd2))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(cd2)), size = smp_size)

train <- cd2[train_ind, ]
test <- cd2[-train_ind, ]


cd2 = NULL

#install.packages("randomForest")
#library(randomForest)
logit <- glm(is_passed ~ diffELO + `average_seconds` + `move_count` + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition`, data = train, family = binomial())

summary(logit)

predictDiff = predict(logit, test)
predictDiff = ((predictDiff > 0) + 0)





print(sum((predictDiff == test$is_passed) + 0, na.rm = TRUE)/nrow(test))

colnames(chess_data)
head(chess_data)



problemCorr = chess_data %>% filter(problemAttemptedCount > 1000) %>% group_by(tactics_problem_id) %>% summarise(corUser = cor(is_passed, ratingUser), corDiff = cor(is_passed, diffELO), problemRating = last(ratingProblem))

head(problemCorr)

problemCorr2 = subset(problemCorr, ! problemCorr$tactics_problem_id %in% subset(problemFinal, problemFinal$tag == "NULL")$tactics_problem_id)

head(problemFinal)

outliers = rbind(subset(problemCorr, problemCorr$corDiff > .75), subset(problemCorr, problemCorr$corDiff < -0.25))
outliers = subset(outliers, ! outliers$tactics_problem_id %in% subset(problemFinal, problemFinal$tag == "NULL")$tactics_problem_id)

outliers =  outliers[order(outliers$corDiff),] 
outliers$problemRating = NULL
head(outliers, 19)

head(outliers)
nrow(outliers)

outliers = subset(problemFinal, problemFinal$tactics_problem_id %in% outliers$tactics_problem_id) %>% left_join(outliers)

ggplot(outliers, aes(valueProb, corDiff, color = tag)) + geom_jitter()

head(problemFinal)

ggplot(problemCorr, aes(x = corUser)) + geom_histogram(binwidth = .05) + xlim(-.4, .8) + xlab("Correlation between user rating and success")

ggplot(problemCorr, aes(x = corDiff)) + geom_histogram(binwidth = .05) + xlim(-.2, .8) + xlab("Correlation between difference in ELO and success")

ggplot(problemCorr2, aes(y = corDiff, x = problemRating)) + geom_jitter() + geom_smooth() + xlab("problem rating") + ylab("Correlation between difference in ELO and success")

ggplot(problemCorr, aes(y = corUser, x = problemRating)) + geom_jitter() + geom_smooth() + xlab("problem rating") + ylab("Correlation between player's ELO and success")

#nulltagged = subset(problemFinal, problemFinal$tag == "NULL")
#chess_data_date = select(chess_data, tactics_problem_id, create_date) %>% distinct()
nulltagged = left_join(nulltagged, chess_data_date)
head(nulltagged)
nrow(nulltagged)


ggplot(nulltagged, aes(x = create_date)) + geom_histogram()+ ggtitle("NULL tagged create date")
ggplot(chess_data_date, aes(x = create_date)) + geom_histogram() + ggtitle("all data create date")

ggplot(nulltagged, aes(x = valueProb)) + geom_histogram()
ggplot(nulltagged, aes(x = attempts)) + geom_histogram()
ggplot(nulltagged, aes(x = average_seconds)) + geom_histogram()

problemTime = problemFinal
problemTime$tag = NULL
problemTime = distinct(problemTime)
ggplot(problemTime, aes(average_seconds)) + geom_histogram(binwidth = 5) + xlim(0, 250)
ggplot(problemTime, aes(x = move_count, y = average_seconds, group = move_count)) + geom_boxplot() + ylim(0, 250)

problemFinal = subset(problemFinal, problemFinal$attempts > 1000)
head(problemFinal)
nrow(problemFinal)

chess_data = subset(chess_data, chess_data$tactics_problem_id %in% problemFinal$tactics_problem_id)
head(chess_data)
nrow(chess_data)
colnames(chess_data)

#chess_data_date = select(chess_data, tactics_problem_id, create_date) %>% distinct()

temp = chess_data %>% group_by(tactics_problem_id, diff_rounded) %>% summarise(success_rate = mean(is_passed), n = n())



temp = subset(temp, temp$n > 100)
head(temp)

temp2 = temp %>% group_by(tactics_problem_id) %>% summarise(correlation = cor(diff_rounded, success_rate), numBins = n())
temp2 = subset(temp2, temp2$numBins > 8)

nrow(temp2)
head(temp2)
#re-connect w problem rating and plot wrt to that?

temp2 = problemFinal %>% select(tactics_problem_id, valueProb, attempts, move_count, average_seconds) %>% distinct %>% right_join(temp2) 

ggplot(temp2, aes(valueProb, correlation)) + geom_jitter() + geom_smooth()
#correlation is now measured between rating bins
#At least 100 values in each bin and at least 8 bins AND at least 1000 attempts overall to not get dropped

temp2 %>% subset(temp2$correlation < .5) %>% left_join(problemFinal) %>% head(100)

#116737, 78661, and 27776 are tagged NULL

temp2 %>% subset(temp2$correlation < .5) %>% left_join(problemFinal) %>% filter(! tag=="NULL") %>% select(tactics_problem_id, valueProb, attempts, move_count, average_seconds, correlation, numBins) %>% distinct %>% head(10)

#ggplot(temp, aes(x = problem_rounded, y = diff_rounded, color = success_rate)) + geom_jitter()
