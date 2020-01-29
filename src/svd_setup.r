#lots of models, including a random forest 
#Set up for SVD, as well as analysis of it
#including clustering and tool to find most/least similar problems

library(survival)
library(tidyr)
library(ggplot2)
#library(plyr)
library(dplyr)
library(readr)
library(survminer)
library(rlang)

chess_data = chess_data_backup


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

mround <- function(x,base){
  base*round(x/base)
} 

chess_data = subset(chess_data, chess_data$ratingDiff < 350)
chess_data$ratingDiffRounded = mround(chess_data$ratingDiff, 100)
chess_data$ratingDiffRoundedChar = as.character(chess_data$ratingDiffRounded)

surv_object = Surv(time = chess_data$seconds, event = chess_data$is_passed)

fit.coxph <- coxph(surv_object ~ ratingDiffRounded, 
                   data = chess_data)
ggforest(fit.coxph, data = ovarian)

######

chess_data = chess_data_backup

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 2000) 
#chess_data = subset(chess_data, chess_data$totalGamesPlayed < 10000)
chess_data = subset(chess_data, chess_data$totalGamesPlayed < 50000)


first_games = subset(chess_data, chess_data$userGamesPlayed <= 50)
chess_data = subset(chess_data, chess_data$userGamesPlayed > 50)
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL

#attatch tags to problems
chess_data = left_join(chess_data, problemData)
data1 = read_csv("/h/224/subramanian/user_elo_for_each_tag.csv")

tags = c("Decoy / Deflection", "Double", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

#restrict to people who attempted at least X attempts in tag

for(i in 1:length(tags)){
  
  tag = tags[i]
  chess_data_tag = subset(chess_data, chess_data[[tag]] == TRUE)
  chess_data_tag = select(chess_data_tag, user_tactics_problem_id, user_hash, is_passed, ratingProblem)
  chess_data_tag$tag = tag  
  to_select = c(tag, "user_hash", "elo")
  data1_tag = select(data1, to_select)
  colnames(data1_tag) = c("tag_elo", "user_hash", "elo") 
  chess_data_tag = left_join(chess_data_tag, data1_tag)
  chess_data_tag$ratingDiff = chess_data_tag$ratingProblem - chess_data_tag$tag_elo
  
  if(i == 1){
    df = chess_data_tag
  } else {
    df = rbind(df, chess_data_tag)
  }
  print(i)
    
}

smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]



d = sum(train$is_passed == 1) - sum(train$is_passed == 0)
if(d > 0){
  train = train[-sample(which(train$is_passed==1), d),]
}
if(d < 0){
  train = train[-sample(which(train$is_passed==0), abs(d)),]
}

d = sum(test$is_passed == 1) - sum(test$is_passed == 0)
if(d > 0){
  test = test[-sample(which(test$is_passed==1), d),]
}
if(d < 0){
  test = test[-sample(which(test$is_passed==0), abs(d)),]
}
train$ratingDiff = train$tag_elo - train$elo
test$ratingDiff = test$tag_elo - test$elo
model1 = glm(data = train, family = "binomial", is_passed~tag_elo+ratingProblem)
model2 = glm(data = train, family = "binomial", is_passed~ratingDiff+tag_elo+ratingProblem)
model3 = glm(data = train, family = "binomial", is_passed~elo+ratingProblem)


test$m1 = predict(model1, newdata = test, type = "response")
test$m2 = predict(model2, newdata = test, type = "response")
test$m3 = predict(model3, newdata = test, type = "response")

test = test[!is.na(test$m2),]

#tag_elo + problem_rating .47096
a = mean(abs(test$is_passed - test$m1))
#tag_elo + problem_rating + diff between elo and tag elo .47086
b = mean(abs(test$is_passed - test$m2))
#user elo + rating problem #.47416
c = mean(abs(test$is_passed - test$m3))

print(c(a, b, c))

###

chess_data = chess_data_backup


chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 2000) 
#chess_data = subset(chess_data, chess_data$totalGamesPlayed < 10000)
chess_data = subset(chess_data, chess_data$totalGamesPlayed < 50000)


first_games = subset(chess_data, chess_data$userGamesPlayed <= 50)
chess_data = subset(chess_data, chess_data$userGamesPlayed > 50)
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser


mround <- function(x,base){
  base*round(x/base)
} 
#set problems >= 150 seconds to 151 seconds
#remove  problrms != 3 moves

chess_data= chess_data %>% ungroup() %>% group_by(tactics_problem_id) %>% mutate(mc = max(correct_move_count))
chess_data = subset(chess_data, chess_data$mc == 3)

#chess_data$seconds[chess_data$seconds >= 150] <- 151


chess_data = ungroup(chess_data) %>%  select( ratingDiff, ratingUser, ratingProblem, seconds, is_passed)
chess_data$userRatingRounded = mround(chess_data$ratingUser, 100)
chess_data$problemRatingRounded = mround(chess_data$ratingProblem, 100)
chess_data = ungroup(chess_data)

cd2 <- chess_data[0,]
cd2 = select(cd2, userRatingRounded, problemRatingRounded, seconds, is_passed)

chess_data = chess_data %>% filter(userRatingRounded >= 1400, userRatingRounded <= 2300, problemRatingRounded >= 1400, problemRatingRounded <= 2300)


###
for(i in 1:36){
  time = i * 5  
  temp = subset(chess_data, chess_data$seconds < time)
  temp = select(temp, userRatingRounded, problemRatingRounded, seconds, is_passed)
  temp$seconds = time - 5
  cd2 = rbind(cd2, temp)
  print(i)
}
cd2 = cd2 %>% group_by(userRatingRounded, problemRatingRounded, seconds) %>% summarise(success = mean(is_passed))

#cd2 = cd2 %>% filter(userRatingRounded > 1000, userRatingRounded < 3000, problemRatingRounded > 1000, problemRatingRounded < 3000)

ggplot(cd2, aes(x = seconds, y = success)) + geom_line() + facet_grid(cols = vars(userRatingRounded), rows = vars(problemRatingRounded)) 
     

cd3 = cd2 %>% filter(seconds < 50)
#ggplot(cd3, aes(x = seconds, y = success)) + geom_line() + facet_grid(cols = vars(userRatingRounded), rows = vars(problemRatingRounded)) 

ggplot(cd3, aes(x = seconds, y = success)) + geom_line() + facet_grid(cols = vars(userRatingRounded), rows = vars(problemRatingRounded)) + 
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

##

cd2 <- chess_data[0,]
cd2 = select(cd2, userRatingRounded, problemRatingRounded, seconds, is_passed)

for(i in 1:6){
  time = i * 30
  temp = subset(chess_data, chess_data$seconds < time)
  temp = select(temp, userRatingRounded, problemRatingRounded, seconds, is_passed)
  temp$seconds = time - 30
  cd2 = rbind(cd2, temp)
}
cd2 = cd2 %>% group_by(userRatingRounded, problemRatingRounded, seconds) %>% summarise(success = mean(is_passed))

cd2 = cd2 %>% filter(userRatingRounded > 1000, userRatingRounded < 3000, problemRatingRounded > 1000, problemRatingRounded < 3000)

ggplot(cd2, aes(x = problemRatingRounded, y = userRatingRounded, fill = success)) + geom_tile() + facet_wrap(.~seconds) + scale_fill_gradient(low="red", high="green") + labs(x = "problem rating", y = "user rating")

##
#OK now we're gonna redo the embedding stuff, but hopefully smarter?

chess_data = chess_data_backup

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 2000) 


problemAttempts = chess_data %>% group_by(tactics_problem_id) %>% summarise(pa = n(), elo = last(ratingProblem))
problemAttempts = problemAttempts %>% filter(elo > 1600, elo < 1800, pa > 4300)
#198 problems
chess_data = left_join(chess_data, problemAttempts)
chess_data = subset(chess_data, !is.na(chess_data$elo))

#for people who got a correct, what percentage got b correct?

temp = select(chess_data, tactics_problem_id, user_hash, is_passed)
temp = spread(temp, tactics_problem_id, is_passed)

#for people who have attempted each pair of problems, what was the mean success on a, b, and both combined

df = data.frame(matrix(,nrow = 3 * (ncol(temp)-1) ** 2, ncol = 4))
df = data.frame(matrix(,nrow = 0, ncol = 5))

colnames(df) = c("problem1", "problem2", "condition", "success", "n")
k = 0

t = list(c())

#started at 5:50pm, should be done in ~20 hours (run last two lines)
for(i in 2:ncol(temp)){
  print((i-1)/ncol(temp))
  problem1Name = colnames(temp)[i]
  temp2 = temp %>% filter(!is.na(!!sym(problem1Name)))
  for(j in 2:ncol(temp)){
    k = k + 1
    problem2Name = colnames(temp)[j]
    t[[as.character(k)]] =  temp2 %>% filter(!is.na(!!sym(problem2Name)) ) %>% group_by((!!sym(problem1Name))) %>% summarise(n = n(), problem1 = problem1Name, problem2 = problem2Name, success = mean(!!sym(problem2Name))) %>% mutate(condition = !!sym(problem1Name)) %>% select( problem1, problem2, condition, success, n) 
  }
}
t2 = rbindlist(t) 

t2_diff = t2 %>% group_by(problem1, problem2) %>%  filter(problem1!=problem2)%>%summarise(diff = max(success) - min(success))

#write.csv(t2, file = "conditional_problem_success.csv")
#View(t2)
#t2 %>% filter(problem1 != problem2) %>% View()

library(tidytext)
library(cluster)
t3 = t2 %>% group_by(problem1, problem2) %>%  filter(problem1!=problem2)%>%summarise(diff = max(success) - min(success)) %>% cast_sparse(problem1, problem2, diff) 

pmi_svd <- irlba(t3, 2, maxit = 500)
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(t3)

#grab 100 words
forplot<-as.data.frame(word_vectors)
forplot$tactics_problem_id<-as.numeric(rownames(forplot))

forplot = left_join(forplot, select(problemAttempts, -pa))


#now plot
library(ggplot2)
ggplot(forplot, aes(x=V1, y=V2, label=tactics_problem_id, color=elo))+
  geom_text(aes(label=tactics_problem_id),hjust=0, vjust=0)+
  theme_minimal()+
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD")


#take forplot
#join all problem tags
problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData$X1 = NULL
forplot2 = left_join(forplot, problemData)
#for tag in tags:
fp = list()
for(i in 1:length(tags)){
  tag = tags[i]
  fp[[as.character(i)]] = forplot2 %>% filter((!!sym(tag)) == 1) %>% summarise(tag = tag, v1 = mean(V1), v2 = mean(V2))
}
fp2 = rbindlist(fp) 

fp3 = forplot %>% select(-elo)
fp3$type = "problem"
colnames(fp3) = c("V1", "V2", "id", "type")
fp2$type = "tag"
colnames(fp2) = c("id", "V1", "V2", "type")

fp4 = rbind(fp3, fp2)


ggplot()+ geom_point(data = forplot, alpha = 1/10, aes(x=V1, y=V2)) + 
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD") +  
  geom_text(aes(V1, V2, label = id), data = fp2, size = 2)

#plot with that tag in one color and all else in another
#or just get the values of SV1 and 2

#ok the interesting tags seem to be:
tags2 = c("Defense", "Mate in 3+", "Mate in 1", "Back Rank", "Pin")

tagnum = 5
tag = tags2[tagnum]

forplot3 = select(forplot2, V1, V2, tactics_problem_id, !!sym(tag)) %>% filter(!is.na(!!sym(tag)))

ggplot() + geom_point(data = forplot3 %>% filter(!!sym(tag) == FALSE), aes(V1, V2), color = "blue", alpha = 1/10) + geom_text(color = "Red", size = 3, aes(V1, V2, label = tactics_problem_id), data = forplot3 %>% filter(!!sym(tag) == TRUE)) + ggtitle(tag)


library(corrplot)
corrplot(as.matrix(t3), type = "full", order = "hclust", method = "color",
         tl.col = "black", tl.srt = 90, cl.lim = c(0, 1), tl.cex = .8)

x = pam(as.matrix(t3), k = 6)
View(x$clustering)


t4 = svd(t3)
word_vectors <- t4$u
rownames(word_vectors) <- rownames(t3)
library(broom)

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}
search_synonyms(word_vectors, word_vectors["905",]) %>% View()
