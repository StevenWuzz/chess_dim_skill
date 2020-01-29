#first foray into tag dimensionality
#Checks how users do above/below expected on each tag
#And then check if that's consistent across the data (do users stay bad/good)
#clustered tags based on how users did on them (hclust created was a cool plot)
#Also did a lot of analysis on how predictive tag strength is on success 
#as well as predictive ability of correlation between tag success

library(tidyr)
library(ggplot2)
#library(plyr)
library(dplyr)
library(readr)
library(randomForest)
library(cluster)



chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")


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

#see most common tags
#temp = gather(chess_data, "tag", "tf", -c(user_tactics_problem_id, user_hash, create_date, date, last_game_date, seconds, is_passed, correct_move_count, rating_change, tactics_problem_id, ratingUser, varianceUser, ratingProblem, varianceProblem, userGamesPlayed, totalGamesPlayed, predicted_success_rate, point_diff, average_seconds, move_count))
#temp2 = subset(temp, temp$tf)
#temp = NULL
#ggplot(temp2, aes(tag)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#For each tag, controlling for ELO, see if people do better or worse than expected.

#chosen tags are defense and mate in 3+
tags = c("Decoy / Deflection", "Double", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")
#tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")



df <- data.frame(matrix(ncol = 3, nrow = 45))
x <- c("tag1", "tag2", "dissimilarity")
colnames(df) <- x

k = 0

for(i in 1:length(tags)){
  for(j in i+1:length(tags)){
    if(j > length(tags)){
      break
    }
    k = k + 1
    print(k)
    first1 = tags[i]
    second1 = tags[j]
    
    tag1 = subset(chess_data, chess_data[[first1]] == TRUE)
    tag2 = subset(chess_data, chess_data[[second1]] == TRUE)
    
    #tag1 = subset(chess_data, chess_data$Pin == TRUE)
    #tag2 = subset(chess_data, chess_data$Zwischenzug == TRUE)
    
    title_str = paste0("Comparison between '",first1,"' and '",second1,"' tags")
    
    user_tag_success_1 = tag1 %>% group_by(user_hash) %>% summarise(tag_affect_1 = mean(point_diff))
    user_tag_success_2 = tag2 %>% group_by(user_hash) %>% summarise(tag_affect_2 = mean(point_diff))
    
    user_tag_diff = inner_join(user_tag_success_1, user_tag_success_2)

    #control
    #add control of number of rows picked of each type
    if(nrow(tag1) > nrow(tag2)){
      smp_size <- nrow(tag2)
      train_ind <- sample(seq_len(nrow(tag1)), size = smp_size)
      tag1_2 <- tag1[train_ind, ]
      control_set = rbind(tag1_2, tag2)
    } else{
      smp_size <- nrow(tag1)
      train_ind <- sample(seq_len(nrow(tag2)), size = smp_size)
      tag2_2 <- tag2[train_ind, ]
      control_set = rbind(tag1, tag2_2)      
    }
    
    
    
    #remove repeats
    control_set<-control_set[dim(control_set)[1]:1,]
    temp = duplicated(control_set)
    control_set<-control_set[dim(control_set)[1]:1,]
    control_set$repeated = duplicated(control_set) & temp
    control_set = subset(control_set, control_set$repeated == FALSE)
    
    
    
    smp_size <- floor(0.5 * nrow(control_set))
    train_ind <- sample(seq_len(nrow(control_set)), size = smp_size)
    tag1_control <- control_set[train_ind, ]
    tag2_control <- control_set[-train_ind, ]
    print("b")
    user_tag_success_1_control = tag1_control %>% group_by(user_hash) %>% summarise(tag_affect_1 = mean(point_diff))
    user_tag_success_2_control = tag2_control %>% group_by(user_hash) %>% summarise(tag_affect_2 = mean(point_diff))
    
    user_tag_diff_control = inner_join(user_tag_success_1_control, user_tag_success_2_control)
    
    #analysis
    user_tag_diff_control$diff = user_tag_diff_control$tag_affect_1 - user_tag_diff_control$tag_affect_2
    user_tag_diff$diff = user_tag_diff$tag_affect_1 - user_tag_diff$tag_affect_2
    user_tag_diff_control$control = "control"
    user_tag_diff$control = "analysis"
    
    user_tag_diff_combined = rbind(user_tag_diff, user_tag_diff_control)
    
    a = ggplot(user_tag_diff_combined, aes(diff)) + geom_histogram() + xlim(-.3, .3) + xlab("difference in tag success") + ggtitle(title_str) + facet_wrap(aes(group = control))
    
    #0 means exactly the same
    skill_overlap = var(user_tag_diff$diff)-var(user_tag_diff_control$diff)
    print("x")
    df[k,1] = first1
    df[k,2] = second1
    df[k,3] = skill_overlap
    
  }
}


ggplot(data = df, aes(x=tag1, y=tag2, fill=dissimilarity)) + geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

####

tag1 = subset(chess_data, chess_data$Sacrifice == TRUE)
#> brier_control
#[1] 0.2285266
#> brier_actual
#[1] 0.2282172

tag1 = subset(chess_data, chess_data$`Decoy / Deflection` == TRUE)


tag1 = tag1 %>% group_by(user_hash) %>% mutate(userGamesPlayed = row_number())#, totalGamesPlayed = last(userGamesPlayed))
tag1 = tag1 %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))

last_100 = tag1 %>% group_by(user_hash) %>% filter(userGamesPlayed > .9*totalGamesPlayed)
tag1 = tag1 %>% group_by(user_hash) %>% filter(userGamesPlayed <= .9*totalGamesPlayed)

user_tag_success_1 = tag1 %>% group_by(user_hash) %>% summarise(tag_affect_1 = mean(point_diff))
tag1 = left_join(tag1, user_tag_success_1)

last_100 = left_join(last_100, user_tag_success_1)

model1 = glm(data = tag1, formula = is_passed~ratingUser+ratingProblem+tag_affect_1)
model2 = glm(data = tag1, formula = is_passed~ratingUser+ratingProblem)

library(DescTools)

last_100$control_predictions = predict.glm(model2, last_100)
last_100$predictions = predict.glm(model1, last_100)

last_100 = subset(last_100, ! is.na(last_100$predictions))

brier_control = BrierScore(last_100$is_passed, last_100$control_predictions)
brier_actual = BrierScore(last_100$is_passed, last_100$predictions)


####

#Ok so now I want to test if people who under-perform in the first half 

#step one, split it into half, calculate how bad they were and how many games they played, final ELO

#step two, 

####
tag1 = subset(chess_data, chess_data$Double == TRUE)

tag1 = tag1 %>% group_by(user_hash) %>% mutate(userGamesPlayed = row_number())#, totalGamesPlayed = last(userGamesPlayed))
tag1 = tag1 %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))

second_half = tag1 %>% group_by(user_hash) %>% filter(userGamesPlayed > .5*totalGamesPlayed)
first_half = tag1 %>% group_by(user_hash) %>% filter(userGamesPlayed <= .5*totalGamesPlayed)
last_10 = tag1 %>% group_by(user_hash) %>% filter(userGamesPlayed > totalGamesPlayed-11)

user_sucess_first_half= first_half %>% group_by(user_hash) %>% summarise(tag_affect_initial = mean(point_diff), userGamesPlayed = last(userGamesPlayed))
user_sucess_second_half= second_half %>% group_by(user_hash) %>% summarise(tag_affect_second_half= mean(point_diff))
user_sucess_last_10= last_10 %>% group_by(user_hash) %>% summarise(tag_affect_last_10 = mean(point_diff))

user_sucess_combined = inner_join(user_sucess_first_half, user_sucess_second_half)
user_sucess_combined = inner_join(user_sucess_combined, user_sucess_last_10)

model1 = glm(data = user_sucess_combined, tag_affect_second_half~tag_affect_initial)
model2 = glm(data = user_sucess_combined, tag_affect_last_10~tag_affect_initial)
model3 = glm(data = user_sucess_combined, tag_affect_last_10~tag_affect_initial+userGamesPlayed)

####
library(lawstat)

tags = c("Decoy / Deflection", "Double", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

df <- data.frame(matrix(ncol = 3, nrow = 45))
x <- c("tag1", "tag2", "dissimilarity")
colnames(df) <- x

k = 0

#for(i in 1:length(tags)){
#  for(j in i+1:length(tags)){
    if(j > length(tags)){
      break
    }
    k = k + 1
    print(k)
    first1 = tags[i]
    second1 = tags[j]
    
    tag1 = subset(chess_data, chess_data[[first1]] == TRUE)
    tag2 = subset(chess_data, chess_data[[second1]] == TRUE)
    
    #tag1 = subset(chess_data, chess_data$Pin == TRUE)
    #tag2 = subset(chess_data, chess_data$Zwischenzug == TRUE)
    
    title_str = paste0("Comparison between '",first1,"' and '",second1,"' tags")
    
    user_tag_success_1 = tag1 %>% group_by(user_hash) %>% summarise(tag_affect_1 = mean(point_diff))
    user_tag_success_2 = tag2 %>% group_by(user_hash) %>% summarise(tag_affect_2 = mean(point_diff))
    
    user_tag_diff = inner_join(user_tag_success_1, user_tag_success_2)
    
    #control
    #add control of number of rows picked of each type
    if(nrow(tag1) > nrow(tag2)){
      smp_size <- nrow(tag2)
      train_ind <- sample(seq_len(nrow(tag1)), size = smp_size)
      tag1_2 <- tag1[train_ind, ]
      control_set = rbind(tag1_2, tag2)
    } else{
      smp_size <- nrow(tag1)
      train_ind <- sample(seq_len(nrow(tag2)), size = smp_size)
      tag2_2 <- tag2[train_ind, ]
      control_set = rbind(tag1, tag2_2)      
    }
    
    
    
    #remove repeats
    control_set<-control_set[dim(control_set)[1]:1,]
    temp = duplicated(control_set)
    #control_set<-control_set[dim(control_set)[1]:1,]
    control_set$repeated = duplicated(control_set) & temp
    control_set = subset(control_set, control_set$repeated == FALSE)
    
    
    
    smp_size <- floor(0.5 * nrow(control_set))
    train_ind <- sample(seq_len(nrow(control_set)), size = smp_size)
    tag1_control <- control_set[train_ind, ]
    tag2_control <- control_set[-train_ind, ]
    print("b")
    user_tag_success_1_control = tag1_control %>% group_by(user_hash) %>% summarise(tag_affect_1 = mean(point_diff))
    user_tag_success_2_control = tag2_control %>% group_by(user_hash) %>% summarise(tag_affect_2 = mean(point_diff))
    
    user_tag_diff_control = inner_join(user_tag_success_1_control, user_tag_success_2_control)
    
    #analysis
    user_tag_diff_control$diff = user_tag_diff_control$tag_affect_1 - user_tag_diff_control$tag_affect_2
    user_tag_diff$diff = user_tag_diff$tag_affect_1 - user_tag_diff$tag_affect_2
    user_tag_diff_control$control = "control"
    user_tag_diff$control = "analysis"
    
    user_tag_diff_combined = rbind(user_tag_diff, user_tag_diff_control)
    
    a = ggplot(user_tag_diff_combined, aes(diff)) + geom_histogram() + xlim(-.3, .3) + xlab("difference in tag success") + ggtitle(title_str) + facet_wrap(aes(group = control))
    
    #0 means exactly the same
    skill_overlap = var(user_tag_diff$diff)-var(user_tag_diff_control$diff)
    print("x")
    df[k,1] = first1
    df[k,2] = second1
    df[k,3] = skill_overlap
    
#  }
#}


ggplot(data = df, aes(x=tag1, y=tag2, fill=dissimilarity)) + geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###


#calculate performance on all tags
i = 1
#tags = c("Decoy / Deflection", "Double", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")
tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")
#tags = c("Defense", "Endgame Tactics", "Hanging Piece", "Pin", "Sacrifice", "Trapped Piece")

t2 = chess_data %>% filter(`Back Rank`+`Basic Checkmates`+`Decoy / Deflection`+`Defense`+`Discovered Attack`+`Mate in 1`+`Zwischenzug`+`Mate in 2`+`Endgame Tactics`+`Double`+`Trapped Piece`+`Simplification`+`Fork / Double Attack`+`Hanging Piece`+`Mate in 3+`+`Mating Net`+`Pin`+`Remove the Defender`+`Sacrifice`+`Vulnerable King` == 1)
problems_to_consider = unique(t2$tactics_problem_id)
#
t = data.frame(problems_to_consider, numeric(length(problems_to_consider)))
colnames(t) = c("tactics_problem_id", "zero")
t = left_join(chess_data, t)
t = subset(t, !is.na(t$zero))


#

tag_name = tags[i]
tag_subset = subset(t, t[[tag_name]] == TRUE)
tag_subset = tag_subset %>% group_by(user_hash) %>% mutate(first_half = row_number()/n() <= .75, last_10 = n()-row_number()<11)
last_10 = subset(tag_subset, tag_subset$last_10 == TRUE)
second_half = subset(tag_subset, tag_subset$first_half == FALSE)
tag_subset = subset(tag_subset, tag_subset$first_half == TRUE)
df = tag_subset %>% group_by(user_hash) %>% summarise(tag_affect = mean(point_diff))
df2 = second_half %>% group_by(user_hash) %>% summarise(tag_affect = mean(point_diff))
df10 = last_10 %>% group_by(user_hash) %>% summarise(tag_affect = mean(point_diff))
df_gp = tag_subset %>% group_by(user_hash) %>% summarise(games_played = n())

colnames(df) = c("user_hash", tag_name)
colnames(df_gp) = c("user_hash",paste0(tag_name, "_games_played"))
colnames(df10) = c("user_hash", tag_name)
colnames(df2) = c("user_hash",tag_name)


for(i in 2:length(tags)){
  tag_name = tags[i]
  print(tag_name)
  tag_subset = subset(chess_data, chess_data[[tag_name]] == TRUE)
  tag_subset = tag_subset %>% group_by(user_hash) %>% mutate(first_half = row_number()/n() <= .75, last_10 = n()-row_number()<11)
  last_10 = subset(tag_subset, tag_subset$last_10 == TRUE)
  second_half = subset(tag_subset, tag_subset$first_half == FALSE)
  tag_subset = subset(tag_subset, tag_subset$first_half == TRUE)
  user_tag_success_1 = tag_subset %>% group_by(user_hash) %>% summarise(tag_affect = mean(point_diff))
  user_tag_success_2 = tag_subset %>% group_by(user_hash) %>% summarise( games_played = n())
  df2_2 = second_half %>% group_by(user_hash) %>% summarise(tag_affect = mean(point_diff))
  df10_2 = last_10 %>% group_by(user_hash) %>% summarise(tag_affect = mean(point_diff))
  colnames(user_tag_success_1) = c("user_hash", tag_name)
  colnames(user_tag_success_2) = c("user_hash",paste0(tag_name, "_games_played"))
  colnames(df2_2) = c("user_hash", tag_name)
  colnames(df10_2) = c("user_hash",tag_name)
  df = left_join(df, user_tag_success_1)
  df_gp = left_join(df_gp, user_tag_success_2)
  df2 = left_join(df2, df2_2)
  df10 = left_join(df10, df10_2)
}

too_few_games = df_gp %>% gather("a", "b", 2:ncol(df_gp))
too_few_games = subset(too_few_games, too_few_games$b<25)
too_few_games = unique(too_few_games$user_hash)
df =  subset(df,!( df$user_hash %in% too_few_games))
df2 =  subset(df2,!( df2$user_hash %in% too_few_games))
df10 =  subset(df10,!( df10$user_hash %in% too_few_games))


# iterate through tags
# run RF and linear model on df10$tag ~ df[all]

list_of_models = vector("list", length(tags))
list_of_t_values = vector("list", length(tags))


for(i in 1:length(tags)){
  tag_name = tags[i]
  print(tag_name)
  temp = df2 %>% select(user_hash, tag_name)
  dependent_name = paste0(tag_name,"_last_10")
  colnames(temp) = c("user_hash", dependent_name)
  temp = left_join(df, temp)
  gp_name = paste0(tag_name, "_games_played")
  temp2 = df_gp %>% select(user_hash, gp_name)
  temp = left_join(temp, temp2)
  temp$user_hash = NULL
  t = as.formula(paste0("`", dependent_name, "`", "~."))
  model_problem = lm(formula = t, data = temp)
  #list_of_t_values[[i]] = summary(model_problem)$coef[, "t value"] 
  list_of_t_values[[i]] = summary(model_problem)$coef[, "Estimate"] #############
  list_of_models[[i]] = model_problem
  #summary(model_problem)
  #fit=randomForest(as.factor(dependent_name)~., data=temp)
}


mat = matrix(, nrow = length(tags), ncol = length(list_of_t_values[[1]]))

for(i in 1:length(tags)){
  mat[i,] = list_of_t_values[[i]]
}

mat = data.frame(mat)
names = colnames(list_of_models[[i]]$model)
names[1] = "intercept"
names[length(names)] = "games of tag played"
colnames(mat) = names
rownames(mat) = tags
mat$intercept = NULL
mat$`games of tag played` = NULL

mat2 = mat
for(i in 1:nrow(mat)){
  mat2[i,i] = NA
}

dt2 <- mat2 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "grey" ) + ylab("independent variable") + xlab("dependent variable") + labs(fill="t value")


#hierarchical clustering of tags
plot(hclust(dist(1-as.matrix(mat)), method="single"), xlab = "", ylab = "")

#Partitioning Around Medoids
x = pam(as.matrix(mat), k = 6)
View(x$clustering)
#d <- dist(as.matrix(df))  
#hc <- hclust(d)
#plot(hc)       
#clusterCut <- cutree(hc, 10)

#km = kmeans(df[,-1], 10)

#df is control set

tags = c("Defense", "Endgame Tactics", "Hanging Piece", "Pin", "Sacrifice", "Trapped Piece")
chess_data$ratingDiff = chess_data$ratingUser-chess_data$ratingProblem
data2 = chess_data %>% select(user_hash, is_passed, Defense, `Endgame Tactics`, `Hanging Piece`, Pin, Sacrifice, `Trapped Piece`, ratingDiff, userGamesPlayed)
validation = data2 %>% group_by(user_hash) %>% filter(row_number()/n() > .75)
data2 = data2 %>% group_by(user_hash) %>% filter(row_number()/n() <= .75)


colnames(validation) = c("user_hash", "is_passed", "Defense_rating", "Endgame_Tactics_Rating", "Hanging_Piece_Rating","Pin_Rating","Sacrifice_Rating","Trapped_Piece_Rating", "ratingDiff","userGamesPlayed")
colnames(data2) = c("user_hash", "is_passed", "Defense_rating", "Endgame_Tactics_Rating", "Hanging_Piece_Rating","Pin_Rating","Sacrifice_Rating","Trapped_Piece_Rating", "ratingDiff","userGamesPlayed")

validation = left_join(validation, df)
validation$userGamesPlayed = NULL  
data2 = left_join(data2, df)
data2$userGamesPlayed = NULL  
data2 = subset(data2, ! is.na(data2$Pin_Rating))
validation = subset(validation, ! is.na(validation$Pin_Rating))

#model_rf <- randomForest(is_passed ~ ., data = data2[,-1], importance = TRUE)
#model_rf

#predictions <- predict(model_rf, validation, type = "class")
#table(predictions, validation$is_passed)  

####

#check how many problems have one common tag and not another
t = chess_data %>% group_by(tactics_problem_id) %>% filter(Zwischenzug + Simplification >= 1) %>% summarise(both = last(Zwischenzug) + last(Simplification) > 1,z = last(Zwischenzug), s = last(Simplification))
nrow(t) - sum(t$both)
sum(t$z)
sum(t$s)


t2 = chess_data %>% filter(`Back Rank`+`Basic Checkmates`+`Decoy / Deflection`+`Defense`+`Discovered Attack`+`Mate in 1`+`Zwischenzug`+`Mate in 2`+`Endgame Tactics`+`Double`+`Trapped Piece`+`Simplification`+`Fork / Double Attack`+`Hanging Piece`+`Mate in 3+`+`Mating Net`+`Pin`+`Remove the Defender`+`Sacrifice`+`Vulnerable King` == 1)
problems_to_consider = unique(t2$tactics_problem_id)

#figure out range of time in dataset
#pick range and figure out how it impacts the data.
#specifically, how many users we'll drop down to for the previous study
#also test for create date of puzzle

create_dates=chess_data %>% group_by(tactics_problem_id) %>% summarise(create_date = last(create_date))
ggplot(create_dates, aes(create_date)) + geom_histogram()
ggplot(chess_data, aes(date)) + geom_histogram()

#pick two correlated tags (simplification and defense) and two un-correlated tags (Pin and Mate in 3+)
#get dataset of just those two problems
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

#make sure users have at least 500(!) problems in both tags
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
    subset_tag1_close = subset(subset_tag1, abs(subset_tag1$ratingDiff) < 30)
    #subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) > 30)
    subset_tag2_close = subset(subset_tag2, abs(subset_tag2$ratingDiff) < 30)
    #subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) > 30)
    subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) >= 30)
    subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) <= 40)
    subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) >= 30)
    subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) <= 40)
#figure out how many people have >10 close attempts of each
    t1 = subset_tag1_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t2 = subset_tag2_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t3 = inner_join(t1, t2, by = "user_hash")
    
    if(nrow(t3) == 0){
      df[row_num,] = c(tag1, tag2, 0,  NA, NA)   
      row_num = row_num + 1
      df[row_num,] = c(tag2, tag1, 0,  NA, NA)   
      row_num = row_num + 1
      next
    }
    #subset into just those users
    t3$n.y = NULL
    t3$n.x = 1
    subset_tag1_close = left_join(subset_tag1_close, t3)
    subset_tag1_other = left_join(subset_tag1_other, t3)
    subset_tag2_close = left_join(subset_tag2_close, t3)
    subset_tag2_other = left_join(subset_tag2_other, t3)
    subset_tag1_close = subset(subset_tag1_close, subset_tag1_close$n.x == 1)
    subset_tag1_other = subset(subset_tag1_other, subset_tag1_other$n.x == 1)
    subset_tag2_close = subset(subset_tag2_close, subset_tag2_close$n.x == 1)
    subset_tag2_other = subset(subset_tag2_other, subset_tag2_other$n.x == 1)
#take 10 random attempts from each of those
    validation_1 = subset_tag1_close %>% group_by(user_hash) %>% sample_n(10)
    validation_2 = subset_tag2_close %>% group_by(user_hash) %>% sample_n(10)
    subset_tag1_close = anti_join(subset_tag1_close, validation_1)
    subset_tag2_close = anti_join(subset_tag2_close, validation_2)
    #add rest back to dataset
    train_set_1 = rbind(subset_tag1_close, subset_tag1_other)
    train_set_2 = rbind(subset_tag2_close, subset_tag2_other)
#calculate how people did on those tags alone
    t1 = train_set_1 %>% group_by(user_hash) %>% summarise(tag1_ability = mean(point_diff))
    t2 = train_set_2 %>% group_by(user_hash) %>% summarise(tag2_ability = mean(point_diff))
    
###only look at users in the margins
   t1 = ungroup(t1)
   t2 = ungroup(t2)
   
   #looking at top/bottom 5% for a given tag

   t1 = mutate(t1, decile_rank_1 = ntile(t1$tag1_ability,20))
   t2 = mutate(t2, decile_rank_2 = ntile(t2$tag2_ability,20))
   t1 = left_join(t1, t2)
   t1$diff_decile = t1$decile_rank_1 - t1$decile_rank_2
    train_set_1 = left_join(train_set_1, t1)
    train_set_2 = left_join(train_set_2, t1)
  validation_1 = left_join(validation_1, t1)
   validation_2 = left_join(validation_2, t1)
   
  #train_set_1 = subset(train_set_1, train_set_1$decile_rank_1 %in% c(1,20))
   #validation_1 = subset(validation_1, validation_1$decile_rank_1 %in% c(1, 20))
   #train_set_2 = subset(train_set_2, train_set_2$decile_rank_2 %in% c(1, 20))
   #validation_2 = subset(validation_2, validation_2$decile_rank_2 %in% c(1, 20))
   
   train_set_1 = subset(train_set_1, train_set_1$diff_decile %in% c( 19,  -19))
   validation_1 = subset(validation_1, validation_1$diff_decile %in% c( 19,  -19))
   train_set_2 = subset(train_set_2, train_set_2$diff_decile %in% c( 19, -19))
   validation_2 = subset(validation_2, validation_2$diff_decile %in% c( 19, -19))

###
    
    #t1 = left_join(t1, t2)
    #looking at top/bottom 5% of difference in tag success rates
  #  t1$diff = t1$tag1_ability-t1$tag2_ability
   # t1 = mutate(t1, decile_rank = ntile(t1$diff,20))
    #
    #train_set_1 = left_join(train_set_1, t1)
    #train_set_2 = left_join(train_set_2, t1)
    #validation_1 = left_join(validation_1, t1)
    #validation_2 = left_join(validation_2, t1)

###only look at users in the margins
    
    #train_set_1 = subset(train_set_1, train_set_1$decile_rank %in% c(1, 20))
    #validation_1 = subset(validation_1, validation_1$decile_rank %in% c(1, 20))
    #train_set_2 = subset(train_set_2, train_set_2$decile_rank %in% c(1, 20))
    #validation_2 = subset(validation_2, validation_2$decile_rank %in% c(1, 20))
        
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


######code above is more efficient and better written/commented


'''correlated_tag_subset = chess_data %>% filter(Simplification + Defense == 1)
correlated_tag_subset = correlated_tag_subset %>% group_by(user_hash) %>% mutate(validation=row_number()>.75*n())
validation = subset(correlated_tag_subset, correlated_tag_subset$validation == TRUE)
correlated_tag_subset = subset(correlated_tag_subset, correlated_tag_subset$validation == FALSE)

user_info = correlated_tag_subset %>% group_by(user_hash, Defense) %>% summarise(games_played = n(), tag_affect = mean(point_diff))
user_info_1 = subset(user_info, user_info$Defense ==TRUE)
user_info_2 = subset(user_info, user_info$Defense == FALSE)

user_info_1 = subset(user_info_1, user_info_1$games_played > 30)
user_info_2 = subset(user_info_2, user_info_2$games_played > 30)
colnames(user_info_1) = c("user_hash", "t", "gp_defense", "tag_affect_defense")
user_info_1$t = NULL
colnames(user_info_2) = c("user_hash", "t", "gp_simplification", "tag_affect_simplification")
user_info_2$t = NULL
user_info = left_join(user_info_1, user_info_2)
user_info = subset(user_info, !is.na(user_info$gp_simplification))

correlated_tag_subset = left_join(correlated_tag_subset, user_info)
correlated_tag_subset = subset(correlated_tag_subset,!is.na(correlated_tag_subset$tag_affect_simplification))
validation = left_join(validation, user_info)
validation = validation %>% filter(! is.na(tag_affect_simplification))

t1 = subset(correlated_tag_subset, correlated_tag_subset$Defense == TRUE)
model1 = glm(data = t1, family = "binomial", is_passed~ratingUser+ratingProblem+tag_affect_simplification+tag_affect_defense)
model2 = glm(data = t1, family = "binomial",  is_passed~ratingUser+ratingProblem+tag_affect_defense)
model3 = glm(data = t1, family = "binomial",  is_passed~ratingUser+ratingProblem)
t2 = filter(validation, Defense == TRUE)
t2$pred1 = predict(model1, newdata =t2, type = "response")
t2$pred2 = predict(model2, newdata =t2, type = "response")
t2$pred3 = predict(model3, newdata =t2, type = "response")

error1 = mean(abs(t2$pred1 - t2$is_passed)) #0.4442826
error2 = mean(abs(t2$pred2 - t2$is_passed)) #0.4443026
error3 = mean(abs(t2$pred3 - t2$is_passed)) #0.4457467

t1 = subset(correlated_tag_subset, correlated_tag_subset$Defense == FALSE)
model1 = glm(data = t1, family = "binomial", is_passed~ratingUser+ratingProblem+tag_affect_simplification+tag_affect_defense)
model2 = glm(data = t1, family = "binomial",is_passed~ratingUser+ratingProblem+tag_affect_simplification)
model3 = glm(data = t1, family = "binomial",is_passed~ratingUser+ratingProblem)
t2 = filter(validation, Defense == FALSE)
t2$pred1 = predict(model1, newdata =t2, type = "response")
t2$pred2 = predict(model2, newdata =t2, type = "response")
t2$pred3 = predict(model3, newdata =t2, type = "response")

error1 = mean(abs(t2$pred1 - t2$is_passed)) #0.4564969
error2 = mean(abs(t2$pred2 - t2$is_passed)) # 0.4565285
error3 = mean(abs(t2$pred3 - t2$is_passed)) #0.45857

##now lets do it again


correlated_tag_subset = chess_data %>% filter(Pin + `Mate in 3+` == 1)
correlated_tag_subset = correlated_tag_subset %>% group_by(user_hash) %>% mutate(validation=row_number()>.75*n())
validation = subset(correlated_tag_subset, correlated_tag_subset$validation == TRUE)
correlated_tag_subset = subset(correlated_tag_subset, correlated_tag_subset$validation == FALSE)

user_info = correlated_tag_subset %>% group_by(user_hash, Pin) %>% summarise(games_played = n(), tag_affect = mean(point_diff))
user_info_1 = subset(user_info, user_info$Pin ==TRUE)
user_info_2 = subset(user_info, user_info$Pin == FALSE)

user_info_1 = subset(user_info_1, user_info_1$games_played > 30)
user_info_2 = subset(user_info_2, user_info_2$games_played > 30)
colnames(user_info_1) = c("user_hash", "t", "gp_pin", "tag_affect_pin")
user_info_1$t = NULL
colnames(user_info_2) = c("user_hash", "t", "gp_mate_in_3", "tag_affect_mate_in_3")
user_info_2$t = NULL
user_info = left_join(user_info_1, user_info_2)
user_info = subset(user_info, !is.na(user_info$gp_mate_in_3))

correlated_tag_subset = left_join(correlated_tag_subset, user_info)
correlated_tag_subset = subset(correlated_tag_subset,!is.na(correlated_tag_subset$tag_affect_mate_in_3))
validation = left_join(validation, user_info)
validation = validation %>% filter(! is.na(tag_affect_mate_in_3))

t1 = subset(correlated_tag_subset, correlated_tag_subset$Pin == TRUE)
model1 = glm(data = t1, family = "binomial", is_passed~ratingUser+ratingProblem+tag_affect_mate_in_3+tag_affect_pin)
model2 = glm(data = t1, family = "binomial",  is_passed~ratingUser+ratingProblem+tag_affect_pin)
model3 = glm(data = t1, family = "binomial",  is_passed~ratingUser+ratingProblem)
t2 = filter(validation, Pin == TRUE)
t2$pred1 = predict(model1, newdata =t2, type = "response")
t2$pred2 = predict(model2, newdata =t2, type = "response")
t2$pred3 = predict(model3, newdata =t2, type = "response")

error1 = mean(abs(t2$pred1 - t2$is_passed)) #0.4494755
error2 = mean(abs(t2$pred2 - t2$is_passed)) #0.4494714
error3 = mean(abs(t2$pred3 - t2$is_passed)) #0.4506257

t1 = subset(correlated_tag_subset, correlated_tag_subset$Pin == FALSE)
model1 = glm(data = t1, family = "binomial", is_passed~ratingUser+ratingProblem+tag_affect_mate_in_3+tag_affect_pin)
model2 = glm(data = t1, family = "binomial",is_passed~ratingUser+ratingProblem+tag_affect_mate_in_3)
model3 = glm(data = t1, family = "binomial",is_passed~ratingUser+ratingProblem)
t2 = filter(validation, Pin == FALSE)
t2$pred1 = predict(model1, newdata =t2, type = "response")
t2$pred2 = predict(model2, newdata =t2, type = "response")
t2$pred3 = predict(model3, newdata =t2, type = "response")

error1 = mean(abs(t2$pred1 - t2$is_passed)) # 0.4544905
error2 = mean(abs(t2$pred2 - t2$is_passed)) # 0.4544805
error3 = mean(abs(t2$pred3 - t2$is_passed)) # 0.4555583
'''

df = data.frame(matrix(,nrow = length(tags) * (length(tags)-1), ncol = 10))
colnames(df) = c("tag1", "tag2", "n_users", "MAE",  "Intercept",   "tag1_ability_p_value", "interaction_term_tag_1_p_value","tag2_ability_p_value", "interaction_term_2_p_value, random_guessing") 
                 
tags = c("Decoy / Deflection", "Double", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

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
    correlated_tag_subset = chess_data %>% filter((!!as.symbol(tag1)) + (!!as.symbol(tag2)) == 1)
    subset_tag1 = subset(correlated_tag_subset, correlated_tag_subset[[tag1]] == TRUE)
    subset_tag2 = subset(correlated_tag_subset, correlated_tag_subset[[tag2]] == TRUE)
    
    #make sure users have at least 500(!) problems in both tags
    t1 = subset_tag1 %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 100)
    t2 = subset_tag2 %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 100)
    t3 = inner_join(t1, t2, by = "user_hash")
    if(nrow(t3) == 0){
      df[row_num,] = c(tag1, tag2, 0,  NA, NA, NA, NA, NA, NA, NA)   
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
    
    subset_tag1_close = subset(subset_tag1, abs(subset_tag1$ratingDiff) < 15)
    #subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) >= 30)
    subset_tag2_close = subset(subset_tag2, abs(subset_tag2$ratingDiff) < 15)
    #subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) >= 30)
    subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) >= 15)
    subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) <= 40)
    subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) >= 15)
    subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) <= 40)
    
    
    #figure out how many people have >10 close attempts of each
    t1 = subset_tag1_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t2 = subset_tag2_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t3 = inner_join(t1, t2, by = "user_hash")
    
    if(nrow(t3) == 0){
      df[row_num,] = c(tag1, tag2, 0,  NA, NA, NA, NA, NA, NA, NA)   
      row_num = row_num + 1
      next
    }
    #subset into just those users
    t3$n.y = NULL
    t3$n.x = 1
    subset_tag1_close = left_join(subset_tag1_close, t3)
    subset_tag1_other = left_join(subset_tag1_other, t3)
    subset_tag2_close = left_join(subset_tag2_close, t3)
    subset_tag2_other = left_join(subset_tag2_other, t3)
    subset_tag1_close = subset(subset_tag1_close, subset_tag1_close$n.x == 1)
    subset_tag1_other = subset(subset_tag1_other, subset_tag1_other$n.x == 1)
    subset_tag2_close = subset(subset_tag2_close, subset_tag2_close$n.x == 1)
    subset_tag2_other = subset(subset_tag2_other, subset_tag2_other$n.x == 1)
    #take 10 random attempts from each of those
    validation_1 = subset_tag1_close %>% group_by(user_hash) %>% sample_n(10)
    validation_2 = subset_tag2_close %>% group_by(user_hash) %>% sample_n(10)
    subset_tag1_close = anti_join(subset_tag1_close, validation_1)
    subset_tag2_close = anti_join(subset_tag2_close, validation_2)
    #add rest back to dataset
    train_set_1 = rbind(subset_tag1_close, subset_tag1_other)
    train_set_2 = rbind(subset_tag2_close, subset_tag2_other)
    #calculate how people did on those tags alone
    t1 = train_set_1 %>% group_by(user_hash) %>% summarise(tag1_ability = mean(point_diff))
    t2 = train_set_2 %>% group_by(user_hash) %>% summarise(tag2_ability = mean(point_diff))
    t1 = ungroup(t1)
    t2 = ungroup(t2)
    t1 = mutate(t1, decile_rank_1 = ntile(t1$tag1_ability,20))
    t2 = mutate(t2, decile_rank_2 = ntile(t2$tag2_ability,20))
    t1 = left_join(t1, t2)
    t1$diff_decile = t1$decile_rank_1 - t1$decile_rank_2
    train_set_1 = left_join(train_set_1, t1)
    train_set_2 = left_join(train_set_2, t1)
    validation_1 = left_join(validation_1, t1)
    validation_2 = left_join(validation_2, t1)
    
    #train_set_1 = subset(train_set_1, train_set_1$decile_rank_1 %in% c(1,20))
    #validation_1 = subset(validation_1, validation_1$decile_rank_1 %in% c(1, 20))
    #train_set_2 = subset(train_set_2, train_set_2$decile_rank_2 %in% c(1, 20))
    #validation_2 = subset(validation_2, validation_2$decile_rank_2 %in% c(1, 20))
    
    train_set_1 = subset(train_set_1, train_set_1$diff_decile %in% c( 19,  -19))
    validation_1 = subset(validation_1, validation_1$diff_decile %in% c( 19,  -19))
    train_set_2 = subset(train_set_2, train_set_2$diff_decile %in% c( 19, -19))
    validation_2 = subset(validation_2, validation_2$diff_decile %in% c( 19, -19))
    
    train_set = rbind(train_set_1, train_set_2)  
    validation_set = rbind(validation_1, validation_2)
    
    d = sum(validation_set$is_passed == 1) - sum(validation_set$is_passed == 0)
    if(d > 0){
      validation_set = validation_set[-sample(which(validation_set$is_passed==1), d),]
    }
    if(d < 0){
      validation_set = validation_set[-sample(which(validation_set$is_passed==0), abs(d)),]
    }
    
    d = sum(train_set$is_passed == 1) - sum(train_set$is_passed == 0)
    if(d > 0){
      train_set = train_set[-sample(which(train_set$is_passed==1), d),]
    }
    if(d < 0){
      train_set = train_set[-sample(which(train_set$is_passed==0), abs(d)),]
    }
    
    
    train_set$is_tag_1 = train_set[[tag1]] == TRUE
    train_set$is_tag_2 = train_set[[tag2]] == TRUE
    validation_set$is_tag_1 = validation_set[[tag1]] == TRUE
    validation_set$is_tag_2 = validation_set[[tag2]] == TRUE
    validation_set$interaction_term_1 = validation_set$tag1_ability*validation_set$is_tag_1
    validation_set$interaction_term_2 = validation_set$tag2_ability*validation_set$is_tag_2
    train_set$interaction_term_1 = train_set$tag1_ability*train_set$is_tag_1
    train_set$interaction_term_2 = train_set$tag2_ability*train_set$is_tag_2
    
    
    model = as.formula("is_passed ~ tag1_ability+ interaction_term_1 + tag2_ability + interaction_term_2")
    model_1 = glm(data=train_set, model, family = binomial())
    n = length(unique(validation_set$user_hash))
    validation_set$model_1 = predict(model_1, newdata = validation_set, type = "response")
    if(n == 1){
      df[row_num,] = c(tag1, tag2, n,  mean(abs(validation_set$is_passed - validation_set$model_1)), NA, NA, NA, NA, NA, NA)   
      
    }else{
    df[row_num,] = c(tag1, tag2, n,  mean(abs(validation_set$is_passed - validation_set$model_1)), summary(model_1)$coefficients[,4], mean(validation_set$is_passed))   
    }
    row_num = row_num + 1
    
  }}



df = data.frame(matrix(,nrow = 10, ncol = 10))
colnames(df) = c("tag1", "tag2", "n_users", "MAE",  "Intercept",   "tag1_ability_p_value", "interaction_term_tag_1_p_value","tag2_ability_p_value", "interaction_term_2_p_value", "percentile") 
tags = c("Remove the Defender", "Sacrifice")
row_num = 1
#step one, iterate through all pairs of tags
for(i in 1:10){
    tag1 = tags[1]
    tag2 = tags[2]
    print(paste(tag1, ",", tag2, ",", row_num))

    correlated_tag_subset = chess_data %>% filter((!!as.symbol(tag1)) + (!!as.symbol(tag2)) == 1)
    subset_tag1 = subset(correlated_tag_subset, correlated_tag_subset[[tag1]] == TRUE)
    subset_tag2 = subset(correlated_tag_subset, correlated_tag_subset[[tag2]] == TRUE)
    
    #make sure users have at least 500(!) problems in both tags
    t1 = subset_tag1 %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 100)
    t2 = subset_tag2 %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 100)
    t3 = inner_join(t1, t2, by = "user_hash")
    if(nrow(t3) == 0){
      df[row_num,] = c(tag1, tag2, 0,  NA, NA, NA, NA, NA, NA, NA)   
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
    
    subset_tag1_close = subset(subset_tag1, abs(subset_tag1$ratingDiff) < 15)
    #subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) >= 30)
    subset_tag2_close = subset(subset_tag2, abs(subset_tag2$ratingDiff) < 15)
    #subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) >= 30)
    subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) >= 15)
    subset_tag2_other = subset(subset_tag2, abs(subset_tag2$ratingDiff) <= 40)
    subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) >= 15)
    subset_tag1_other = subset(subset_tag1, abs(subset_tag1$ratingDiff) <= 40)
    
    
    #figure out how many people have >10 close attempts of each
    t1 = subset_tag1_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t2 = subset_tag2_close %>% group_by(user_hash) %>% summarise(n = n()) %>% filter(n > 10)
    t3 = inner_join(t1, t2, by = "user_hash")
    
    if(nrow(t3) == 0){
      df[row_num,] = c(tag1, tag2, 0,  NA, NA, NA, NA, NA, NA, NA)   
      row_num = row_num + 1
      next
    }
    #subset into just those users
    t3$n.y = NULL
    t3$n.x = 1
    subset_tag1_close = left_join(subset_tag1_close, t3)
    subset_tag1_other = left_join(subset_tag1_other, t3)
    subset_tag2_close = left_join(subset_tag2_close, t3)
    subset_tag2_other = left_join(subset_tag2_other, t3)
    subset_tag1_close = subset(subset_tag1_close, subset_tag1_close$n.x == 1)
    subset_tag1_other = subset(subset_tag1_other, subset_tag1_other$n.x == 1)
    subset_tag2_close = subset(subset_tag2_close, subset_tag2_close$n.x == 1)
    subset_tag2_other = subset(subset_tag2_other, subset_tag2_other$n.x == 1)
    #take 10 random attempts from each of those
    validation_1 = subset_tag1_close %>% group_by(user_hash) %>% sample_n(10)
    validation_2 = subset_tag2_close %>% group_by(user_hash) %>% sample_n(10)
    subset_tag1_close = anti_join(subset_tag1_close, validation_1)
    subset_tag2_close = anti_join(subset_tag2_close, validation_2)
    #add rest back to dataset
    train_set_1 = rbind(subset_tag1_close, subset_tag1_other)
    train_set_2 = rbind(subset_tag2_close, subset_tag2_other)
    #calculate how people did on those tags alone
    t1 = train_set_1 %>% group_by(user_hash) %>% summarise(tag1_ability = mean(point_diff))
    t2 = train_set_2 %>% group_by(user_hash) %>% summarise(tag2_ability = mean(point_diff))
    t1 = ungroup(t1)
    t2 = ungroup(t2)
    t1 = mutate(t1, decile_rank_1 = ntile(t1$tag1_ability,20))
    t2 = mutate(t2, decile_rank_2 = ntile(t2$tag2_ability,20))
    t1 = left_join(t1, t2)
    t1$diff_decile = t1$decile_rank_1 - t1$decile_rank_2
    train_set_1 = left_join(train_set_1, t1)
    train_set_2 = left_join(train_set_2, t1)
    validation_1 = left_join(validation_1, t1)
    validation_2 = left_join(validation_2, t1)
    
    #train_set_1 = subset(train_set_1, train_set_1$decile_rank_1 %in% c(1,20))
    #validation_1 = subset(validation_1, validation_1$decile_rank_1 %in% c(1, 20))
    #train_set_2 = subset(train_set_2, train_set_2$decile_rank_2 %in% c(1, 20))
    #validation_2 = subset(validation_2, validation_2$decile_rank_2 %in% c(1, 20))
    
    percentile_to_subset = c(-19:((20-i)*-1), c((20-i):19))
    
    train_set_1 = subset(train_set_1, train_set_1$diff_decile %in% percentile_to_subset) #c( 19,  -19))
    validation_1 = subset(validation_1, validation_1$diff_decile %in% percentile_to_subset) #c( 19,  -19))
    train_set_2 = subset(train_set_2, train_set_2$diff_decile %in% percentile_to_subset) #c( 19, -19))
    validation_2 = subset(validation_2, validation_2$diff_decile %in% percentile_to_subset) #c( 19, -19))
    
    train_set = rbind(train_set_1, train_set_2)  
    validation_set = rbind(validation_1, validation_2)
    
    d = sum(validation_set$is_passed == 1) - sum(validation_set$is_passed == 0)
    if(d > 0){
      validation_set = validation_set[-sample(which(validation_set$is_passed==1), d),]
    }
    if(d < 0){
      validation_set = validation_set[-sample(which(validation_set$is_passed==0), abs(d)),]
    }
    
    d = sum(train_set$is_passed == 1) - sum(train_set$is_passed == 0)
    if(d > 0){
      train_set = train_set[-sample(which(train_set$is_passed==1), d),]
    }
    if(d < 0){
      train_set = train_set[-sample(which(train_set$is_passed==0), abs(d)),]
    }
    
    
    train_set$is_tag_1 = train_set[[tag1]] == TRUE
    train_set$is_tag_2 = train_set[[tag2]] == TRUE
    validation_set$is_tag_1 = validation_set[[tag1]] == TRUE
    validation_set$is_tag_2 = validation_set[[tag2]] == TRUE
    validation_set$interaction_term_1 = validation_set$tag1_ability*validation_set$is_tag_1
    validation_set$interaction_term_2 = validation_set$tag2_ability*validation_set$is_tag_2
    train_set$interaction_term_1 = train_set$tag1_ability*train_set$is_tag_1
    train_set$interaction_term_2 = train_set$tag2_ability*train_set$is_tag_2
    
    
    model = as.formula("is_passed ~ tag1_ability+ interaction_term_1 + tag2_ability + interaction_term_2")
    model_1 = glm(data=train_set, model, family = binomial())
    n = length(unique(validation_set$user_hash))
    validation_set$model_1 = predict(model_1, newdata = validation_set, type = "response")
    if(n == 1){
      df[row_num,] = c(tag1, tag2, n,  mean(abs(validation_set$is_passed - validation_set$model_1)), NA, NA, NA, NA, NA, NA)   
      
    }else{
      df[row_num,] = c(tag1, tag2, n,  mean(abs(validation_set$is_passed - validation_set$model_1)), summary(model_1)$coefficients[,4], i*5)   
    }
    row_num = row_num + 1
}
plot(df$percentile, df$MAE, main = paste(tag1, "and", tag2))



######################3
#This stuff is just to see how people did on one tag

tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

df = data.frame(matrix(,nrow = length(tags), ncol = 5))
colnames(df) = c("tag", "z_value", "MAE_model", "MAE_baseline", "n_users")

for(i in 1:length(tags)){
  tag1 = tags[i]
  data_tag1 = subset(chess_data, chess_data[[tag1]] == TRUE)
  last_100 = subset(data_tag1, data_tag1$userGamesPlayed > data_tag1$totalGamesPlayed - 11)
  data_tag1 =  subset(data_tag1, data_tag1$userGamesPlayed <= data_tag1$totalGamesPlayed - 11)
  user_strength = data_tag1 %>% group_by(user_hash) %>% summarise(tag_strength = mean(point_diff), user_attempts = n())
  data_tag1 = left_join(data_tag1, user_strength)
  last_100 = left_join(last_100, user_strength)
  data_tag1 = subset(data_tag1, data_tag1$user_attempts > 100)
  last_100 = subset(last_100, last_100$user_attempts > 100)
  
  
  d = sum(last_100$is_passed == 1) - sum(last_100$is_passed == 0)
  if(d > 0){
    last_100 = last_100[-sample(which(last_100$is_passed==1), d),]
  }
  if(d < 0){
    last_100 = last_100[-sample(which(last_100$is_passed==0), abs(d)),]
  }
  
  d = sum(data_tag1$is_passed == 1) - sum(data_tag1$is_passed == 0)
  if(d > 0){
    data_tag1 = data_tag1[-sample(which(data_tag1$is_passed==1), d),]
  }
  if(d < 0){
    data_tag1 = data_tag1[-sample(which(data_tag1$is_passed==0), abs(d)),]
  }
  
 #not rating user, rating diff
  mdl = glm(data=data_tag1, is_passed ~ tag_strength+ ratingDiff, family = binomial())
  mdl_baseline = glm(data=data_tag1, is_passed ~ ratingDiff, family = binomial())
  
  n = length(unique(last_100$user_hash))
  
  last_100$mdl = predict(mdl, newdata = last_100, type = "response")
  last_100$mdl_baseline = predict(mdl_baseline, newdata = last_100, type = "response")
  
  last_100 = last_100[!is.na(last_100$mdl),]
  
  a = mean(abs(last_100$is_passed - last_100$mdl))
  b = mean(abs(last_100$is_passed - last_100$mdl_baseline))

  df[i, ] = c(tag1, summary(mdl)$coefficients[3,3], a, b, n)
}
df$MAE_model = as.numeric(df$MAE_model)
df$MAE_baseline = as.numeric(df$MAE_baseline)
df$z_value = as.numeric(df$z_value)

df$improvement = df$MAE_model - df$MAE_baseline


#this makes the cool plot that measures MAE as you take a more extreme subset of users
#tags = c("Endgame Tactics", "Defense", "Trapped Piece", "Basic Checkmates", "Simplification")
tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")


df = data.frame(matrix(,nrow = 500, ncol = 6))
colnames(df) = c("tag", "percentile", "z_value", "MAE_model", "MAE_baseline", "n_users")
k = 0

for(j in 1:length(tags)){


  tag1 = tags[j]
  data_tag1 = subset(chess_data, chess_data[[tag1]] == TRUE)
  last_100 = subset(data_tag1, data_tag1$userGamesPlayed > data_tag1$totalGamesPlayed - 11)
  data_tag1 =  subset(data_tag1, data_tag1$userGamesPlayed <= data_tag1$totalGamesPlayed - 11)
  user_strength = data_tag1 %>% group_by(user_hash) %>% summarise(tag_strength = mean(point_diff), user_attempts = n())
  user_strength = subset(user_strength, user_strength$user_attempts > 100)
  user_strength = user_strength %>% mutate(PCT = ntile(user_strength$tag_strength, 20))
  
  
  data_tag1 = left_join(data_tag1, user_strength)
  last_100 = left_join(last_100, user_strength)
  data_tag1 = data_tag1[!is.na(data_tag1$PCT),]
  
  


for(i in 1:10){
  k = k + 1
  pct_included = c(1:i, (20-i+1):20)
  data_tag2 = subset(data_tag1, data_tag1$PCT %in% pct_included)
  last_100_2 = subset(last_100, last_100$PCT %in% pct_included)
  
  
  
  d = sum(last_100_2$is_passed == 1) - sum(last_100_2$is_passed == 0)
  if(d > 0){
    last_100_2 = last_100_2[-sample(which(last_100_2$is_passed==1), d),]
  }
  if(d < 0){
    last_100_2 = last_100_2[-sample(which(last_100_2$is_passed==0), abs(d)),]
  }
  
  d = sum(data_tag2$is_passed == 1) - sum(data_tag2$is_passed == 0)
  if(d > 0){
    data_tag2 = data_tag2[-sample(which(data_tag2$is_passed==1), d),]
  }
  if(d < 0){
    data_tag2 = data_tag2[-sample(which(data_tag2$is_passed==0), abs(d)),]
  }
  
  #not rating user, rating diff
  mdl = glm(data=data_tag2, is_passed ~ tag_strength+ ratingDiff, family = binomial())
  mdl_baseline = glm(data=data_tag2, is_passed ~ ratingDiff, family = binomial())
  
  n = length(unique(last_100_2$user_hash))
  
  last_100_2$mdl = predict(mdl, newdata = last_100_2, type = "response")
  last_100_2$mdl_baseline = predict(mdl_baseline, newdata = last_100_2, type = "response")
  
  last_100_2 = last_100_2[!is.na(last_100_2$mdl),]
  
  a = mean(abs(last_100_2$is_passed - last_100_2$mdl))
  b = mean(abs(last_100_2$is_passed - last_100_2$mdl_baseline))
  
  df[k, ] = c(tag1, i*5, summary(mdl)$coefficients[3,3], a, b, n)

}}
df$percentile = as.numeric(df$percentile)
df$MAE_model = as.numeric(df$MAE_model)
df$MAE_baseline = as.numeric(df$MAE_baseline)
df$improvement = df$MAE_model - df$MAE_baseline

ggplot(data = df, aes(x = percentile, y = MAE_model, color = tag)) + geom_point() + geom_line() + ylab("MAE")
ggplot(data = df, aes(x = percentile, y = improvement, color = tag)) + geom_point() + geom_line() + ylab("MAE")

ggplot(data = df, aes(x = percentile, y = MAE_baseline, color = tag)) + geom_point() + geom_line() + ylab("MAE")


#write.csv(df, file = "mae_plot_all.csv")
