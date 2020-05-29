library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
#Read CSV
chess_data = fread("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00",nrows=1,data.table = FALSE,
                   showProgress = TRUE)
cols <- colnames(chess_data)
#Add a column to chess_data that included the total number of games played for each user
chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = chess_data[-1,]

#Load in rows from the CSV in n sized chunks until there are at least user_amount number of users in table that have played between
#1000 and 2000 games
user_amount = 5000
#Max num of records for chess_data
max_num_records = 10000000
#Step amount
n=100000
i = 0
user_num = 0
while(user_num < user_amount & nrow(chess_data) < max_num_records)
{
  user_num = length(unique(chess_data$user_hash))
  if(i%%1000000 == 0){
    print(paste("Unique Users:",user_num))
  }
  temp = fread("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00",nrows=n,skip=i,data.table=FALSE)
  colnames(temp) <- cols
  #Consider getting rid of this line and just using usergamesplayed instead
  #temp = temp %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
  temp = subset(temp,temp$userGamesPlayed > 1000 & temp$userGamesPlayed < 2000)
  chess_data <- rbind(chess_data,temp)
  i = i + n
}

#Create an elo column based on the last ratingUser value for each user
user_elo = chess_data %>% group_by(user_hash) %>% summarise(elo = last(ratingUser))

#Filter out only attempts where the user passed
#passed_problems = subset(enough_data,temp$is_passed == 1 || temp$is_passed == 0)
chess_data = select(chess_data,user_hash,tactics_problem_id,correct_move_count)

#Load Problem Data
problemData = fread("/w/225/1/chess/tactics/tag_data_updated.csv",data.table = FALSE,showProgress = TRUE)
problemData$X1 = NULL

tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

problemData = select(problemData,c(tags,"tactics_problem_id","move_count"))

chess_data = left_join(chess_data,problemData,"tactics_problem_id")

#Add a column move_acc that contains the accuracy for each attempt at the puzzle
chess_data$move_acc = chess_data$correct_move_count/chess_data$move_count

#Create Matrix of Tags and users in order to store average accuaracy for tags
vectorized_tags = data.frame(matrix(ncol = 1, nrow = nrow(user_elo)))
vectorized_tags[,1] = user_elo[,1]
colnames(vectorized_tags) = c("user_hash")


#Average accuracy for each tag and then join with vectorized tags by user
for(i in 1:length(tags)){
  tag = tags[i]
  tag_data = subset(chess_data,chess_data[[tag]] == TRUE)
  tag_acc = select(tag_data,user_hash,move_acc) %>% group_by(user_hash) %>% summarise(strength_tag = mean(move_acc))
  colnames(tag_acc) = c("user_hash",tag)
  vectorized_tags = left_join(vectorized_tags,tag_acc,"user_hash")
}

#Replace Empty Values with Zeros
vectorized_tags[is.na(vectorized_tags)] <- 0

#Remove Row names of vectorized_tags
vectorized_tags = vectorized_tags %>% remove_rownames %>% column_to_rownames(var="user_hash")

pca_results <- prcomp(vectorized_tags, center = TRUE,scale. = TRUE)

#Plot Scree Plot 
std_dev <- pca_results$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b",  xlim = c(1, 20))

#Plot First Two Principal Components and color coat by user_elo
scores = as.data.frame(pca_results$x) 
scores$user_hash = row.names(scores)
scores = left_join(scores, user_elo)
p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = elo)) + 
  geom_point(size=2) + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
  )
p

