#A variety of last minute analyses for the initial publication attempt
#A number of plots followed by an analysis on time taken per puzzle

data1 = read_csv("/h/224/subramanian/user_elo_for_each_tag.csv")
#data1 = read_csv("/h/224/subramanian/user_elo_for_each_tag_randomized.csv")

data1 = subset(data1, data1$n > 500)

most_common_tags = c("Back Rank", "Basic Checkmates","Decoy / Deflection", "Defense", "Discovered Attack","Mate in 1","Zwischenzug", "Mate in 2", "Endgame Tactics","Double","Trapped Piece","Simplification", "Fork / Double Attack", "Hanging Piece", "Mate in 3+", "Mating Net", "Pin", "Remove the Defender", "Sacrifice", "Vulnerable King")

columns_to_keep = c("user_hash", "n", "elo", most_common_tags)

data1 = data1[,columns_to_keep]

data1$variance = 0
for(i in 1:nrow(data1)){
  data1$variance[i]  = var(as.numeric(data1[i,4:24]), na.rm = TRUE)
}


data2 = select(data1, user_hash, `Back Rank`:`Vulnerable King`)



data2 = data2 %>% remove_rownames %>% column_to_rownames(var="user_hash")
pca_results <- prcomp(data2, center = TRUE,scale. = TRUE)
pca_results_user_tag = pca_results

std_dev <- pca_results$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

scores = as.data.frame(pca_results$x) 
scores$user_hash = row.names(scores)

user_elo = select(data1, user_hash, elo)

scores = left_join(scores, user_elo)
p <- ggplot(data = scores, aes(x = PC1, y = PC2, color = elo)) + 
  geom_point(size=2) + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  coord_fixed(ratio=1, xlim=range(scores$PC1), ylim=range(scores$PC2)
  )
p 


#############
matrix1 = cor(data2)


library(corrplot)
corrplot(matrix1, type = "full", order = "hclust", method = "color",
         tl.col = "black", tl.srt = 90, cl.lim = c(0, 1), tl.cex = .8)

#############
#creating new chess_data with just a few users
data1 = select(data1, user_hash, n)
chess_data = left_join(chess_data, data1)
chess_data = select(chess_data, user_hash, n, tactics_problem_id, date, is_passed)
chess_data = subset(chess_data, chess_data$n > 500)

problemRatings = read_csv("/h/224/subramanian/problem_final_ratings.csv")
chess_data = left_join(chess_data, problemRatings)
chess_data = subset(chess_data, !is.na(chess_data$problemRating))

write.csv(x = chess_data, file = "chess_data_top_users.csv")


###########

variances = read_csv("variance_bootstrap.csv")
variances$is_bootstrap = ifelse(variances$bootstrap == 0, "actual", "bootstrap")
ggplot(variances, aes(group = is_bootstrap, y = variance, x= is_bootstrap)) + geom_boxplot(outlier.shape = NA)+coord_cartesian(ylim = quantile(variances$variance, c(0.001, 0.997))) + theme(axis.title.x=element_blank())


###########
user_tag_elos = read_csv("user_tag_elos_for_cor_matrix.csv")
bootstrap_tag_elos = read_csv("bootstrap_tag_elos_for_cor_matrix.csv")

#
user_tag_elos$means = rowMeans(user_tag_elos)
bootstrap_tag_elos$means = rowMeans(bootstrap_tag_elos)
user_tag_elos = user_tag_elos - user_tag_elos$means
user_tag_elos$means = NULL
bootstrap_tag_elos = bootstrap_tag_elos - bootstrap_tag_elos$means
bootstrap_tag_elos$means = NULL
#

data1 = cor(user_tag_elos, method = "pearson")
data2 = cor(bootstrap_tag_elos, method = "pearson")

data3 = cor(user_tag_elos, method = "spearman")
data4 = cor(bootstrap_tag_elos, method = "spearman")#takes a while, avoid if possible

mat1 = data1-data2 #Note: bootstrap has HIGHER correlations #Note not true after subtracting mean?
corrplot(mat1, type = "full", order = "hclust", method = "color",
         tl.col = "black", tl.srt = 90, tl.cex = .8) 

mat2 = data3-data4 
corrplot(mat2, type = "full", order = "hclust", method = "color",
         tl.col = "black", tl.srt = 90, tl.cex = .8) 

plot(hclust(dist(1-as.matrix(mat1)), method="single"), xlab = "", ylab = "")


######



chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))

games_played = chess_data%>% group_by(user_hash) %>% summarise(userGamesPlayed = last(userGamesPlayed))
user_elo = chess_data %>% group_by(user_hash) %>% summarise(elo = last(ratingUser))
problem_elo = chess_data %>% group_by(tactics_problem_id) %>% summarise(elo = last(ratingProblem))
problem_creation = chess_data %>% group_by(tactics_problem_id) %>% summarise(first_attempt = first(date))


##
mean_variances = read_csv("variances_of_each_bootstrap.csv")

mean_variances2 = subset(mean_variances, mean_variances$Variance < 5000)
ggplot(mean_variances, aes( Variance)) + geom_histogram()


####

#OK In this part we're looking at time taken on a puzzle
chess_data = chess_data_backup
chess_data$ratingDiff=chess_data$ratingProblem - chess_data$ratingUser

chess_data = chess_data %>% group_by(user_hash) %>% mutate(totalGamesPlayed = last(userGamesPlayed))
chess_data = subset(chess_data, chess_data$totalGamesPlayed > 2500) 

chess_data= ungroup(chess_data)
timeSummary = chess_data %>% group_by(ratingDiff, is_passed) %>% summarise(timeTaken = mean(seconds)) %>% filter(abs(ratingDiff) <= 250)
ggplot(timeSummary, aes(x = ratingDiff, y = timeTaken, color = as.logical(is_passed), as.logical(fill = is_passed), group = as.logical(is_passed))) + geom_smooth() + labs(x = "Problem Rating minus User Rating", y = "Mean Time Taken", color = "Correct?")

problemData = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
problemData = select(problemData, tactics_problem_id, move_count, average_seconds)

chess_data = left_join(chess_data, problemData)
chess_data$move_count2 = ifelse(chess_data$move_count > 5, "6+", chess_data$move_count)

#fix line to do other version of plot
#timeSummary2 = chess_data %>% group_by(ratingUser, ratingProblem, is_passed, move_count2) %>% summarise(timeTaken = mean(seconds)) %>% filter(abs(ratingUser-ratingProblem) <= 250)

timeSummary2 = chess_data %>% group_by(ratingUser, ratingProblem, is_passed, move_count2) %>% summarise(timeTaken = mean(seconds)) %>% filter(abs(ratingUser-ratingProblem) <= 250)
timeSummary2 = subset(timeSummary2, !is.na(timeSummary2$move_count2))
timeSummary2$ratingDiff = timeSummary2$ratingProblem - timeSummary2$ratingUser

ggplot(timeSummary2, aes(x = ratingDiff, y = timeTaken, color = as.logical(is_passed), as.logical(fill = is_passed), group = as.logical(is_passed))) + geom_smooth() + labs(x = "Problem Rating minus User Rating", y = "Mean Time Taken", color = "Correct?") + facet_wrap(~move_count2)

#####
mround <- function(x,base){
  base*round(x/base)
} 


chess_data$ratingDiff = chess_data$ratingProblem-chess_data$ratingUser

chess_data$ratingDiffRounded = mround(chess_data$ratingDiff, 50)
chess_data$ratingUserRounded = mround(chess_data$ratingUser, 100)
chess_data$secondsRounded = mround(chess_data$seconds, 5)

#first plot is y axis diff rating, X axis time taken, color is mean success
timeSummary3 = chess_data %>% group_by(ratingDiffRounded, move_count2, secondsRounded) %>% filter(abs(ratingDiff) < 500) %>% summarise(n = n(), success = mean(is_passed) )
timeSummary3 = subset(timeSummary3, !is.na(timeSummary3$move_count2))
ggplot(timeSummary3, aes(y = ratingDiffRounded, x = secondsRounded, fill = success)) + geom_tile() + facet_wrap(.~move_count2) + xlim(c(1, 120)) + labs(x = "seconds", y = "rating difference") + scale_fill_gradientn(colours = terrain.colors(10))

#second is same thing but facetted by rating bin
timeSummary4 = chess_data %>% group_by(ratingDiffRounded, ratingUserRounded, secondsRounded) %>% filter(abs(ratingDiff) < 200, move_count2 == "3") %>% summarise(n = n(), success = mean(is_passed) )

ggplot(timeSummary4, aes(y = ratingUserRounded, x = secondsRounded, fill = success)) + geom_tile() + facet_wrap(.~ratingDiffRounded) + xlim(c(1, 120)) + labs(x = "seconds", y = "user rating") + scale_fill_gradientn(colours = terrain.colors(10)) + ylim(c(1200, 3000))
