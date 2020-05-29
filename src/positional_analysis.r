#Analysis into positional data
#We didn't end up using any of it

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(smooth)
library(broom)
library(tree)
library(randomForest)

#positional_data = read_csv("/w/225/1/chess/tactics/positional_data_depth2.csv")

#data_FENs = read_csv("/w/225/1/chess/tactics/tactics_problem.csv")

advanced = TRUE
temp1 = read_csv("/w/225/1/chess/tactics/positional_data_no_depth.csv")
temp2 = read_csv("/w/225/1/chess/tactics/positional_data_no_depth_2.csv")
temp3 = read_csv("/w/225/1/chess/tactics/positional_data_no_depth_3.csv")
temp4 = read_csv("/w/225/1/chess/tactics/positional_data_no_depth_4.csv")
temp5 = read_csv("/w/225/1/chess/tactics/positional_data_no_depth_5.csv")
positional_data_no_depth = rbind(temp1, temp2, temp3, temp4, temp5)
positional_data_no_depth = distinct(positional_data_no_depth)

temp1  = read_csv("/w/225/1/chess/tactics/positional_data_just_depth.csv")
temp2  = read_csv("/w/225/1/chess/tactics/positional_data_just_depth_2.csv")
temp3  = read_csv("/w/225/1/chess/tactics/positional_data_just_depth_3.csv")
temp4  = read_csv("/w/225/1/chess/tactics/positional_data_just_depth_4.csv")
temp5  = read_csv("/w/225/1/chess/tactics/positional_data_just_depth_5.csv")

positional_data_just_depth =  rbind(temp1, temp2, temp3, temp4, temp5)
positional_data_just_depth = distinct(positional_data_just_depth)

positional_data_just_depth$multiplier = ((positional_data_just_depth$isWhite == "True")-.5)*2
positional_data_just_depth$score = positional_data_just_depth$score * positional_data_just_depth$multiplier


##Let's get some stats
#get difference between solution strength at depth 10 and depth 1.
deff_depth_10_depth_1 = positional_data_just_depth %>% group_by(tactics_problem_id) %>% filter(isSolution == 1) %>% filter(depth %in% c(1, 10)) %>% summarise(diff_10_1 = max(score) - min(score))

#ranking of correct move
rank_correct = positional_data_just_depth %>% group_by(tactics_problem_id) %>% filter(depth == 1) %>% arrange(desc(score)) %>% mutate(rank = order(desc(score))) %>% filter(isSolution == 1)
rank_correct = rank_correct %>% select(tactics_problem_id, rank)

#Number of moves with positive stockfish eval at depth 1, 2
pos_1 = positional_data_just_depth %>% group_by(tactics_problem_id) %>% filter(depth == 1) %>% filter(score > 0) %>% summarise(num_positive_moves_1 = n())
pos_2 = positional_data_just_depth %>% group_by(tactics_problem_id) %>% filter(depth == 2) %>% filter(score > 0) %>% summarise(num_positive_moves_2 = n())
pos_5 = positional_data_just_depth %>% group_by(tactics_problem_id) %>% filter(depth == 5) %>% filter(score > 0) %>% summarise(num_positive_moves_5 = n())
pos_10 = positional_data_just_depth %>% group_by(tactics_problem_id) %>% filter(depth == 10) %>% filter(score > 0) %>% summarise(num_positive_moves_10 = n())

#Is it a backwards move?
temp = positional_data_just_depth %>% filter(isSolution==1, depth ==1)
temp$start = substr(temp$move, 2, 2)
temp$start = as.numeric(temp$start)
temp$stop = substr(temp$move, 4, 4)
temp$stop = as.numeric(temp$stop)
temp$backwards = (temp$stop < temp$start) + 0
temp$backwards = (temp$backwards + temp$multiplier) %in% c(-1, 2)
temp = temp %>% select(tactics_problem_id, backwards)



#First stat is difference between solution at depth and next best option at depth
max_score_depth = positional_data_just_depth %>% group_by(tactics_problem_id, depth, isSolution) %>% summarise(max_score = max(score))

max_score_depth = max_score_depth %>% filter(isSolution == 0)
max_score_depth$isSolution = NULL

max_score_depth = left_join(max_score_depth, subset(positional_data_just_depth, positional_data_just_depth$isSolution == 1))
max_score_depth$score_no_multiplier = NULL
max_score_depth$isWhite = NULL
max_score_depth$move = NULL
max_score_depth$isSolution = NULL
max_score_depth$multiplier = NULL
max_score_depth$difference = max_score_depth$score - max_score_depth$max_score
max_score_depth$max_score = NULL
depth_differences = max_score_depth
depth_differences$score = NULL
depth_differences = spread(depth_differences, key = depth, value = difference)
colnames(depth_differences) = c("tactics_problem_id", "score_diff_depth_1", "score_diff_depth_2", "score_diff_depth_5", "score_diff_depth_10")

#positional_data$tactics_problem_id = as.numeric(positional_data$tactics_problem_id)

chess_data = read_csv("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00")

problem_data = chess_data %>% group_by(tactics_problem_id) %>% summarise(attempts_problem = n(), successes_problem = sum(is_passed), finalRatingProblem = last(ratingProblem), first_date = first(date), last_date = last(date))
problem_data$first_date = NULL
problem_data$last_date = NULL
problem_data$successes_problem = NULL

positional_data = left_join(depth_differences, problem_data)
positional_data = left_join(positional_data, positional_data_no_depth)
positional_data$piece_to_move = tolower(positional_data$piece_to_move)

positional_data$move_count = as.numeric(positional_data$move_count)
positional_data$log_mc = log(positional_data$move_count)


msd = max_score_depth
msd$difference = NULL
msd$max_score = NULL
msd = spread(msd, key = depth, value = score)
msd$`5` = NULL
msd$`10` = NULL
colnames(msd) = c("tactics_problem_id", "score_1", "score_2")
positional_data = left_join(positional_data, msd)


positional_data = left_join(positional_data, deff_depth_10_depth_1)
positional_data = left_join(positional_data, rank_correct)
positional_data = left_join(positional_data, temp)
positional_data = left_join(positional_data, pos_1)
positional_data = left_join(positional_data, pos_2)
positional_data = left_join(positional_data, pos_5)
positional_data = left_join(positional_data, pos_10)


#if(advanced){
#positional_data$diff1 = abs(as.numeric(positional_data$scoreDepth1) - as.numeric(positional_data$bestAlt1))
#positional_data$diff2 = abs(as.numeric(positional_data$scoreDepth1) - as.numeric(positional_data$bestAlt2))
#positional_data$diff5 = abs(as.numeric(positional_data$scoreDepth1) - as.numeric(positional_data$bestAlt5))
#positional_data$diff10 = abs(as.numeric(positional_data$scoreDepth1) - as.numeric(positional_data$bestAlt10))
#}

#barebones model
lin.model = lm(finalRatingProblem~log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem, data=positional_data)

tag_data = read_csv(file = "/w/225/1/chess/tactics/tag_data_updated.csv")
tag_data$X1 = NULL
tag_data$move_count = NULL
tag_data$average_seconds = NULL
positional_data = left_join(positional_data, tag_data)


positional_data$score_dummy = positional_data$score_1 > 0
positional_data$score_dummy_2 = positional_data$score_2 > 0


#with all tags and differences
s <- lm(finalRatingProblem~backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10+diff_10_1, data = positional_data)
s_1 <- lm(finalRatingProblem~backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10+diff_10_1+num_positive_moves_1, data = positional_data)

#with all tags without differences
s2 <- lm(finalRatingProblem~log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition`, data = positional_data)

#No move count
s3 <- lm(finalRatingProblem~piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10, data = positional_data)

#with scores of the position
#s4 <- lm(finalRatingProblem~diff_10_1+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2, data = positional_data)
#s4 <- lm(finalRatingProblem~rank+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2, data = positional_data)
s4_0 <- lm(finalRatingProblem~log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2, data = positional_data)
s4_1 <- lm(finalRatingProblem~log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy, data = positional_data)
s4_2 <- lm(finalRatingProblem~log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = positional_data)

#With backwards move
#s5 <- lm(finalRatingProblem~backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = positional_data)


s5 <- lm(finalRatingProblem~backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = positional_data)
s5_1 <- lm(finalRatingProblem~backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = positional_data)


#With the number of potentially positive moves  
#s6 <- lm(finalRatingProblem~num_positive_moves_1+backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = positional_data)

s6 <- lm(finalRatingProblem~num_positive_moves_1+backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = positional_data)
s6_1 <- lm(finalRatingProblem~num_positive_moves_1+backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = positional_data)


#with a decision tree
positional_data$bin = cut(positional_data$finalRatingProblem, 3, labels=c("Low", "Med", "High"))
s7 <- tree(bin~log_mc+move_count+piece_to_move+forcing_move+num_legal_moves+attempts_problem +score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2, data = positional_data)
s8 <- tree(finalRatingProblem~num_positive_moves_1+backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves +score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2, data = positional_data, method = "anova")
s9 <- tree(finalRatingProblem~num_positive_moves_1+backwards+piece_to_move+forcing_move+num_legal_moves +score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2, data = positional_data, method = "anova")

plot(s9); text(s9)
#lin.model2 = lm(finalRatingProblem~log_mc+move_count, data=positional_data)

positional_data$piece_to_move = as.factor(positional_data$piece_to_move)



positional_data4 = positional_data %>% select(finalRatingProblem,backwards,log_mc,move_count,piece_to_move,forcing_move,num_legal_moves,`Attacking f7/f2`, `Attacking Castled King` , `Back Rank` , `Basic Checkmates` , `Exchange Sacrifice` , `Mate in 3+` , `Mating Net` , `Queen Sacrifice` , `Sacrifice` , `Vulnerable King` , `Clearance Sacrifice` , `Decoy / Deflection` , `Mate in 2` , `Discovered Attack` , `Fork / Double Attack` , `Hanging Piece` , `Double Check` , `Pin` , `Smothered Mate` , `Remove the Defender` , `Interference` , `Mate in 1` , `NULL` , `Trapped Piece` , `Pawn Promotion` , `Skewer` , `X-Ray Attack` , `Overloading` , `Simplification` , `Defense` , `Zwischenzug` , `Endgame Tactics` , `Stalemate` , `Perpetual Check` , `Desperado` , `Zugzwang` , `En passant` , `Windmill` , `Underpromotion` , `Double` , `Opposition` , score_diff_depth_1 , score_diff_depth_2 , score_diff_depth_5 , score_diff_depth_10 , score_1 , score_2)
positional_data4$tactics_problem_id = NULL
names(positional_data4) <- make.names(names(positional_data4))
bag.s5 = randomForest(finalRatingProblem~., data=positional_data4,importance=TRUE, na.action=na.omit)


positional_data3 = positional_data %>% select(finalRatingProblem,num_positive_moves_1,backwards,log_mc,move_count,piece_to_move,forcing_move,num_legal_moves,`Attacking f7/f2`, `Attacking Castled King` , `Back Rank` , `Basic Checkmates` , `Exchange Sacrifice` , `Mate in 3+` , `Mating Net` , `Queen Sacrifice` , `Sacrifice` , `Vulnerable King` , `Clearance Sacrifice` , `Decoy / Deflection` , `Mate in 2` , `Discovered Attack` , `Fork / Double Attack` , `Hanging Piece` , `Double Check` , `Pin` , `Smothered Mate` , `Remove the Defender` , `Interference` , `Mate in 1` , `NULL` , `Trapped Piece` , `Pawn Promotion` , `Skewer` , `X-Ray Attack` , `Overloading` , `Simplification` , `Defense` , `Zwischenzug` , `Endgame Tactics` , `Stalemate` , `Perpetual Check` , `Desperado` , `Zugzwang` , `En passant` , `Windmill` , `Underpromotion` , `Double` , `Opposition` , score_diff_depth_1 , score_diff_depth_2 , score_diff_depth_5 , score_diff_depth_10 , score_1 , score_2)
positional_data3$tactics_problem_id = NULL
names(positional_data3) <- make.names(names(positional_data3))

bag.s6 = randomForest(finalRatingProblem~., data=positional_data3,importance=TRUE, na.action=na.omit)
bag.s6_1=randomForest(finalRatingProblem~num_positive_moves_1+backwards+log_mc+move_count+forcing_move+num_legal_moves + piece_to_move + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2,data=positional_data,importance=TRUE, na.action=na.omit)

#remove score1, score2
positional_data4 = positional_data %>% select(finalRatingProblem,backwards,log_mc,move_count,piece_to_move,forcing_move,num_legal_moves,`Attacking f7/f2`, `Attacking Castled King` , `Back Rank` , `Basic Checkmates` , `Exchange Sacrifice` , `Mate in 3+` , `Mating Net` , `Queen Sacrifice` , `Sacrifice` , `Vulnerable King` , `Clearance Sacrifice` , `Decoy / Deflection` , `Mate in 2` , `Discovered Attack` , `Fork / Double Attack` , `Hanging Piece` , `Double Check` , `Pin` , `Smothered Mate` , `Remove the Defender` , `Interference` , `Mate in 1` , `NULL` , `Trapped Piece` , `Pawn Promotion` , `Skewer` , `X-Ray Attack` , `Overloading` , `Simplification` , `Defense` , `Zwischenzug` , `Endgame Tactics` , `Stalemate` , `Perpetual Check` , `Desperado` , `Zugzwang` , `En passant` , `Windmill` , `Underpromotion` , `Double` , `Opposition` , score_diff_depth_1 , score_diff_depth_2 , score_diff_depth_5 , score_diff_depth_10)
positional_data4$tactics_problem_id = NULL
names(positional_data4) <- make.names(names(positional_data4))
bag.s5_2 = randomForest(finalRatingProblem~., data=positional_data4,importance=TRUE, na.action=na.omit)


positional_data4 = positional_data %>% select(finalRatingProblem,`Attacking f7/f2`, `Attacking Castled King` , `Back Rank` , `Basic Checkmates` , `Exchange Sacrifice` , `Mate in 3+` , `Mating Net` , `Queen Sacrifice` , `Sacrifice` , `Vulnerable King` , `Clearance Sacrifice` , `Decoy / Deflection` , `Mate in 2` , `Discovered Attack` , `Fork / Double Attack` , `Hanging Piece` , `Double Check` , `Pin` , `Smothered Mate` , `Remove the Defender` , `Interference` , `Mate in 1` , `NULL` , `Trapped Piece` , `Pawn Promotion` , `Skewer` , `X-Ray Attack` , `Overloading` , `Simplification` , `Defense` , `Zwischenzug` , `Endgame Tactics` , `Stalemate` , `Perpetual Check` , `Desperado` , `Zugzwang` , `En passant` , `Windmill` , `Underpromotion` , `Double` , `Opposition`)
positional_data4$tactics_problem_id = NULL
names(positional_data4) <- make.names(names(positional_data4))
bag.s5 = randomForest(finalRatingProblem~., data=positional_data4,importance=TRUE, na.action=na.omit)


s5_2 = lm(finalRatingProblem~., data=positional_data4)

for(i in 1:10000000){
  print(i)
}

#
#
#
#
#
#
#
#
#


set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(positional_data), 0.8*nrow(positional_data))  # row indices for training data
trainingData <- positional_data[trainingRowIndex, ]  # model training data
testData  <- positional_data[-trainingRowIndex, ]   # test data

my_model <- lm(finalRatingProblem~backwards+log_mc+move_count+piece_to_move+forcing_move+num_legal_moves + `Attacking f7/f2` + `Attacking Castled King` + `Back Rank` + `Basic Checkmates` + `Exchange Sacrifice` + `Mate in 3+` + `Mating Net` + `Queen Sacrifice` + `Sacrifice` + `Vulnerable King` + `Clearance Sacrifice` + `Decoy / Deflection` + `Mate in 2` + `Discovered Attack` + `Fork / Double Attack` + `Hanging Piece` + `Double Check` + `Pin` + `Smothered Mate` + `Remove the Defender` + `Interference` + `Mate in 1` + `NULL` + `Trapped Piece` + `Pawn Promotion` + `Skewer` + `X-Ray Attack` + `Overloading` + `Simplification` + `Defense` + `Zwischenzug` + `Endgame Tactics` + `Stalemate` + `Perpetual Check` + `Desperado` + `Zugzwang` + `En passant` + `Windmill` + `Underpromotion` + `Double` + `Opposition` + score_diff_depth_1 + score_diff_depth_2 + score_diff_depth_5 + score_diff_depth_10 + score_1 + score_2+score_dummy+score_dummy_2, data = trainingData)
distPred <- predict(my_model, testData)  
actuals_preds <- data.frame(cbind(actuals=testData$finalRatingProblem, predicteds=distPred))
actuals_preds$actuals = as.double(actuals_preds$actuals)
actuals_preds = actuals_preds[complete.cases(actuals_preds), ]
correlation_accuracy <- cor(actuals_preds)

temp2 = actuals_preds$actuals - actuals_preds$predicteds#= as.numeric(actuals_preds$actuals)
temp2 = temp2/actuals_preds$actuals
temp2 = abs(temp2)
temp2 = temp2[complete.cases(temp2) ]
mean(temp2)


positional_data2 = subset(positional_data, positional_data$`Mate in 2` == "TRUE")
