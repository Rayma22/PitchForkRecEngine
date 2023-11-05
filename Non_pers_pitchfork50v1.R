# Recommendation EDA and Non-Personalized

# Library Input
library('recommenderlab')
library(reshape2)

# Data Input

df <- read.csv("C:\\Users\\majon\\OneDrive\\Pulpit\\IE docs\\SEMESTER 5\\Recommendation Engines\\Final Project\\GitHubFinalProject\\PitchForkRecEngine\\user_item_mat_pitchfork.csv")
head(df)

# Turning it into a matrix.
df_reordered <- df[, c("artist", "author", "score")]

df_reordered$score <- as.numeric(df_reordered$score) 

df_testmat <- dcast(df_reordered, author ~ artist, value.var = "score", fun.aggregate = function(x) mean(x, na.rm = TRUE))
df_testmat <- as.matrix(df_testmat)

rownames(df_testmat) <- df_testmat[, 1]
df_testmat <- df_testmat[, -1]

# Not working!!!!!! Problem: The whole matrix is class character, we want to somehow change it to numeric!

# ex. sapply(df_testmat, class)

df_testmat <- round(df_testmat, 2)

df_mat <- as(df_testmat, "realRatingMatrix")

df_mat

# Divide data
e <- evaluationScheme(df_mat, method = "split", train = 0.9, k=1, given=15) 
e
