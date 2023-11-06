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
row.names(df_testmat) <- df_testmat$author
df_testmat <- df_testmat[, -1]

class(df_testmat$'1990s')
lapply(df_testmat, class)

# as.matrix changes everything to character??
df_testmat <- as.matrix(df_testmat)
#df_testmat <- as.matrix(sapply(df_testmat, as.numeric))
class(df_testmat['1990s'])
lapply(df_testmat, class)

df_testmat <- round(df_testmat, 2)

df_mat <- as(df_testmat, "realRatingMatrix")

df_mat

#########################################################
# Divide data
e <- evaluationScheme(df_mat, method = "split", train = 0.9, k=1, given=10) 
e

# given 10 = 247, given 3 = 363

r <- Recommender(getData(e, "train"), method = "RANDOM")
r

getData(e, "train")
getData(e, "known")
getData(e, "unknown")

a <- getData(e, "known")
#predict using the predict function, known data and type ratings
p <- predict(r, getData(e, "known"), type = "ratings")
p

#Regression
##calcPredictionAccuracy 
calcPredictionAccuracy(p, getData(e, "unknown"))

#########################################################
# BY GOODRating

e2 <- evaluationScheme(df_mat, method="split", train=0.9,
                       k=1, given=10,goodRating=7)
e2

r2 <- Recommender(getData(e2, "train"), "RANDOM")
r2

p2 <- predict(r2, getData(e2, "known"), type = "topNList")
p2

calcPredictionAccuracy(p2, getData(e2, "unknown"), given = 10, goodRating = 7)

#########################################################

library(Metrics)

predicted <-as(p, "matrix")
actual<-as(getData(e, "unknown"), "matrix")
mae(actual['Briana Younger',],predicted['Briana Younger',])
rmse(actual['Briana Younger',],predicted['Briana Younger',])

#########################################################

results <- evaluate(x=e2, method="RANDOM", n=c(1,3,5,10,100))
getConfusionMatrix(results)
plot(results, "prec/rec", annotate=T, main="Precisi?n/Recall")

#########################################################

algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL)
)

#########################################################

results <- evaluate(x=e2, method=algorithms, n=c(1,3,5,10,100))
getConfusionMatrix(results[[1]])
plot(results, "prec/rec", annotate=T, main="Precisi?n/Recall")
