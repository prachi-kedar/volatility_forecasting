library("ggplot2")
library("dplyr")
library("ggfortify")

book_example <- read.csv("C:/Users/Public/cleaned_data.csv")

summary(book_example)
head(book_example)

data <- select(book_example)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(book_example)

kmean <- kmeans(book_example, 2)
kmean$centers

autoplot(kmean, book_example, frame = TRUE)
