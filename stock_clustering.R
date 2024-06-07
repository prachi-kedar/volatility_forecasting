library("ggplot2")
library("dplyr")
library("ggfortify")
library("ClusterR") 
library("cluster")
library(factoextra)


book_example <- read.csv("C:/Users/prach/OneDrive - Politecnico di Milano/Documenti/courses/Applied Statistics/project_april/train.csv")
book_example

min <- min(book_example$target)
min

max <- max(book_example$target)
max

med <- median(book_example$target)
med

hist(book_example$target)

book_example$target = log(book_example$target)
book_example$target

hist(book_example$target)


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

sum(is.na(book_example))


wssplot(book_example)

kmean <- kmeans(book_example, 3)
kmean$centers

#plot results of final k-means model
fviz_cluster(kmean, data = book_example)



















