#clean the workspace and memory
rm( list=ls() )
gc()

library(adabag)

data <- read.csv("data/datasets001.csv", header=TRUE)
datasets001 <- as.data.frame(data)

sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
data.bagging <- bagging(Species ~ ., data=datasets001[sub,], mfinal=10)
data.predbagging<- predict.bagging(data.bagging, newdata=datasets001[-sub,])

print(data.bagging)
