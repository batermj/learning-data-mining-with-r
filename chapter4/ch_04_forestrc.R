#clean the workspace and memory
rm( list=ls() )
gc()

library(randomForest)

datasets003 <- read.csv("data/datasets003.csv", header=TRUE)
datasets003 <- as.data.frame(datasets003)

model.randomforest <- randomForest(Species ~ ., data=iris, ntree=300, importance=TRUE, proximity=TRUE)
print(model.randomforest)
