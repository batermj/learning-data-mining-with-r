#clean the workspace and memory
rm( list=ls() )
gc()

library(randomForest)

datasets004 <- read.csv("data/datasets004.csv", header=TRUE)
datasets004 <- as.data.frame(datasets004)

model.randomforest <- randomForest(Species ~ ., data=iris, importance=TRUE, proximity=TRUE)
print(model.randomforest)
