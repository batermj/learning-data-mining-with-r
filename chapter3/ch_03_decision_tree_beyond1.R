#clean the workspace and memory
rm( list=ls() )
gc()

library(rpart)  

datasets004 <- read.csv("data/datasets004.csv", header=TRUE)
datasets004 <- as.data.frame(datasets004)

model.decisiontree <- rpart(Species ~ Sepal.Length + Sepal.Width, data = datasets004)
print(model.decisiontree)
