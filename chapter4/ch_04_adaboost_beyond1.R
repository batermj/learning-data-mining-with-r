#clean the workspace and memory
rm( list=ls() )
gc()

library(adabag)

data <- read.csv("data/datasets002.csv", header=TRUE)
datasets002 <- as.data.frame(data)

model.adaboost <- boosting(Species~., data=datasets002, boos=FALSE, mfinal=5, coeflearn='Zhu')
print(model.adaboost)
importanceplot(model.adaboost)
