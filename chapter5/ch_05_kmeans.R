#clean the workspace and memory
rm( list=ls() )
gc()

library(fpc)

data <- read.csv("data/datasets001.csv",header=TRUE)
print(data)

pamk.result <- pamk(data)
print(pamk.result)
print(pamk.result$nc)

kc <- kmeans(data, pamk.result$nc)
kc$cluster
print(kc)
