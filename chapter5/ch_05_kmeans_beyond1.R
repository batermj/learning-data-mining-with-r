#clean the workspace and memory
rm( list=ls() )
gc()

library(fpc)

data <- read.csv("data/datasets001.csv",header=TRUE)

pamk.result <- pamk(data)
kc <- kmeans(data, pamk.result$nc)
print(kc$cluster)
print(kc)

pamk.result <- pamk(data,criterion="multiasw",usepam=FALSE,critout=TRUE)
kc <- kmeans(data, pamk.result$nc)
print(kc$cluster)
print(kc)
