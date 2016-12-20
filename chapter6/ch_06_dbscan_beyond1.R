#clean the workspace and memory
rm( list=ls() )
gc()

library(fpc)

data001 <- read.csv("data/datasets001.csv", header=TRUE)
data001 <- as.data.frame(data001)

result.cluster <- dbscan(data001, 0.2)
print(result.cluster)
