#clean the workspace and memory
rm( list=ls() )
gc()

library(cluster);
library(fpc);

data004 <- read.csv("data/datasets004.csv", header=TRUE)
data004 <- as.data.frame(data004)

result.cluster <- pam(data004,3,diss=inherits(data004,"dist"),metric="euclidean",cluster.only=TRUE)
print(result.cluster)

result.cluster <- pam(data004,3,diss=inherits(data004,"dist"),metric="euclidean",medoids = c(32,85,137),cluster.only=TRUE)
print(result.cluster)
