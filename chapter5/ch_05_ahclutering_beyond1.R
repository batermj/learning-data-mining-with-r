#clean the workspace and memory
rm( list=ls() )
gc()

data <- read.csv("data/datasets009.csv", header=TRUE)

dist.e <- dist(data,method='manhattan')

model2ah <- hclust(dist.e,method='ward')
result <- cutree(model2ah,k=3)

print("Agglomerative Hierachical Clustering")
print(model2ah)
print(result)
