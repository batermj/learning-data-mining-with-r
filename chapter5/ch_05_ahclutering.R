#clean the workspace and memory
rm( list=ls() )
gc()

data <- read.csv("data/datasets009.csv", header=TRUE)

dist.e=dist(data,method='manhattan')

model1=hclust(dist.e,method='ward')
result=cutree(model1,k=3)

print("Agglomerative Hierachical Clustering")
print(model1)
print(result)
