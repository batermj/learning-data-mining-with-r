#clean the workspace and memory
rm( list=ls() )
gc()

library(arules)

load("data/datasets006.RData")

rules <- apriori(datasets006)
print(rules)
inspect(rules)
