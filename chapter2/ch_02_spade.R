#clean the workspace and memory
rm( list=ls() )
gc()

library(arulesSequences)

data(zaki)
analyzed_result <- cspade(zaki, parameter = list(support = 0.4), control = list(verbose = TRUE))
print(analyzed_result)
