#clean the workspace and memory
rm( list=ls() )
gc()

library(textcat)

tbl <- read.csv("data/datasets003.csv", header=FALSE)
tbl <- as.data.frame(tbl)
sentences <- as.character(tbl[,1])

rtn <- textcat(sentences,p = textcat::TC_char_profiles,method = "CT")
print(rtn)
