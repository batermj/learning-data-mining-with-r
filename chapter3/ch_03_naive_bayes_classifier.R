#clean the workspace and memory
rm( list=ls() )
gc()

library(plyr)
library(reshape2)

tbl <- read.csv("data/datasets006.csv", header=TRUE)
tbl <- as.data.frame(tbl)

x <- data.frame(feature.name =c("Size", "Weight", "Color"),feature.value =c('Big','Heavy','Red'))

TestClassifier <- function(x){
	data <- tbl
	prob4class <- GetPriorProbability(data,"Taste")
	prob4fclass <- GetConditionalProbability(data,"Taste")
	y <- GetLabelForMaxPostProbability(prob4class,prob4fclass,x)
	print(y)
}

GetLabelForMaxPostProbability <- function(prob4class,prob4fclass,x){
	colnames(x) <- c("feature.name", "feature.value")
	colnames(prob4class) <- c("class.name","prob")
	colnames(prob4fclass) <- c("class.name","feature.name","feature.value","prob")
	feature.all <- join(x,prob4fclass,by=c("feature.name","feature.value"),type="inner")
	feature.prob <- ddply(feature.all,.(class.name),summarize,prob_fea=prod(prob))
	class.all <- join(feature.prob,prob4class,by="class.name",type="inner")
	y <- ddply(class.all,.(class.name),mutate,PreProbability=prob_fea*prob)[,c(1,4)]
}

GetConditionalProbability <- function(data,class2predict){
	data.melt <- melt(data,id=c(class2predict))
	aa <- ddply(data.melt, c(class2predict,"variable","value"), "nrow")
	bb <- ddply(aa, c(class2predict,"variable"),mutate,sum=sum(nrow),prob=nrow/sum)
	colnames(bb) <- c("class.name","feature.name","feature.value","feature.nrow","feature.sum","prob")
	bb[,c(1,2,3,6)]
}

GetPriorProbability <- function(data,class2predict){
	ddply(ddply(data,class2predict,"nrow"),class2predict,mutate,prob=nrow/len)[,-2]
}

len <- nrow(tbl)
TestClassifier(x)
