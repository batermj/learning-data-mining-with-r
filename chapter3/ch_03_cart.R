#clean the workspace and memory
rm( list=ls() )
gc()

library(rpart);

data <- read.csv("data/datasets003.csv", header=TRUE)
data <- as.data.frame(data)

ct <- rpart.control(xval=10, minsplit=20, cp=0.1) 
formula <- Kyphosis~Age + Number + Start
fit <- rpart(formula=formula,data=data, 
		method="class",control=ct,
		parms=list(prior = c(0.65,0.35), 
		split="information"))

print(fit)
ndata=data.frame('Kyphosis'='present','Age'=82,'Number'=5,'Start'=11)
print(predict(fit,ndata))
