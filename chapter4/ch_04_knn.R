#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/itemsets005.csv", header=TRUE)

PreProcessing <- function(data,parameter=NULL){
	label_names <- colnames(data)
	len <- length(label_names)
	data_standardized <- cbind(data.frame(scale(data[,-len])), data[, len])
	names(data_standardized)[len] <- label_names[len]
	return(data_standardized)
}

KNN <- function(data,datapoint,parameter=NULL){
	length.known <- parameter[[1]]
	dim2data <- parameter[[2]] 
	k <- parameter[[3]] 

	dist2known <- data.frame(dis = rep(0, length.known))
	for (idx in seq(length.known)){ 
		dist2known[idx,1] <- dist(rbind(datapoint[-dim2data],data[idx,-dim2data]),method ="euclidean") 
		dist2known[idx,2] <- data[idx,dim2data] 
		names(dist2known)[1] = "Distances"
		names(dist2known)[2] = "Species"
	}
	dist2known <- dist2known[order(dist2known$Distances),]
	type_freq <- as.data.frame(table(dist2known[1:k,]$Species))
	type_freq <- type_freq[order(-type_freq$Freq),]
	datapoint[dim2data+1] <- type_freq[1,1]
	names(datapoint)[dim2data+1] = "Species.Predicted"
	return(datapoint)
}

testKNN <- function(data,parameter=NULL){
	datasize <- nrow(tbl)
	samplesize4known <- datasize*2/3
	samplesize4unknown <- datasize - samplesize4known
	sampling <- sample(seq(datasize),size=samplesize4known)

	label_names <- colnames(data)
	len <- length(label_names)

	data.known <- tbl[sampling,]
	data.unknown <- tbl[-sampling,]

	length.known <- nrow(data.known)
	length.unknown <- nrow(data.unknown)
	resultset <- NULL
	k <- 5
	for(idx in seq(length.unknown)){
		resultset <- rbind(resultset,KNN(data.known,data.unknown[idx,],list(length.known,len,k)))
	}
	print(resultset[,len:(len+1)])
}

tbl <- PreProcessing(tbl)
print(tbl)

testKNN(tbl)
