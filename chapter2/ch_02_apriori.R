#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/itemsets001.csv", header=FALSE)
tbl <- as.matrix(tbl)
colnames(tbl) <- NULL
itemsets <- tbl
items <- c(1,2,3,4,5)
min_sup <- 0.22*nrow(itemsets)

#######
#	Algorithm: Apriori, 
#		generate the frequent itemsets from the input datasets with the specified support
#	Input:
#		data, MATRIX
#		base_items, VECTOR
#		MIN_SUP, FLOAT
#		parameter,
#######
Apriori <- function(data,base_items,MIN_SUP,parameter=NULL){
	f <- InitCandidateSet(data,base_items)
	c <- list()
	c[[1]] <- FindFrequentItemset(f,base_items,1,MIN_SUP)
	k <- 2
	len4data <- GetDatasetSize(data)
	while( !IsEmpty(c,k-1) ){
		f[[k]] <- AprioriGen(c,k-1)
		if(length(f)==k){
			f[[k]] <- IncreaseSupportCount(f[[k]],data)
			c[[k]] <- FindFrequentItemset(f,base_items,k,MIN_SUP)		
		}else{break}
		k <- k+1
	}
	c
}

AprioriGen <- function(c,k){
	ck <- c[[k]][,-ncol(c[[k]])]
	f <- NULL
	len <- nrow(ck)
	for(idx in seq(nrow(ck))){
		jdx <- idx+1
		while(idx<jdx && jdx<=len){
			a <- ck[idx,]
			b <- ck[jdx,]
			if( k==1 || identical(a[1:(k-1)],b[1:(k-1)]) ){
					ab <- ifelse(a+b,1,0)
					if( !NeedPrune(ck,ab,k) ){
						f <- rbind(f,ab)
					}else{
						#print("Pruned")
					}
			}
			jdx <- jdx + 1
		}
	}

	if(length(f)){
		 f <- cbind(f,rep(0,dim(f)[1]))
		 rownames(f) <- NULL
	}
	return(f)
}

NeedPrune <- function(ck,ab,k){
	ck <- rbind(ck,ab)
	len <- dim(ck)[1]
	for(idx in which(ab>0)){
		temp <- ab
		temp[idx] <- 0
		for(idx in seq(len)){
			if(identical(temp,ck[idx,]))break
		}
		if(idx==len)return(TRUE)
	}
	return(FALSE)
}

IncreaseSupportCount <- function(fk,data){
	w4f <- ncol(fk)
	len4f <- nrow(fk)
	len4d <- nrow(data)
	for(idx in seq(len4d)){
		for(jdx in seq(len4f)){
			if(identical(fk[jdx,-w4f],fk[jdx,-w4f]*data[idx,])){
				fk[jdx,w4f] <- fk[jdx,w4f] + 1
			}
		}
	}
	return(fk)
}

IsEmpty <- function(ck,k){
	return(ifelse(nrow(ck[[k]])>0,FALSE,TRUE))
}

GetDatasetSize <- function(data){
	return( nrow(data) )
}

InitCandidateSet <- function(data,base_items){
	list(cbind(diag(length(base_items)),apply(data,2,sum)))
}

FindFrequentItemset <- function(fk,base_items,k,MIN_SUP){
	data <- fk[[k]]
	return(data[data[,dim(data)[2]]>MIN_SUP,])
}

frequent_itemsets <- Apriori(itemsets,items,min_sup)
print(frequent_itemsets)
