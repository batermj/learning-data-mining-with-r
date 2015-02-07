#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/itemsets002.csv", header=FALSE)
tbl <- as.matrix(tbl)
colnames(tbl) <- NULL
itemsets <- t(tbl)
print(itemsets)

items <- c(1,2,3,4,5)
min_sup <- 0.22*nrow(itemsets)

f <- NULL
ff <- NULL
testEclat <- function(data,base_items,MIN_SUP){
	print(data)
	p <- GetFrequentTidSets(data,base_items,MIN_SUP)
	print(p)
	Eclat(p,f,MIN_SUP,length(base_items))
	return(f)
}

GetFrequentTidSets <- function(data,base_items,MIN_SUP){
	tidsets <- NULL
	data <- cbind(data,apply(data,1,sum))
	items <- diag(length(base_items))
	for(idx in seq(nrow(data))){
		tidsets <- rbind(tidsets,c(items[idx,],data[idx,]))	
	}
	tidsets <- tidsets[tidsets[,ncol(tidsets)]>MIN_SUP,-ncol(tidsets)]
	return(tidsets)
}

Eclat <- function(p,f,MIN_SUP,parameter=NULL){
	len <- nrow(p)
	for(idx in seq(len)){
		a <- p[idx,]
		AddFrequentItemset(f,a)
		pa <- NULL
		jdx <- idx + 1
		while(idx<jdx && jdx<=len){
			b <- p[jdx,]
			ab <- MergeTidSets(a,b,parameter)
			if(GetSupport(ab,parameter)>=MIN_SUP){
				pa <- rbind(pa,ab)
			}
			jdx <- jdx + 1
		}
		rownames(pa) <- NULL
		if(!IsEmptyTidSets(pa)){
			#print(pa)
			Eclat(pa,f,MIN_SUP,parameter)
		}
	}
}

IsEmptyTidSets <- function(pa){
	if(length(pa)>0)return(FALSE)
	return(TRUE)
}

MergeTidSets <- function(a,b,parameter=NULL){
	len4i <- parameter
	len4t <- length(a)
	return(c(ifelse(a[1:len4i]+b[1:len4i],1,0),a[(len4i+1):len4t]*b[(len4i+1):len4t]))
}

AddFrequentItemset <- function(f,p){
	ff <<- rbind(ff,p)
}

GetSupport <- function(ab,parameter=NULL){
	len4i <- parameter
	len4t <- length(ab)
	return(sum(ab[(len4i+1):len4t]))
}

testEclat(itemsets,items,min_sup)
rownames(ff) <- NULL
print(ff)
