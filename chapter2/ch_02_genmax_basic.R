#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/itemsets005.csv", header=FALSE)
tbl <- as.matrix(tbl)
colnames(tbl) <- NULL
itemsets <- t(tbl)

items <- c(1,2,3,4,5)
min_sup <- 3

mf <- NULL

testGenMax <- function(data,base_items,MIN_SUP){
	m <- CreateMFI()
	p <- GenerateFrequentTidSet(data,base_items,MIN_SUP)
	GenMax(p,m,base_items,MIN_SUP)
}

GenerateFrequentTidSet <- function(data,base_items,MIN_SUP){
    tidsets <- NULL
    data <- cbind(data,apply(data,1,sum))
    items <- diag(length(base_items))
    for(idx in seq(nrow(data))){
        tidsets <- rbind(tidsets,c(items[idx,],data[idx,]))
    }
    tidsets <- tidsets[tidsets[,ncol(tidsets)]>MIN_SUP,-ncol(tidsets)]
    return(tidsets)
}

GenMax <- function (p,m,base_items,MIN_SUP){
	y <- GetItemsetUnion(p,base_items)
	if( SuperSetExists(m,y,base_items) ){
		return
	}
	len4p <- GetLength(p)
	for(idx in seq(len4p)){
		q <- NULL
		jdx <- idx + 1
		while(idx<jdx && jdx<=len4p){
			xij <- MergeTidSets(p[idx,],p[jdx,],base_items)
			sup_xij <- GetSupport(xij,base_items)
			if(sup_xij>=MIN_SUP){
				q <- Add2Itemset(q,xij)
			}			
			jdx <- jdx + 1
		}
		rownames(q) <- NULL
		
		if( !IsEmpty(q) ){
			GenMax(q,m,base_items,MIN_SUP)
		}else if( !SuperSetExists(m,p[idx,],base_items) ){
			Add2MFI(m,p[idx,])
		}
	}
}

Add2MFI	<- function(m,p){
	mf <<- rbind(mf,p)
	rownames(mf) <<- NULL
}

IsEmpty	<- function(q){
	return(length(q)==0)	
}

Add2Itemset	<- function(q,xij){
	return(rbind(q,xij))
}

GetSupport <- function(xij,base_items){
	return(sum(xij[-1:-length(base_items)]))
}

MergeTidSets <- function(pa,pb,base_items){
	len <- length(base_items)
	x <- ifelse(pa[1:len]+pb[1:len],1,0)
	sup_x <- ifelse(pa[-1:-len]*pb[-1:-len],1,0)
	return(c(x,sup_x))
}

GetLength <- function(p){
	return(nrow(p))
}

SuperSetExists <- function(m,y,base_items){
	if(length(mf)==0){
		return(FALSE)
	}
	return(any(mf[,1:length(base_items)]%*%y[1:length(base_items)]>=sum(y[1:length(base_items)])))
}

GetItemsetUnion	<- function(p,base_items){
	return(ifelse(apply(p,2,sum)[1:length(base_items)],1,0))
}

CreateMFI <- function(){}

testGenMax(itemsets,items,min_sup)
rownames(mf) <- NULL
print(mf[,1:length(items)])
