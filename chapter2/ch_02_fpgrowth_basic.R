#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/itemsets003.csv", header=FALSE)
tbl <- as.matrix(tbl)
colnames(tbl) <- NULL
itemsets <- tbl
items <- c(1,2,3,4,5)
min_sup <- 3
ff <- NULL

testFPGrowth <- function(data,base_items,MIN_SUP,parameter=NULL){
	ordered_data <- PrepareDataset(data,base_items)
	ranking_asc <- rev(order(apply(data,2,sum),decreasing = TRUE))
	cnts <- rep(1,length(ordered_data))
	r <- GenerateFPTree(ordered_data,base_items,cnts)
	p <- NULL
	f <- NULL
	FPGrowth(r,p,f,base_items,ranking_asc,MIN_SUP)
}

PrepareDataset <- function(data,base_items){
	ranking_desc <- order(apply(data,2,sum),decreasing = TRUE)
	newdata <- rbind(data,seq(ncol(data)))
	rownames(newdata) <- NULL
	len <- nrow(data);

	rtset <- list()
	for( idx in seq(len)){
		tempv <- newdata[c(idx,len+1),ranking_desc]
		rtset[[idx]] <- c(tempv[2,which(tempv[1,]>0)])
	}
	return(rtset)
}

UpdateRankingASC <- function(r,ranking_asc){
	new_ranking_asc <- NULL
	itemp_index <- ncol(r)-1
	for(item in ranking_asc){
		if(any(r[,itemp_index]==item)){
			new_ranking_asc <- c(new_ranking_asc,item)
		}
	}
	return(new_ranking_asc)
}

MergeSet <- function(p,item){
	return(c(p,item))
}

FPGrowth <- function (r,p,f,base_items,ranking_asc,MIN_SUP){
	r <- RemoveInfrequentItems(r,ranking_asc,MIN_SUP)
	if(IsPath(r)){
		y <- GetSubset(r)
		len4y <- GetLength(y)
		for(idx in seq(len4y)){
			x <- MergeSet(p,y[[idx]])
			sup_x <- GetMinCnt(r,y[[idx]])
			AddToFrequentItemsets(f,base_items,x,sup_x)
		}
	}else{
		ranking_asc <- UpdateRankingASC(r,ranking_asc)
		for(item in ranking_asc){
			x <- MergeSet(p,item)
			sup_x <- GetSupportCount(r,item)
			AddToFrequentItemsets(f,base_items,x,sup_x)
			paths <- GetAllPathsFromRoot(r,item)
			data <- paths[[1]]
			cnts <- paths[[2]]
			rx <- GenerateFPTree(data,base_items,cnts)
			if( !IsEmpty(rx) ){
				FPGrowth(rx,x,f,base_items,ranking_asc,MIN_SUP)
			}
		}
	}
}

IsEmpty <- function(rx){
	return(length(rx)==0)
}

GetAllPathsFromRoot <- function(r,item){
	cntp_index <- ncol(r)
	itemp_index <- cntp_index - 1
	item_nodes <- which(r[,itemp_index]==item)
	rtset <- list()
	cnts <- NULL
	for( idx in seq(length(item_nodes)) ){
		tempv <- GetPathFromRoot(r,item_nodes[idx])
		if(length(tempv[-length(tempv)])>0){
			cnts <- c(cnts,r[item_nodes[idx],cntp_index])
			rtset[[idx]] <- tempv[-length(tempv)]
		}
	}
	return(list(rtset,cnts))
}

GetPathFromRoot <- function(r,idx){
	cntp_index <- ncol(r)
	itemp_index <- cntp_index - 1
	parentp_index <- cntp_index - 2
	current_index <- idx
	path <- NULL
	while(current_index>0){
		if(r[current_index,itemp_index]>0){
			path <- c(r[current_index,itemp_index],path)
		}
		current_index <- r[current_index,parentp_index]
	}
	return(path)
}

AddToFrequentItemsets <- function(f,base_items,x,sup_x){
	itemv <- rep(0,length(base_items))
	itemv[x] <- 1
	ff <<- rbind(ff,c(itemv,sup_x))		
}

GetSupportCount <- function(r,item){
	cntp_index <- ncol(r)
	itemp_index <- cntp_index - 1
	return( sum(r[which(r[,itemp_index]==item),cntp_index]) )
}

GetMinCnt <- function(r,y){
	len <- (ncol(r)-3)/2
	cntp_index <- ncol(r)
	itemp_index <- cntp_index - 1
	sup_v <- NULL
	for(item in y){
		cnt <- r[which(r[,itemp_index]==item),cntp_index]
		sup_v <- c(sup_v,cnt)
	}
	return(min(sup_v))
}

GetSubset <- function(r){
	len <- (ncol(r)-3)/2
	cntp_index <- ncol(r)
	itemp_index <- cntp_index - 1
	items_index <- which(r[,itemp_index]>0)
	items_count <- length(items_index)
	rtlist <- list()
	subset_index <- 1
	for(idx in seq(items_count)){
		rsubsets <- combn(seq(items_count),idx)
		for(jdx in seq(ncol(rsubsets))){
			rtlist[[subset_index]] <- r[items_index[rsubsets[,jdx]],itemp_index]
			subset_index <- subset_index + 1
		}
	}
	return(rtlist)
}

GetLength <- function(r){
	return(length(r))
}

IsPath <- function(r){
	base_items_count <- (ncol(r)-3)/2
	return(all(apply(r[,1:base_items_count],1,sum)<=1))
}

RemoveInfrequentItems <- function(r,ranking_asc,MIN_SUP){
	len <- (ncol(r)-3)/2
	cntp_index <- ncol(r)
	itemp_index <- cntp_index - 1
	parentp_index <- cntp_index - 2
	for(item in ranking_asc){
		items_index <- which(r[,itemp_index]==item)
		if( sum(r[items_index,cntp_index])>=MIN_SUP ){
			break;
		}else{
			for(item_index in items_index){
				r[r[item_index,parentp_index],item] <- 0	
				r[r[item_index,parentp_index],item+len] <- 0	
				r[item_index,] <- 0	
			}
		}
	}
	return(r)
}

CreateHeader <- function(value,base_items,cnt=0){
	len <- length(base_items)
	parentp <- 0
	child_node <- rep(0,len)
	childp <- rep(0,len)
	new_node <- c(child_node,childp,parentp,value,cnt)
	return(t(as.matrix(new_node,nrow=1,ncol=length(new_node))))
}

CheckAndAddNodes <- function(rtm,base_items,items,cnt=1){
	len <- length(base_items)
	currentp <- 1
	parentp_index <- len*2+1
	valuep_index <- parentp_index + 1
	cntp_index <- parentp_index + 2
	for( item in items ){
		rtm[currentp,cntp_index] <- rtm[currentp,cntp_index] + cnt
		childp_index <- item + len
		if( rtm[currentp,item]!=1 ){
			parentp <- currentp
			child_node <- rep(0,len)
			childp <- rep(0,len)
			new_node <- c(child_node,childp,parentp,item,0)
			rtm <- rbind(rtm,new_node)
			rtm[currentp,item] <- 1
			rtm[currentp,childp_index] <- nrow(rtm)
		}
		currentp <- rtm[currentp,childp_index]
	}
	#update the cnt for the last item
	rtm[currentp,cntp_index] <- rtm[currentp,cntp_index] + cnt
	rownames(rtm) <- NULL
	return(rtm)
}

GenerateFPTree <- function(data,base_items,cnts){
	rtm <- NULL
	if(length(data)==0){
		return(rtm)
	}
	a <- 0
	rtm <- CreateHeader(a,base_items)
	for(idx in seq(length(data))){
		cnt <- cnts[idx]
		a <- data[[idx]]
		rtm <- CheckAndAddNodes(rtm,base_items,a,cnt)
	}
	return(rtm)
}

testFPGrowth(itemsets,items,min_sup)
print(ff)
