library(utils)

#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/itemsets007.csv",header=FALSE)
print(tbl)

tbl <- as.matrix(tbl)
itemsets <- as.vector(tbl)
print(itemsets)

items <- c('A','C','G','T')
min_sup <- 3

ff <- NULL
#c(parent,first_child_pos,num_of_children,sup)
prefixtree_head <- NULL
prefixtree_data <- NULL
#c(first_leaf_pos,num_of_leaves,place_holder)
prefixtree_level <- NULL

testGSP <- function(data,base_items,MIN_SUP){
    print(data)
    GSP(data,base_items,MIN_SUP)
}

GSP <- function(data,base_items,MIN_SUP){
    levelJdx <- 1
    prefixtree_level <<- rbind(prefixtree_level,c(1,1,1))
    InitNextLevel(levelJdx)
    currentIdx <- prefixtree_level[levelJdx+1,1]
    prefixtree_data <<- rbind(prefixtree_data,"NULL")
    prefixtree_head <<- rbind(prefixtree_head,c(0,currentIdx,0,1000))
    for( c in base_items ){
        AddLeaf(1,levelJdx,currentIdx,c)
        currentIdx <- currentIdx + 1
    }
    
    levelJdx <- 2
    while( !IsPrefixTreeEmpty(levelJdx) ){
        CalculateSupport(data,levelJdx)
        currentLeavesIndex <- GetLeavesIdx(levelJdx)
        for( leafIdx in currentLeavesIndex ){
            if( GetSupport(leafIdx)>=MIN_SUP ){
                Add2FrequentSequencesSet(leafIdx)
            }else{
                RemovePrefixTreeLeaf(leafIdx,levelJdx)
            }
        }
        GeneratePrefixTree(levelJdx)
        levelJdx <- levelJdx + 1
    }
}

RemovePrefixTreeLeaf <- function(leafIdx,levelJdx){
    prefixtree_head[leafIdx] <<- 0
    prefixtree_data[leafIdx] <<- ""
    prefixtree_level[levelJdx,2] <<- prefixtree_level[levelJdx,2] - 1
}

Add2FrequentSequencesSet <- function(leafIdx){
    ff <<- c(ff, prefixtree_data[leafIdx])
}

GetLeavesIdx <- function(levelJdx){
    leavesIdx <- (prefixtree_level[levelJdx,1]):(prefixtree_level[levelJdx,1]+prefixtree_level[levelJdx,3]-1)
    leavesIdx[which(prefixtree_data[leavesIdx]!="")]
}

IsPrefixTreeEmpty <- function(levelJdx){
    rtn <- TRUE
	if( length(prefixtree_level)>0 ){
    	if(nrow(prefixtree_level)>=levelJdx){
        	rtn <- (prefixtree_level[levelJdx,2] == 0)
    	}
	}
    rtn
}

IsSubSequence <- function(leaf,s){
	len4leaf <- nchar(leaf)
	len4s <- nchar(s)
	rtn <- FALSE
	if( len4leaf>len4s ){
		rtn <- FALSE
	}else if( len4leaf==len4s ){
		rtn <- (leaf==s)
	}else{
		if( substr(leaf,1,1)==substr(s,1,1) ){
			rtn <- IsSubSequence(substring(leaf,2),substring(s,2))
		}
		if( !rtn ){
			rtn <- IsSubSequence(leaf,substring(s,2))
		}
	}
	rtn
}

GetLeaf <- function(leafIdx){
	prefixtree_data[leafIdx]
}

IncreaseSupport <- function(leafIdx){
    prefixtree_head[leafIdx,4] <<- prefixtree_head[leafIdx,4] + 1
}

GetSupport <- function(leaf_idx){
    prefixtree_head[leaf_idx,4]
}

CalculateSupport <- function(data,levelJdx){
    currentLeavesIndex <- GetLeavesIdx(levelJdx)
	for( s in data ){
        for( leafIdx in currentLeavesIndex ){
			leaf <- GetLeaf(leafIdx)
			if( IsSubSequence(leaf,s) ){
				IncreaseSupport(leafIdx)
			}
		}
	}
}

GetSiblingsIdx <- function(leafIdx){
	parentIdx <- prefixtree_head[leafIdx,1]
	siblingsIdx <- seq(prefixtree_head[parentIdx,3]) + prefixtree_head[parentIdx,2] - 1
	siblingsIdx[which(prefixtree_data[siblingsIdx]!="")]
}

NoExtensions <- function(leafIdx){
	prefixtree_head[leafIdx,3]==0
}

CombineStr <- function(leafIdx,siblingIdx){
	leaf <- prefixtree_data[leafIdx]
	sibling <- prefixtree_data[siblingIdx]
	newExtension <- paste(leaf,substring(sibling,nchar(sibling)),sep="")
	newExtension
}

BelongToLevel <- function(levelJdx,sub_sequence){
	rtn <- FALSE
	currentLeavesIndex <- GetLeavesIdx(levelJdx)
    for( leafIdx in currentLeavesIndex ){
		leaf <- GetLeaf(leafIdx)
		if(IsSubSequence(sub_sequence,leaf)){
			rtn <- TRUE
			break;
		}
	}
	rtn
}

CheckFrequency <- function(levelJdx,extendedLeaf){
	rtn <- TRUE
	strV <- strsplit(extendedLeaf,'')[[1]]
	if( nchar(extendedLeaf) == 2 ){
		rtn <- BelongToLevel(levelJdx,substr(extendedLeaf,2,2))
	}
	if( rtn ){
		for( idx in seq(nchar(extendedLeaf)) ){
			sub_sequence <- paste(strV[-idx],collapse="")
			if( !BelongToLevel(levelJdx,sub_sequence) ) break
		}
	}
	rtn
}

AddLeaf <- function(leafIdx,levelJdx,currentIdx,extendedLeaf){
	#update current leaf information
	prefixtree_data <<- c(prefixtree_data,extendedLeaf)
	prefixtree_head <<- rbind(prefixtree_head,c(leafIdx,0,0,0))

	#update parent information
	if( prefixtree_head[leafIdx,2]==0 ){
		prefixtree_head[leafIdx,2] <<- currentIdx
	}
	prefixtree_head[leafIdx,3] <<- prefixtree_head[leafIdx,3] + 1

	prefixtree_level[levelJdx+1,2] <<- prefixtree_level[levelJdx+1,2] + 1
	prefixtree_level[levelJdx+1,3] <<- prefixtree_level[levelJdx+1,3] + 1
}

InitNextLevel <- function(levelJdx){
    prefixtree_level <<- rbind(prefixtree_level,c(prefixtree_level[levelJdx,1] + prefixtree_level[levelJdx,3],0,0))
}

GeneratePrefixTree <- function(levelJdx){
	InitNextLevel(levelJdx)
	currentIdx <- prefixtree_level[levelJdx+1,1]
    for( leafIdx in GetLeavesIdx(levelJdx) ){
		for( siblingIdx in GetSiblingsIdx(leafIdx) ){
			extendedLeaf <- CombineStr(leafIdx,siblingIdx)
			if( CheckFrequency(levelJdx,extendedLeaf) ){
				AddLeaf(leafIdx,levelJdx,currentIdx,extendedLeaf)
				currentIdx <- currentIdx + 1
			}
		}
		if( NoExtensions(leafIdx) ){
			RemovePrefixTreeLeaf(leafIdx,levelJdx)
		}
	}
}

testGSP(itemsets,items,min_sup)
rownames(ff) <- NULL
print("ff:")
print(ff)
