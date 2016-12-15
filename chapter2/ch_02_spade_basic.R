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
prefixtree_position_list <- list(list())

testSPADE <- function(data,base_items,MIN_SUP){
    print(data)
    currentLeafIndex <- InitSpade(data,base_items,MIN_SUP)
	print(prefixtree_position_list)
	print(cbind(prefixtree_head,prefixtree_data))
	currentNodeIdx <- 1
    SPADE(data,currentNodeIdx,currentLeafIndex,MIN_SUP,1)
}

InitSpade <- function(data,base_items,MIN_SUP){
    prefixtree_data <<- rbind(prefixtree_data,"NULL")
    prefixtree_position_list <<- list("NULL")
    prefixtree_head <<- rbind(prefixtree_head,c(0,0,0,1000))
    currentLeafIndex <- 2
    for( c in base_items ){
        support_count <- GetLeafPositionList(data,currentLeafIndex,c,MIN_SUP)
        if( support_count>=MIN_SUP ){
            AddLeaf(1,currentLeafIndex,c)
            currentLeafIndex <- currentLeafIndex + 1
        }
    }   
    currentLeafIndex
}

GetLeafPositionList <- function(data,currentIdx,sym,MIN_SUP){
    poslist <- list()
	jdx <- 1
    for( idx in seq(length(data)) ){
		if(length(which(strsplit(data[idx],"")[[1]]==sym))>0){
			poslist[[jdx]] <- c(idx,which(strsplit(data[idx],"")[[1]]==sym))
			jdx <- jdx + 1
		} 
	}
	prefixtree_position_list[[currentIdx]] <<- poslist
	length(prefixtree_position_list[[currentIdx]])
}

GetSequencialJoinPositionList <- function(xLeafIdx,yLeafIdx,currentIdx){
    xLeafPositionList <- prefixtree_position_list[[xLeafIdx]]
    yLeafPositionList <- prefixtree_position_list[[yLeafIdx]]
    poslist <- list()
    pIdx <- 1
    for( idx in seq(length(xLeafPositionList)) ){
        xlist <- xLeafPositionList[[idx]]
        ylist <- NULL
        
        for( jdx in seq(length(yLeafPositionList)) ){
            ylist <- yLeafPositionList[[jdx]]
            if( xlist[1]==ylist[1] ){
                break
            }
        }
        currentList <- xlist[1]
        if( jdx<=length(yLeafPositionList) ){
            for( yPos in ylist[-1] ){
                if( any(xlist[-1]<yPos) ){
                    currentList <- c(currentList,yPos)
                }
            }
        }
        if( length(currentList)>1 ){
            poslist[[pIdx]] <- currentList
            pIdx <- pIdx + 1
        }
    }
    prefixtree_position_list[[currentIdx]] <<- poslist
    length(prefixtree_position_list[[currentIdx]])
}

SPADE <- function(data,currentNodeIdx,currentLeafIndex,MIN_SUP,k){
    currentLeavesIndex <- GetLeavesIdx(currentNodeIdx)
    for( xLeafIdx in currentLeavesIndex ){
        Add2FrequentSequencesSet(xLeafIdx)
        for( yLeafIdx in currentLeavesIndex ){
            extendedLeaf <- CombineStr(xLeafIdx,yLeafIdx)
            support_count <- GetSequencialJoinPositionList(xLeafIdx,yLeafIdx,currentLeafIndex)
            if( support_count>=MIN_SUP ){
                AddLeaf(xLeafIdx,currentLeafIndex,extendedLeaf)
                currentLeafIndex <- currentLeafIndex + 1
            }
        }
        if( !NoExtensions(xLeafIdx) ){
            currentLeafIndex <- SPADE(data,xLeafIdx,currentLeafIndex,MIN_SUP,k+1)
        }
    }
    currentLeafIndex
}

Add2FrequentSequencesSet <- function(leafIdx){
    ff <<- c(ff, prefixtree_data[leafIdx])
}

GetLeavesIdx <- function(currentNodeIdx){
    currentLeavesIndex <- c()
    if( prefixtree_head[currentNodeIdx,3]>0 )
        currentLeavesIndex <- seq(prefixtree_head[currentNodeIdx,3]) + prefixtree_head[currentNodeIdx,2] - 1
    currentLeavesIndex    
}

NoExtensions <- function(leafIdx){
	prefixtree_head[leafIdx,3]==0
}

CombineStr <- function(leafIdx,siblingIdx){
	leaf <- prefixtree_data[leafIdx]
	sibling <- prefixtree_data[siblingIdx]
	paste(leaf,substring(sibling,nchar(sibling)),sep="")
}

AddLeaf <- function(leafIdx,currentIdx,extendedLeaf){
	#update current leaf information
	prefixtree_data <<- c(prefixtree_data,extendedLeaf)
	prefixtree_head <<- rbind(prefixtree_head,c(leafIdx,0,0,0))

	#update parent information
	if( prefixtree_head[leafIdx,2]==0 ){
		prefixtree_head[leafIdx,2] <<- currentIdx
	}
	prefixtree_head[leafIdx,3] <<- prefixtree_head[leafIdx,3] + 1
}

testSPADE(itemsets,items,min_sup)
rownames(ff) <- NULL
print("ff:")
print(ff)
