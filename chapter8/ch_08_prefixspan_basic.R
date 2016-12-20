library(utils)

#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/datasets004.csv",header=FALSE)
print(tbl)

tbl <- as.matrix(tbl)
itemsets <- as.vector(tbl)
print(itemsets)

items <- c('A','C','G','T')
min_sup <- 3

ff <- NULL
#c(parent,first_child_pos,num_of_children,sup)
prefixtree_head <- NULL
prefixtree_data <- list(list())
prefixtree_symbol_support <- NULL

testPrefixSpan <- function(data,base_items,MIN_SUP){
    currentLeafIndex <- InitPrefixSpan(data,base_items,MIN_SUP)
	currentNodeIdx <- 1
    PrefixSpan(base_items,"",currentNodeIdx,currentLeafIndex,MIN_SUP)
}

InitPrefixSpan <- function(data,base_items,MIN_SUP){
    currentLeafIndex <- 1
    subSequenceList <- list()
    acc_support <- rep(0,length(base_items))
    names(acc_support) <- base_items
    for( idx in seq(length(data)) ){
        subSequenceList[[idx]] <- data[idx]
        for( c in base_items ){
            acc_support[c] <- acc_support[c] + length(which(strsplit(data[idx],"")[[1]]==c))
        }
    }
    prefixtree_data[[currentLeafIndex]] <<- subSequenceList
    prefixtree_symbol_support[[currentLeafIndex]] <<- acc_support
    prefixtree_head <<- rbind(prefixtree_head,c(0,0,0,1000))
    currentLeafIndex + 1
}

PrefixSpan <- function(baseItemsSet,sequenceStr,currentNodeIdx,currentLeafIndex,MIN_SUP){
    baseItemsSetSupport <- GetBaseItemsSupport(currentNodeIdx)
    currentNodeDataset <- GetCurrentNodeDataset(currentNodeIdx)
    for( c in baseItemsSet ){
        if(baseItemsSetSupport[c]>=MIN_SUP){
            extendedStr <- paste(sequenceStr,c,sep = "")
            Add2FrequentSequencesSet(extendedStr)
            subSequenceList <- list()
			jdx <- 1
            for( idx in seq(length(currentNodeDataset)) ){
                itemStr <- currentNodeDataset[[idx]]
                itemStr <- GetProjectionStr(itemStr,c)
                itemStr <- RemoveInfrequentPart(itemStr,baseItemsSet,baseItemsSetSupport,MIN_SUP)
                if( nchar(itemStr)>0 ){
                    subSequenceList[[jdx]] <- itemStr
					jdx <- jdx + 1
                }
            }
			if( length(subSequenceList)>0 ){
            	AddLeaf(currentNodeIdx,currentLeafIndex,subSequenceList,baseItemsSet)
            	currentLeafIndex <- currentLeafIndex + 1
                currentLeafIndex <- PrefixSpan(baseItemsSet,extendedStr,currentLeafIndex-1,currentLeafIndex,MIN_SUP)
            }
        }
    }
    currentLeafIndex
}

RemoveInfrequentPart <- function(itemStr,baseItemsSet,baseItemsSetSupport,MIN_SUP){
    for( c in baseItemsSet ){
        if( baseItemsSetSupport[c]<MIN_SUP ){
            itemStr <- paste(strsplit(itemStr,"")[[1]][which(strsplit(itemStr,"")[[1]]!=c)],sep="",collapse="")
        }
    }
    itemStr
}

GetProjectionStr <- function(itemStr,sym){
	rtnStr <- ""
	if( any(strsplit(itemStr,"")[[1]]==sym) ){
    	pos <- which(strsplit(itemStr,"")[[1]]==sym)[1] + 1
    	rtnStr <- substring(itemStr,pos)
	}
	rtnStr
}

GetCurrentNodeDataset <- function(currentNodeIdx){
    prefixtree_data[[currentNodeIdx]]
}

GetBaseItemsSupport <- function(currentNodeIdx){
    prefixtree_symbol_support[[currentNodeIdx]]
}

Add2FrequentSequencesSet <- function(extendedStr){
    ff <<- c(ff, extendedStr)
}

NoExtensions <- function(currentNodeIdx){
	length(prefixtree_data[[currentNodeIdx]])==0
}

AddLeaf <- function(currentNodeIdx,currentLeafIndex,subSequenceList,baseItemsSet){
	#update current leaf information
	prefixtree_data[[currentLeafIndex]] <<- subSequenceList
	acc_support <- rep(0,length(baseItemsSet))
	names(acc_support) <- baseItemsSet
	for( idx in seq(length(subSequenceList)) ){
	    for( c in baseItemsSet ){
	        acc_support[c] <- acc_support[c] + length(which(strsplit(subSequenceList[[idx]],"")[[1]]==c))
	    }
	}
	prefixtree_symbol_support[[currentLeafIndex]] <<- acc_support
	prefixtree_head <<- rbind(prefixtree_head,c(currentNodeIdx,0,0,0))

	#update parent information
	if( prefixtree_head[currentNodeIdx,2]==0 ){
		prefixtree_head[currentNodeIdx,2] <<- currentLeafIndex
	}
	prefixtree_head[currentNodeIdx,3] <<- prefixtree_head[currentNodeIdx,3] + 1
}

testPrefixSpan(itemsets,items,min_sup)
rownames(ff) <- NULL
print("ff:")
print(ff)
