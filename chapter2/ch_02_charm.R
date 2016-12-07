#clean the workspace and memory
rm( list=ls() )
gc()

tbl <- read.csv("data/itemsets002.csv", header=FALSE)
tbl <- as.matrix(tbl)
colnames(tbl) <- NULL
itemsets <- t(tbl)
print(itemsets)

items <- c(1,2,3,4,5)
min_sup <- 3 #0.22*nrow(itemsets)

f <- NULL
c <- NULL
ff <- NULL
cc <- NULL

testEclat <- function(data,base_items,MIN_SUP){
  print(data)
  p <- GetFrequentTidSets(data,base_items,MIN_SUP)
  print(p)
  Eclat(p,f,MIN_SUP,length(base_items))
  return(f)
}

testCharm <- function(data,base_items,MIN_SUP){
  print(data)
  p <- GetFrequentTidSets(data,base_items,MIN_SUP)
  print(p)
  Charm(p,MIN_SUP,length(base_items))
  return(f)
}

GetFrequentTidSets <- function(data,base_items,MIN_SUP){
  tidsets <- NULL
  data <- cbind(data,apply(data,1,sum))
  print(data)
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

TidsetSort <- function(p,parameter){
  len4x <- parameter
  len4t <- ncol(p)

  if( nrow(p)>1 ){
    p <- cbind(p,apply(p[,(len4x+1):len4t],1,sum))
    p <- p[order(p[,ncol(p)]),-ncol(p)]
  }
  p
}

ReplaceX <- function(p,xi,xij,parameter){
  len4x <- parameter
  for( idx in seq(nrow(p)) ){
    if( all(p[idx,1:len4x]*xi[1:len4x]==xi[1:len4x]) ){
        p[idx,1:len4x] <- ifelse( p[idx,1:len4x]|xij[1:len4x],1,0)
    }
  }
  p
}

RemoveX <- function(p,xj,parameter){
  rtn <- NULL
  for( idx in seq(nrow(p)) ){
    if( !( all(p[idx,]==xj)) ){
      rtn <- rbind(rtn,p[idx,])
    }
  }
  rtn
}

AddClosedFrequentItemset <- function(p){
    cc <<- rbind(cc,p)
}

CloseCheck <- function(cc,xi,parameter){
  len4x <- parameter
  len4t <- ncol(cc)
  rtn <- FALSE
  
  for( idx in seq(nrow(cc)) ){
    z <- cc[idx,]
    if( ( all((xi[1:len4x]*z[1:len4x])==xi[1:len4x]) && 
           all(xi[(len4x+1):len4t]==z[(len4x+1):len4t])) )
      rtn <- TRUE    
  }
  rtn
}

Charm <- function(p,MIN_SUP,parameter=NULL){
  len <- nrow(p)
  len4x <- parameter
  len4t <- ncol(p)
  
  rownames(p) <- NULL
  p <- TidsetSort(p,parameter)
  jv <- seq(len)
  
  for( idx in seq(len) ){
    xi <- p[idx,]
    pi <- NULL
    jv <- jv[-1]
    for( jdx in jv ){
      xj <- p[jdx,]
      xij <- MergeTidSets(xi,xj,parameter)
      if(GetSupport(xij,parameter)>=MIN_SUP){
        if( all(xi[(len4x+1):len4t]==xj[(len4x+1):len4t]) ){
          p <- ReplaceX(p,xi,xij,parameter)
          if( !IsEmptyTidSets(pi) ){
            pi <- ReplaceX(pi,xi,xij,parameter)
          }
          p <- RemoveX(p,xj,parameter)
          #replace xi with xij in current loop
          xi <- xij
        }else if( all((xi[(len4x+1):len4t]*xj[(len4x+1):len4t])==xi[(len4x+1):len4t]) ){
          p <- ReplaceX(p,xi,xij,parameter)
          if( !IsEmptyTidSets(pi) ){
            pi <- ReplaceX(pi,xi,xij,parameter)
          }
          #replace xi with xij in current loop
          xi <- xij
        }else{
          pi <- rbind(pi,xij)
        }
      }
    }
    if(!IsEmptyTidSets(pi)){
        Charm(pi,MIN_SUP,parameter)
    } 
    if( !CloseCheck(cc,xi,parameter) )
      AddClosedFrequentItemset(xi)
  }
}

testEclat(itemsets,items,min_sup)
testCharm(itemsets,items,min_sup)
rownames(ff) <- NULL
print("ff:")
print(ff)
rownames(cc) <- NULL
print("cc:")
print(cc)
