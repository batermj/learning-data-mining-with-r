#clean the workspace and memory
rm( list=ls() )
gc()

library(cluster);

data005 <- read.csv("data/datasets005.csv", header=TRUE)
data005 <- as.data.frame(data005)
clarax <- clara(data005, 2, samples=50)
print(clarax)

all.equal(clarax[-8],clara(data005, 2, samples=50, pamLike = TRUE)[-8])
plot(clarax)

data006 <- read.csv("data/datasets006.csv", header=TRUE)
data006 <- as.data.frame(data006)
clx3 <- clara(data006, 3)
print(clx3)
cl.3 <- clara(data006, 3, samples=100)
print(clx3)

stopifnot(cl.3$clustering == clx3$clustering)
nSim <- 100
nCl <- 3
set.seed(421)
cl <- matrix(NA,nrow(data006), nSim)
for(i in seq(nSim)){
	cl[,i] <- clara(data006, nCl, medoids.x = FALSE, rngR = TRUE)$cluster
}
tcl <- apply(cl,1, tabulate, nbins = nCl)

iDoubt <- which(apply(tcl,2, function(n) all(n < nSim)))
if(length(iDoubt)) {
	tabD <- tcl[,iDoubt, drop=FALSE]
	dimnames(tabD) <- list(cluster = paste(1:nCl), obs = format(iDoubt))
	t(tabD)
}
