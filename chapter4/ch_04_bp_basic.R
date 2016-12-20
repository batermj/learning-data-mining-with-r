#clean the workspace and memory
rm( list=ls() )
gc()

data <- read.csv("data/datasets011.csv", header=TRUE)
data <- as.data.frame(data)

TestBPNN <- function(data){
	set.seed(5)
	n <- length(data[,1])
	samp <- sample(1:n,n/5)
	traind <- data[-samp,c(1,2)]
	train1 <- data[-samp,3]
	testd <- data[samp,c(1,2)]
	test1 <- data[samp,3]

	set.seed(1)
	ntrainnum <- 120
	nsampdim <- 2

	net.nin <- 2
	net.nhidden <- 3
	net.nout <- 1
	w <- 2*matrix(runif(net.nhidden*net.nin)-0.5,net.nhidden,net.nin)
	b <- 2*(runif(net.nhidden)-0.5)
	net.w1 <- cbind(w,b)
	W <- 2*matrix(runif(net.nhidden*net.nout)-0.5,net.nout,net.nhidden)
	B <- 2*(runif(net.nout)-0.5)
	net.w2 <- cbind(W,B)

	traind_s <- traind
	traind_s[,1] <- traind[,1]-mean(traind[,1])
	traind_s[,2] <- traind[,2]-mean(traind[,2])
	traind_s[,1] <- traind_s[,1]/sd(traind_s[,1])
	traind_s[,2] <- traind_s[,2]/sd(traind_s[,2])
 
	sampinex <- rbind(t(traind_s),rep(1,ntrainnum))
	expectedout <- train1

	eps <- 0.01
	a <- 0.3
	mc <- 0.9
	maxiter <- 2000
	iter <- 0
 
	errrec <- rep(0,maxiter)
	outrec <- matrix(rep(0,ntrainnum*maxiter),ntrainnum,maxiter)
 
	sigmoid <- function(x){
		return(1/(1 + exp(-x)))
	}

	for(i in seq(maxiter)){
		hid_input <- net.w1%*%sampinex
		hid_out <- sigmoid(hid_input)
		out_input1 <- rbind(hid_out,rep(1,ntrainnum))
		out_input2 <- net.w2%*%out_input1
		out_out <- sigmoid(out_input2)
		outrec[,i] <- t(out_out)
		err <- expectedout-out_out
		sse <- sum(err^2)
		errrec[i] <- sse
		iter <- iter + 1
		if( sse<=eps ){
			break
		}
 
		Delta <- err*sigmoid(out_out)*(1-sigmoid(out_out))
		delta <- (matrix(net.w2[,1:(length(net.w2[1,])-1)]))%*%Delta*sigmoid(hid_out)*(1-sigmoid(hid_out))
 
		dWex <- Delta%*%t(out_input1)
		dwex <- delta%*%t(sampinex)
 
		if(i==1){
			net.w2 <- net.w2 + a*dWex
			net.w1 <- net.w1 + a*dwex
		}else{
			net.w2 <- net.w2 + (1-mc)*a*dWex + mc*dWexold
			net.w1 <- net.w1 + (1-mc)*a*dwex + mc*dwexold
		}
 
		dWexold <- dWex
		dwexold <- dwex
	}

	testd_s <- testd
	testd_s[,1] <- testd[,1]-mean(testd[,1])
	testd_s[,2] <- testd[,2]-mean(testd[,2])
	testd_s[,1] <- testd_s[,1]/sd(testd_s[,1])
	testd_s[,2] <- testd_s[,2]/sd(testd_s[,2])
 
	inex <- rbind(t(testd_s),rep(1,150-ntrainnum))
	hid_input <- net.w1%*%inex
	hid_out <- sigmoid(hid_input)
	out_input1 <- rbind(hid_out,rep(1,150-ntrainnum))
	out_input2 <- net.w2%*%out_input1
	out_out <- sigmoid(out_input2)
	out_out1 <- out_out
 
	out_out1[out_out<0.5] <- 0
	out_out1[out_out>=0.5] <- 1
 
	rate <- sum(out_out1==test1)/length(test1)
}

rate <- TestBPNN(data)
print(rate)
