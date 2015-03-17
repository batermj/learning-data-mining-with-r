#clean the workspace and memory
rm( list=ls() )
gc()

library(bnlearn)
data(learning.test)

data2 <- learning.test

bayesnet <- hc(data2)

bayesnet<- set.arc(bayesnet,'B','F')

fitted <- bn.fit(bayesnet, data2,method='mle')

pre <- predict(fitted,data=data2,node='E')
