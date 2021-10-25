n <- 50
d1 <- 5
d2 <- 13

res <- replicate(500, {
  
  X1 <- matrix(rnorm(n*d1,0,sqrt(.3))+rnorm(n,0,sqrt(.7)),n,d1)
  X2 <- matrix(rnorm(n*d2,0,sqrt(.3))+rnorm(n,0,sqrt(.7)),n,d2)
  
  X <- cbind(X1,X2)
  
  testEquiRankBootstrap(X, clus2 = c(rep(1,d1),rep(2,d2)), M = 2000)
})

#hist(res, breaks=50)

k <- 2
plot(sort(res[k,]),type="l")
abline(a=0,b=1/ncol(res),col="red")
quantile(res[k,], prob= seq(0,1,.01))
