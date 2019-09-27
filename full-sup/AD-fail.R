n <- 200
d <- 50

pvalsMC <- replicate(1000, {
  sort(runif(d+1))
})


decisions <- replicate(500, {
  X <- rnorm(n,0,1) + matrix(rnorm(n*d,0,1.5),n,d)
  # X[,1:15] <- X[,1:15] + rnorm(n,0,3)
  
  Tau.hat <- cor.fk(X)
  
  pvals1 <- ADGofTest::ad.test(x = (colSums(Tau.hat)-1)/(d-1), distr.fun = pnorm)$p.value
  pvals2 <- sapply(1:d, function(i) ADGofTest::ad.test(Tau.hat[i,-i], distr.fun = pnorm)$p.value)
  # hist(c(pvals1,pvals2), breaks = 25)
  sort(c(pvals1,pvals2))
  # 
  # shapiro.test(Tau.hat[t(combn(d,2))])$p.value
})

# qqplot(decisions, pvalsMC, type="l", col="red")
# abline(a=0,b=1)


plot(sort(decisions[1,]), ylim=c(0,1), type="l", lwd=2)
sapply(2:(d+1), function(i){
  lines(sort(decisions[i,]), ylim=c(0,1), col=i, lwd=2)
})
# 
# rbind(seq(d+1)/(d+2),rowMeans(decisions))
