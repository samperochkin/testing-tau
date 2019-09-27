n <- 200
d <- 200

pvalsMC <- replicate(1000, {
  sort(runif(d+1))
})


decisions <- replicate(1000, {
  X <- rexp(n,1) + matrix(log(abs(rnorm(n*d,0,1.5))),n,d)
  # X[,1:2] <- X[,1:2] + rnorm(n,0,3)
  # X[,1:15] <- X[,1:15] + rnorm(n,0,3)
  
  Tau.hat <- cor.fk(X)

  # pvals1 <- shapiro.test((colSums(Tau.hat)-1)/(d-1))$p.value
  pvals2 <- sapply(1:d, function(i) shapiro.test(Tau.hat[i,-i])$p.value)
  # hist(c(pvals1,pvals2), breaks = 25)
  # c(pvals1,pvals2)
  # sort(c(pvals1,pvals2))
  # 
  sort(pvals2)
  # goftest::cvm.test(pvals2)$p
})

# mean(decisions<.05)

plot(sort(decisions[1,]), ylim=c(0,1), type="l", lwd=2, col=heat.colors(100)[1])
sapply(2:(d+1), function(i){
  lines(sort(decisions[i,]), ylim=c(0,1), col=heat.colors(100)[i], lwd=2)
})

pairs(t(decisions[1:5,]))
hist(decisions[1,])


al <- .05
# k <- round(al*(d+1))
k <- round(al*d)
# qqplot(decisions[k,], replicate(500, {sort(runif(d+1))[k]}), type="l", col="red")
qqplot(decisions[k,], replicate(500, {sort(runif(d))[k]}), type="l", col="red")
abline(a=0,b=1)

