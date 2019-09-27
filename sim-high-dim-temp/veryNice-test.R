n <- 150
d <- 150
p <- d*(d-1)/2

pvalsMC <- replicate(1000, {
  sort(runif(d+1))
})


decisions <- replicate(1000, {
  X <- rexp(n,1) + matrix(log(abs(rnorm(n*d,0,1.5))),n,d)
  # X[,1:2] <- X[,1:2] + rnorm(n,0,3)
  # X[,1:15] <- X[,1:15] + rnorm(n,0,.5)
  
  l.ij.mat <- t(combn(d,2))
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[l.ij.mat]
  tau.bar <- mean(tau.hat)
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  tb <- as.vector(B %*% (solve(crossprod(B)) %*% crossprod(B,tau.hat)))
  
  Tb <- diag(d)
  Tb[rbind(l.ij.mat,l.ij.mat[,2:1])] <- tb
  # image(t((Tau.hat - Tb)[d:1,]))
  # image(t((Tb - tau.bar)[d:1,]))
  
  s2 <- n*crossprod(tau.hat - tb)/(p-d)
  s1 <- n*crossprod(tb-tau.bar)/(d*(d-1))
  
  # MONTE CARLO : very efficient.
  MC <- replicate(500, {
    Z1 <- rnorm(d,0,sqrt(s1))
    Z2 <- rnorm(p,0,sqrt(s2))
    
    Z <- Z2 + Z1[l.ij.mat[,1]] + Z1[l.ij.mat[,2]]
    
    max(abs(Z - mean(Z)))
  })
  
  mean(MC > sqrt(n)*max(abs(tau.hat-tau.bar)))
  testEquiRankMC(X,500)
})

# pvals <- pmin(decisions,1-decisions)
# mean(pvals<.025)
# mean(pvals<.05)
# hist(decisions, breaks=20)
# hist(pvals, breaks=20)
# quantile(decisions, seq(0,1,.05))
# quantile(pvals, seq(0,1,.05))

plot((1:length(decisions))/length(decisions),sort(decisions), type = "l", col="red")
lines(c(0,1),c(0,1))

plot((1:length(decisions))/length(decisions),sort(pvals)*2, type = "l", col="red")
lines(c(0,1),c(0,1))




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

