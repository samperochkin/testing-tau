testEquiRankMC2 <- function(X){
  # setup -------------------------------------------------------------------
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  # ij.l.mat <- matrix(0,d,d)
  # ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
  
  
  # computation of estimates ------------------------------------------------
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  
  # s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  # s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
  # s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  # s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
  #############
  
  pvals1 <- pnorm(t.col,
                  t.bar,
                  sd(t.col))
  # pvals1 <- pnorm(t.col,
  #                 t.bar,
  #                 sqrt(s10/n))
  pvals2 <- pnorm(t.hat,
                  t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - t.bar,
                  sd(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar))
  # pvals2 <- pnorm(t.hat,
  #                 t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - t.bar,
  #                 sqrt(s210/n))
  

  pvals <- c(pvals1,pvals2)
  
  ks.test(pvals,"punif")$p.value
  # c(
  #   ks.test(pvals1,"punif")$p.value,
  #   ks.test(pvals2,"punif")$p.value
  # )
}


testEquiRankMC2(X)


n <- 200
d <- 50

pvals <- replicate(300, {
  X <- rnorm(n,0,1) + matrix(rnorm(n*d,0,1.5),n,d)
  X[,1:15] <- X[,1:15] + rnorm(n,0,1)
  testEquiRankMC2(X)
})

plot(pvals)
mean(pvals < .05)


pvalsMC <- replicate(1000, {
  min(runif(d-1))
  # ks.test(runif(d),"punif")$p.value
})

qqplot(pvals, pvalsMC, type="l", col="red")
abline(a=0,b=1)




plot(pvals)
hist(pvals, breaks=50)
quantile(pvals, prob = c(.001, .01, .05, .1,.5))

hist(pvals[1,], breaks=50)
hist(pvals[2,], breaks=50)
quantile(pvals, prob = c(.001, .01, .05, .1,.5))
