testEquiRank <- function(X, M = 2000){
  
  # setup -------------------------------------------------------------------
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p
  
  
  # computation of estimates ------------------------------------------------
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  t.cen <- t.hat - t.bar
  
  # computation of estimates: orthogonal components
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B))
  
  
  #### Plugin tests
  Th <- computeTh(X)
  Sh <- n*buildSigma(Th,rep(t.bar,p),n,F)
  
  for(i in 1:3){
    Sh[which(BtB == i-1)] <- mean(Sh[which(BtB == i-1)])
  }
  
  sigma <- Sh[1,c(1,2,p)]

  delta <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma

  maha_d <- n*mahalanobis(c((G-1/p) %*% t.hat),F,Sh)
  maha_p <- n*mahalanobis(c((diag(p)-G) %*% t.hat),F,Sh)
  maha <- maha_d + maha_p
  euc <- c(n*crossprod(t.hat-t.bar))
  sup <- sqrt(n)*max(abs(t.hat-t.bar))
  
  
  #### p-values
  MCeuc <- delta[1]*rchisq(M, p-d) + delta[2]*rchisq(M, d-1)
  
  MCsup <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(sigma[2]-sigma[3]))
    Z2 <- rnorm(p,0,sqrt(delta[1]))
    Z <- Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
    max(abs(Z - mean(Z)))
  })
  

  pvals <- c(
    pmaha = pchisq(maha,p-1,lower.tail = F),
    pmaha_d = pchisq(maha_d,d-1,lower.tail = F),
    pmaha_p = pchisq(maha_p,p-d,lower.tail = F),
    peuc = mean(MCeuc > euc),
    psup = mean(MCsup > sup)
  )               
               
  pvals
}  



