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
  S1 <- n*buildSigma(Th,t.hat,n,F)
  S2 <- n*buildSigma(Th,rep(t.bar,p),n,F)
  
  for(i in 1:3){
    S1[which(BtB == i-1)] <- mean(S1[which(BtB == i-1)])
    S2[which(BtB == i-1)] <- mean(S2[which(BtB == i-1)])
  }
  
  st1 <- S1[1,c(1,2,p)]
  st2 <- S2[1,c(1,2,p)]
  
  dt1 <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% st1
  dt2 <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% st2
  
  maha1 <- n*mahalanobis(t.hat-t.bar,F,S1)
  maha2 <- n*mahalanobis(t.hat-t.bar,F,S2)
  
  euc <- c(n*crossprod(t.hat-t.bar))
  sup <- sqrt(n)*max(abs(t.hat-t.bar))
  
  
  #### p-values
  MCeuc1 <- dt1[1]*rchisq(M, p-d) + dt1[2]*rchisq(M, d-1)
  MCeuc2 <- dt2[1]*rchisq(M, p-d) + dt2[2]*rchisq(M, d-1)

  MCsup1 <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(st1[2]-st1[3]))
    Z2 <- rnorm(p,0,sqrt(dt1[1]))
    Z <- Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
    Z - mean(Z)
  })
  
  MCsup2 <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(st2[2]-st2[3]))
    Z2 <- rnorm(p,0,sqrt(dt2[1]))
    Z <- Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
    Z - mean(Z)
  })
  
  pvals <- c(
    pmaha1 = pchisq(maha1,p-1,lower.tail = F),
    pmaha2 = pchisq(maha2,p-1,lower.tail = F),
    
    peuc1 = mean(MCeuc1 > euc),
    peuc2 = mean(MCeuc2 > euc),

    psup1 = mean(apply(MCsup1, 2, function(z) max(abs(z))) > sup),
    psup2 = mean(apply(MCsup2, 2, function(z) max(abs(z))) > sup)
  )               
               
  pvals
}  



