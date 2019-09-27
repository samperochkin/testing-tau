testEquiRankPlugin <- function(X, M = 2000){
  
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
  t.star <- (d-1)/(d-2)*(t.col[ij.mat[,1]] + t.col[ij.mat[,2]]) - d/(d-2)*t.bar
  
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

  maha_d <- n*mahalanobis(t.star-t.bar,F,Sh)
  maha_p <- n*mahalanobis(t.hat-t.star,F,Sh)
  maha <- maha_d + maha_p
  euc <- c(n*crossprod(t.hat-t.bar))
  
  sup <- sqrt(n)*max(abs(t.hat-t.bar))
  sup_p <- sqrt(n)*max(abs(t.hat-t.star))
  sup_d <- sqrt(n)*max(abs(t.col-t.bar))
  sups <- sqrt(n)*max(abs((t.hat-t.star) * sqrt(1/delta[1]) + (t.star-t.bar) * sqrt(1/delta[2])))
  
  
  #### p-values
  MCeuc <- delta[1]*rchisq(M, p-d) + delta[2]*rchisq(M, d-1)
  
  MCsup <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(sigma[2]-sigma[3]))
    Z2 <- rnorm(p,0,sqrt(delta[1]))
    Z.hat <- Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
    Z.bar <- mean(Z.hat)
    Z.col <- sapply(1:d, function(i) mean(Z.hat[ij.l.mat[,i]]))
    Z.star <- (d-1)/(d-2)*(Z.col[ij.mat[,1]] + Z.col[ij.mat[,2]]) - d/(d-2)*Z.bar
    
    sapply(list(Z.hat - Z.bar,
                Z.hat - Z.star,
                Z.col - Z.bar),function(z) max(abs(z)))
  })
  
  MCsups <- replicate(M, {z <- rnorm(p); max(abs(z-mean(z)))})
  
  pvals <- c(
    maha = pchisq(maha,p-1,lower.tail = F),
    maha_d = pchisq(maha_d,d-1,lower.tail = F),
    maha_p = pchisq(maha_p,p-d,lower.tail = F),
    euc = mean(MCeuc > euc),
    
    sups = mean(MCsups > sups),
    sup_d = mean(MCsup[3,] > sup_d),
    sup_p = mean(MCsup[2,] > sup_p),
    sup = mean(MCsup[1,] > sup)
  )               
               
  pvals
}  



