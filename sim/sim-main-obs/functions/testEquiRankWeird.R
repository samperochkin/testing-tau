testEquiRankWeird <- function(X, M = 2000){
  
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

  s210 <- n*crossprod(t.hat - t.star)/(p-d)
  s10 <- n*crossprod(t.star - t.bar)/(d*(d-1))
  # s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  # s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
  
  delta <- c(s210,s210+(d-4)*s10)
  
  sup <- sqrt(n)*max(abs(t.hat-t.bar))
  sup_p <- sqrt(n)*max(abs(t.hat-t.star))
  sup_d <- sqrt(n)*max(abs(t.col-t.bar))
  sups <- sqrt(n)*max(abs((t.hat-t.star) * sqrt(1/delta[1]) + (t.star-t.bar) * sqrt(1/delta[2])))
  
  #### p-values
  MCsup <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(s10))
    Z2 <- rnorm(p,0,sqrt(s210))
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
    sups = mean(MCsups > sups),
    sup_d = mean(MCsup[3,] > sup_d),
    sup_p = mean(MCsup[2,] > sup_p),
    sup = mean(MCsup[1,] > sup)
  )
               
  pvals
}  



