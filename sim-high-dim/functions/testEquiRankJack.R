testEquiRankJack <- function(X, Mmc){
  
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
  t.cen <- t.hat - t.bar
  
  # Monte Carlo Jack
  s.JK <- jackknifeVariance(X)
  s210.JK <- c(1,-2,1) %*% s.JK
  s10.JK <- c(0,1,-1) %*% s.JK

  MC.JK <- replicate(Mmc, {
    Z1 <- rnorm(d,0,sqrt(s10.JK))
    Z2 <- rnorm(p,0,sqrt(s210.JK))
    Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
  })
  
  supFun <- function(tt){max(abs(tt-mean(tt)))}
  supMC.JK <- apply(MC.JK, 2, supFun)
  psupMC.JK <-  mean(supMC.JK > sqrt(n)*max(abs(t.hat-t.bar)))
  
  
  c(pval_MC_JK = psupMC.JK)
}
  