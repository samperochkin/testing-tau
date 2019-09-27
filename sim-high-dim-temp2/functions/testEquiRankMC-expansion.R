testEquiRankMC <- function(X, M){
  
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
  
  s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
  
  # s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  # s10 <- n*crossprod(t.col - t.bar)/(d-1)

  
  # MCI <- matrix(rnorm(M*p),p,M)
  MCS1 <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(s10))
    Z1[ij.mat[,1]] + Z1[ij.mat[,2]] - 2*mean(Z1)
  })
  MCS2 <- replicate(M, {
    Z2 <- rnorm(p,0,sqrt(s210))
  })
  
  supFun <- function(tt){max(abs(tt-mean(tt)))}

  supMCS <- apply(MCS1 + MCS2, 2, supFun)
  supMCS1 <- apply(MCS1, 2, supFun)
  supMCS2 <- apply(MCS2, 2, supFun)
  
  psup_total <-  mean(supMCS > sqrt(n)*max(abs(t.hat-t.bar)))
  psup_10 <-  mean(supMCS1 > sqrt(n)*max(abs(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)))
  psup_210 <-  mean(supMCS2 > sqrt(n)*max(abs(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)))
  
  c(psup_total=psup_total, psup_10=psup_10, psup_210=psup_210)
}
