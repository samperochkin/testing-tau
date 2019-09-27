testEquiRankSim <- function(X, Mmc, Mboot){
  
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
  
  # Monte Carlo
  s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
  
  MC <- replicate(Mmc, {
    Z1 <- rnorm(d,0,sqrt(s10))
    Z2 <- rnorm(p,0,sqrt(s210))
    Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
  })
  
  supFun <- function(tt){max(abs(tt-mean(tt)))}
  supMC <- apply(MC, 2, supFun)
  psupMC <-  mean(supMC > sqrt(n)*max(abs(t.hat-t.bar)))
  
  # Boot
  C.gen <- sapply(1:n, function(r){
    V <- t(X[r,] < t(X[-r,]))
    V <- rowSums(apply(V, 1, function(v){
      4*(outer(v,v,"=="))[ij.mat]
    }))/(n*(n-1))
    V - mean(V) - t.cen/n
  })
  
  # tiime <- Sys.time()
  boot.gen <- replicate(Mboot, {
    rowSums(C.gen %*% rnorm(n))
  })
  # print(difftime(Sys.time(), tiime))
  
  supChen.gen <- apply(boot.gen, 2, function(tt) max(abs(tt)))
  psupChen.gen <-  mean(supChen.gen > max(abs(t.cen)))
  
  c(pval_MC = psupMC, pval_boot_gen = psupChen.gen)
}
