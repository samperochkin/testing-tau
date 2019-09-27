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
  
  # Boot
  C.gen <- lapply(1:n, function(r){
    v <- t(X[r,] < t(X[-r,]))
    v <- rowSums(apply(v, 1, function(v){
      4*(outer(v,v,"=="))[ij.mat]
    }))
    V <- diag(d)
    V[ij.mat] <- V[ij.mat[,2:1]] <- v #- mean(v)# - t.cen/n
    V
  })
  
  Tcs <- sapply(C.gen, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(C.gen, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  s2 <- mean(apply(Ths, 1, var))/(n*(n-1))
  s1 <- mean(apply(Tcs, 1, var))/(n*(n-1))
  s0 <- var(Tbs)/(n*(n-1))
  s <- c(s2,s1,s0)
  
  s210 <- c(1,-2,1) %*% s
  s10 <- c(0,1,-1) %*% s
  
  MC <- replicate(Mmc, {
    Z1 <- rnorm(d,0,sqrt(s10))
    Z2 <- rnorm(p,0,sqrt(s210))
    Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
  })
  
  supChen <- apply(MC, 2, function(tt) max(abs(tt)))
  psupChen <-  mean(supChen > sqrt(n)*max(abs(t.cen)))
  
  c(pval_chen_MC = psupChen)
}
  