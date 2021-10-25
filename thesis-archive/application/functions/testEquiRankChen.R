testEquiRankChen <- function(X, clus, Mboot){
  
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
  
  # clusters ----------------------------------------------------------------
  
  K <- length(unique(clus))
  kk.mat <- matrix(clus[ij.mat],p,2)
  kk.mat <- t(apply(kk.mat,1,sort))
  
  block <- (kk.mat - 1) %*% c(K,1) + 1
  
  L <- 0
  for(b in sort(unique(block))){
    ind <- which(block == b)
    if(length(ind) > 0){
      L <- L + 1
      block[ind] <- L
    }
  }
  
  B <- matrix(0,p,L)
  B[cbind(1:p,block)] <- 1
  t.bar <- c(((t.hat %*% B)/colSums(B)) %*% t(B))
  
  t.cen <- t.hat - t.bar
  

  # Boot
  C.gen <- sapply(1:n, function(r){
    V <- t(X[r,] < t(X[-r,]))
    V <- rowSums(apply(V, 1, function(v){
      4*(outer(v,v,"=="))[ij.mat]
    }))/(n*(n-1))
    
    V - c(((V %*% B)/colSums(B)) %*% t(B)) - t.cen/n
  })
  
  # tiime <- Sys.time()
  boot.gen <- replicate(Mboot, {
    rowSums(C.gen %*% rnorm(n))
  })
  # print(difftime(Sys.time(), tiime))
  
  supChen.gen <- apply(boot.gen, 2, function(tt) max(abs(tt)))
  psupChen.gen <-  mean(supChen.gen > max(abs(t.cen)))
  
  c(pval_boot_gen = psupChen.gen)
}
