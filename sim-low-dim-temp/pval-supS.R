# Monte Carlo sample
mc_sup <- sapply(seq(5,20,5), function(d){
  p <- d*(d-1)/2
  replicate(10000, {
    ts <- rnorm(p)
    max(abs(ts - mean(ts)))
  })
})
colnames(mc_sup) <- seq(5,20,5)

# Compute Monte Carlo quantile
monteCarlo <- function(loss,d){
  k <- unique(d)
  sapply(loss, function(ll){
    mean(mc_sup[,as.character(k)] > ll)
  })
}

res[loss_type == "supS", pval := monteCarlo(loss,d), .(d)]



######################

mc_sup <- sapply(seq(5,20,5), function(d){
  p <- d*(d-1)/2
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[t(combn(d,2))] <- 1:p
  ij.l.mat <- ij.l.mat + t(ij.l.mat)
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B)) - 1/p
  
  replicate(10000, {
    max(abs(G %*% rnorm(p)))
  })
})
colnames(mc_sup) <- seq(5,20,5)

res[loss_type == "supS2", pval := monteCarlo(loss,d), .(d)]



######################


mc_sup <- sapply(seq(5,20,5), function(d){
  p <- d*(d-1)/2
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[t(combn(d,2))] <- 1:p
  ij.l.mat <- ij.l.mat + t(ij.l.mat)
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  G <- diag(p) - as.matrix(B %*% solve(crossprod(B)) %*% t(B))
  
  replicate(10000, {
    max(abs(G %*% rnorm(p)))
  })
})
colnames(mc_sup) <- seq(5,20,5)

res[loss_type == "supS3", pval := monteCarlo(loss,d), .(d)]
