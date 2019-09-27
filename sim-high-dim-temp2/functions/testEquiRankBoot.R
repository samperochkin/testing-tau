testEquiRankBoot <- function(X, M){
  
  # setup -------------------------------------------------------------------
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  # ij.l.mat <- matrix(0,d,d)
  # ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
  
  # computation of estimates ------------------------------------------------
  tau.hat <- cor.fk(X)[ij.mat]
  tau.cen <- tau.hat - mean(tau.hat)
  
  C <- sapply(1:n, function(r){
    V <- t(X[r,] < t(X))
    rowSums(apply(V, 1, function(v){
      vv <- (outer(v,v,"-") == 0)[ij.mat]
      4*(vv - mean(vv)) - tau.cen
    }))/(n*(n-1))
  })

  # tiime <- Sys.time()
  boot <- replicate(M, {
    rowSums(C %*% rnorm(n))
  })
  # print(difftime(Sys.time(), tiime))
  
  supChen <- apply(boot, 2, function(tt) max(abs(tt)))
  psup <-  mean(supChen > max(abs(tau.cen)))
  
  psup
}


# library(HAC)
# library(mvtnorm)
# library(pcaPP)
# library(Matrix)
# source("functions/generateData.R")
# 
# X <- generateData(n = 150,
#                   d = 150,
#                   tau = .3,
#                   dtau = 0,
#                   #dtau_type = "column",
#                   distribution = "joe")
#   
#   
# tiime <- Sys.time()
# testEquiRankBoot(X, 500)
# difftime(Sys.time(), tiime)
# 
# # takes less than 30 secs for worst case





