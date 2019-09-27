computeThChunks <- function(X,G1,G2){
  
  n <- dim(X)[1]
  one.vec <- rep(1,n)
  
  indicatorMatrixFun <- function(Y){
    M1 <- matrix(Y, n, n, byrow = TRUE)
    M2 <- matrix(Y, n, n)
    1 * (M1 < M2)
  }

  if(identical(G1,G2)){
    d <- length(G1)
    p <- d*(d-1)/2
    ij.mat <- t(combn(d,2))
    
    I.list <- sapply(G1,function(i){indicatorMatrixFun(X[,i])},simplify=FALSE)
    I2.mat <- apply(ij.mat,1,function(ij){I <- I.list[[ij[1]]]*I.list[[ij[2]]]; c(I,I+t(I))})
    IJ.vec <- sapply(1:p, function(l){t(one.vec) %*% matrix(I2.mat[(n^2+1):(2*n^2),l],n,n)})
    
  }else{
    d1 <- length(G1)
    d2 <- length(G2)
    ij.mat <- as.matrix(expand.grid(1:d1,1:d2))
    p <- d1*d2
    
    I.list.G1 <- sapply(G1,function(i){indicatorMatrixFun(X[,i])},simplify=FALSE)
    I.list.G2 <- sapply(G2,function(i){indicatorMatrixFun(X[,i])},simplify=FALSE)
    
    I2.mat <- apply(ij.mat,1,function(ij){I <- I.list.G1[[ij[1]]]*I.list.G2[[ij[2]]]; c(I,I+t(I))})
    IJ.vec <- sapply(1:p, function(l){t(one.vec) %*% matrix(I2.mat[(n^2+1):(2*n^2),l],n,n)})
  }
  
  (4 / (n * (n - 1)))^2 * (crossprod(IJ.vec) - crossprod(I2.mat[1:(n^2),],I2.mat[(n^2+1):(2*n^2),]))
}



# I2.list <- sapply(1:p,function(r){ij <- ij.mat[r,] ;I.list.G1[[ij[1]]]*I.list.G2[[ij[2]]]}, simplify = F)
# IJ.list <- sapply(1:p, function(l){I <- I2.list[[l]]; I + t(I)}, simplify = FALSE)
# IJ.mat <- sapply(1:p, function(l){I <- I2.list[[l]]; I + t(I)})
# one.vec <- rep(1,n)
# IJ.vec <- sapply(1:p, function(l){t(one.vec) %*% IJ.list[[l]]})
# I2.mat <- matrix(unlist(I2.list), n^2, p)
# IJ.mat <- matrix(unlist(IJ.list), n^2, p)
# (4 / (n * (n - 1)))^2 * (crossprod(IJ.vec) - crossprod(I2.mat,IJ.mat))

