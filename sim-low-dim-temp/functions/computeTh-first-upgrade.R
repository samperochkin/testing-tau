# Upgrade Matrix
computeTh <- function(X){
  require(Matrix)
  
  d <- dim(X)[2]
  p <- d * (d - 1) / 2
  n <- dim(X)[1]
  
  l.ij.mat <- t(combn(d,2))
  # l.ij.mat <- t(sapply(1:p,function(l){
  #   i <- d-sum(l<=cumsum(d-(1:(d-1))))
  #   j <- l - ((i-1)*d - i*(i-1)/2 - i)
  #   c(i,j)
  # }))
  
  indicatorMatrixFun <- function(Y){
    1*outer(Y,Y,">")
  }

  tt <- Sys.time()
  I.list <- sapply(1:d,function(i){indicatorMatrixFun(X[,i])},simplify=FALSE)
  I2.list <- sapply(1:p,function(l){I <- I.list[[l.ij.mat[l,1]]]*I.list[[l.ij.mat[l,2]]]}, simplify = FALSE)
  IJ.list <- sapply(I2.list, function(I){I + t(I)}, simplify = FALSE)
  difftime(Sys.time(), tt)
    
  one.vec <- rep(1,n)
  IJ.vec <- sapply(IJ.list, function(IJ){crossprod(one.vec,IJ)})
  I2.mat <- matrix(unlist(I2.list), n^2, p)
  IJ.mat <- matrix(unlist(IJ.list), n^2, p)

  (4 / (n * (n - 1)))^2 * (crossprod(IJ.vec) - crossprod(I2.mat,IJ.mat))
}
