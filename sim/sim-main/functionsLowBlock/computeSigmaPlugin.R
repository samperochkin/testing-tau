computeSigmaPlugin <- function(X,ij.mat,tau=NULL,both=F,clus=NULL){
  
  n <- dim(X)[1]
  d <- dim(X)[2]
  p <- choose(d,2)

  if(nrow(ij.mat) != p) print("Potential problem in Sigma Plugin")
  if(is.null(tau)) tau <- pcaPP::cor.fk(X)[ij.mat]

  indicatorMatrixFun <- function(Y){
    M1 <- matrix(Y, n, n, byrow = TRUE)
    M2 <- matrix(Y, n, n)
    1 * (M1 < M2)
  }
  
  I.list <- sapply(1:d,function(i){indicatorMatrixFun(X[,i])},simplify=FALSE)
  I2.list <-   sapply(1:p,function(l){I.list[[ij.mat[l,1]]]*I.list[[ij.mat[l,2]]]}, simplify = FALSE)
  
  IJ.list <- sapply(1:p, function(l){I <- I2.list[[l]]; I + t(I)}, simplify = FALSE)
  IJ1.vec <- sapply(IJ.list,colSums)
  IJ2.vec <- sapply(IJ.list,rowSums)
  
  I2.mat <- matrix(unlist(I2.list), n^2, p)
  IJ.mat <- matrix(unlist(IJ.list), n^2, p)
  
  ThP <- (4 / (n * (n - 1)))^2 *
    (crossprod(IJ1.vec,IJ2.vec) - crossprod(I2.mat,IJ.mat))
  
  thO <- 2*(2*n - 3) / (n*(n - 1)) *
    outer(tau+1,tau+1,"*")
  
  if(!both) return( list(ShP = n*(ThP - thO)))
  
  K <- length(unique(clus))
  D <- matrix(0,d,K)
  D[cbind(1:d,clus)] <- 1
  DD <- tcrossprod(D)

  Tau.hat <- diag(d)
  Tau.hat[rbind(ij.mat,ij.mat[,2:1])] <- tau
  tt <- ((DD %*% (Tau.hat - diag(d)) %*% DD) / (DD %*% (1 - diag(d)) %*% DD))[ij.mat]
  tt0 <- 2*(2*n - 3) / (n*(n - 1)) *
    outer(tt+1,tt+1,"*")
  
  return(list(ShP = n*(ThP - thO),
              SbP = n*(averageSigmaBlock(ThP,ij.mat,clus) - tt0)))
  
}
