computePartialSt <- function(X, tau.hat, parallel = FALSE, only = ncol(X)){
  d <- dim(X)[2]
  p <- d * (d - 1) / 2
  n <- dim(X)[1]
  
  l.ij.mat <- t(combn(d,2))
  
  indicatorMatrixFun <- function(Y){
    M1 <- matrix(Y, n, n, byrow = TRUE)
    M2 <- matrix(Y, n, n)
    1 * (M1 < M2)
  }
  
  # tt <- Sys.time()
  # # I.list <- sapply(1:d,function(j){indicatorMatrixFun(X[,j])},simplify=FALSE)
  # print(difftime(Sys.time(),tt))
  ind.list <- lapply(1:only, function(i){
    ind <- which(rowSums(l.ij.mat == i) != 0)
  })
  
  
  if(parallel=="parallel"){
    tt <- Sys.time()
    clus <- makeCluster(detectCores()-1)
    # clusterExport(clus, c("X", "l.ij.mat","n","d"), envir = environment())
    # #clusterExport(clus, c("l.ij.mat","n","d","I.list"), envir = environment())
    dtt <- difftime(Sys.time(),tt)

    
    mat.list <- parLapply(clus, ind.list, function(ind){
    # mat.list <- lapply(ind.list, function(ind){
      indicatorMatrixFun <- function(Y){
        M1 <- matrix(Y, n, n, byrow = TRUE)
        M2 <- matrix(Y, n, n)
        1 * (M1 < M2)
      }
      I.list <- sapply(1:d,function(j){indicatorMatrixFun(X[,j])},simplify=FALSE)
      I2.list <- lapply(ind,function(l){i <- l.ij.mat[l,1]; j <- l.ij.mat[l,2]; I.list[[i]]*I.list[[j]]})
      IJ.list <- sapply(1:length(I2.list), function(l){I2.list[[l]] + t(I2.list[[l]])}, simplify = FALSE)
      I2.mat <- matrix(unlist(I2.list), n^2, length(ind))
      IJ.mat <- matrix(unlist(IJ.list), n^2, length(ind))
      
      (4 / (n * (n - 1)))^2 * (crossprod(sapply(1:length(ind), function(l){colSums(IJ.list[[l]])}),
                                         sapply(1:length(ind), function(l){rowSums(IJ.list[[l]])})) - crossprod(I2.mat,IJ.mat))
    })
    mat <- Reduce("+",mat.list)/length(mat.list)
    
    stopCluster(clus)
    #print(difftime(Sys.time(),tt))
    print(c(dtt, difftime(Sys.time(),tt)))
  }else if(parallel=="nothing"){
    tt <- Sys.time()
    
    I.list <- sapply(1:d,function(j){indicatorMatrixFun(X[,j])},simplify=FALSE)
    I2.list <- sapply(1:p,function(l){i <- l.ij.mat[l,1]; j <- l.ij.mat[l,2]; I.list[[i]]*I.list[[j]]}, simplify = FALSE)
    
    IJ.list <- sapply(1:p, function(l){I <- I2.list[[l]]; I + t(I)}, simplify = FALSE)
    IJ1.vec <- sapply(1:p, function(l){colSums(IJ.list[[l]])})
    IJ2.vec <- sapply(1:p, function(l){rowSums(IJ.list[[l]])})
    
    I2.mat <- matrix(unlist(I2.list), n^2, p)
    IJ.mat <- matrix(unlist(IJ.list), n^2, p)
    
    dtt <- difftime(Sys.time(),tt)
    mat.list <- lapply(ind.list, function(ind){
      (4 / (n * (n - 1)))^2 * (crossprod(IJ1.vec[,ind],IJ2.vec[,ind]) - crossprod(I2.mat[,ind],IJ.mat[,ind]))
    })
    #print(difftime(Sys.time(),tt))
    # print(c(dtt, difftime(Sys.time(),tt)))
    mat <- Reduce("+",mat.list)/length(mat.list)
  }else if(parallel=="loop"){
    tt <- Sys.time()
    
    # I.list <- sapply(1:d,function(j){indicatorMatrixFun(X[,j])},simplify=FALSE)
    I2.list <- sapply(1:p,function(l){i <- l.ij.mat[l,1]; j <- l.ij.mat[l,2]; I.list[[i]]*I.list[[j]]}, simplify = FALSE)
    
    IJ.list <- sapply(1:p, function(l){I <- I2.list[[l]]; I + t(I)}, simplify = FALSE)
    IJ1.vec <- sapply(1:p, function(l){colSums(IJ.list[[l]])})
    IJ2.vec <- sapply(1:p, function(l){rowSums(IJ.list[[l]])})
    
    I2.mat <- matrix(unlist(I2.list), n^2, p)
    IJ.mat <- matrix(unlist(IJ.list), n^2, p)
    
    mat <- 0
      
    for(ind in ind.list){
      mat <- mat + (4 / (n * (n - 1)))^2 * (crossprod(IJ1.vec[,ind],IJ2.vec[,ind]) - crossprod(I2.mat[,ind],IJ.mat[,ind]))
    }
    mat <- mat/length(ind.list)
    print(difftime(Sys.time(),tt))
  }

  diag(mat) <- mean(diag(mat))
  offdiag <- t(combn(nrow(mat),2))
  mat[rbind(offdiag,offdiag[,2:1])] <- mean(mat[offdiag])
  mat - 2*(2*n - 3) / (n*(n - 1)) * (mean(tau.hat)+1)^2
}