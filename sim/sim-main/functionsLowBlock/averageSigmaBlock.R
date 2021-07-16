averageSigmaBlock <- function(S, l.ij.mat, clus){
  
  d <- length(clus)
  p <- choose(d,2)
  
  m.lr.mat <- t(mapply(l = unlist(sapply(1:p, function(l){rep(l, p - l + 1)})), r = unlist(sapply(1:p, function(r){r:p})), function(l,r){c(l,r)}))
  
  lr.m.mat <- matrix(0,p,p)
  lr.m.mat[m.lr.mat] <- 1:dim(m.lr.mat)[1]
  lr.m.mat <- lr.m.mat + t(lr.m.mat)  - diag(diag(lr.m.mat))
  
  m.shared.mat <- t(sapply(1:dim(m.lr.mat)[1], function(m){
    shared <- intersect(l.ij.mat[m.lr.mat[m, 1],],l.ij.mat[m.lr.mat[m, 2],])
    if(length(shared) == 0){
      return(c(0,0))
    }else if(length(shared) == 1){
      return(c(0,shared))
    }else{
      shared <- sort(shared)
      return(shared)
    }
  }))
  
  K <- length(unique(clus))
  D <- matrix(0,d,K)
  D[cbind(1:d,clus)] <- 1
  # D <- tcrossprod(D)
  
  computeTt(D, S, m.lr.mat, lr.m.mat, m.shared.mat, l.ij.mat)
}
