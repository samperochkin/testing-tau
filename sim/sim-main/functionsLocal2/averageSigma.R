# JUST FOR B = rep(1,p) FOR NOW
averageSigma <- function(S, l.mat, full=F){
  
  d <- ncol(l.mat)
  p <- choose(d,2)

  B <- Matrix::Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[l.mat[i,-i],i] <- 1
  }
  BtB <- Matrix::tcrossprod(B)
  
  if(!full) return(sapply(0:2, function(k) mean(S[Matrix::which(BtB == k)])))
  
  for(k in 0:2){
    S[Matrix::which(BtB == k)] <- mean(S[Matrix::which(BtB == k)])
  }
  return(S)
}
