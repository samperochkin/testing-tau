testOuter <- function(Tau.hat,B,i.ind,l.ind){
  
  d2 <- nrow(B)
  BB <- tcrossprod(B)
  
  ind <- which(BB[t(combn(d2,2))] != 1)
  if(length(ind) == 0) ind <- 1:nrow(l.ij.mat)
  l.ij.mat <- t(combn(d2,2))[ind,]
  
  tau.bar <- ((BB %*% (Tau.hat[i.ind,i.ind] - diag(d2)) %*% t(BB)) / (BB %*% (1 - diag(d2)) %*% t(BB)))[l.ij.mat]
  t.diff <- Tau.hat[i.ind,i.ind][l.ij.mat] - tau.bar
  
  tbs <- sapply(T.hajek, function(T.haj){
    ((BB %*% (T.haj[i.ind,i.ind] - diag(diag(T.haj[i.ind,i.ind]))) %*% t(BB)) / (BB %*% (1 - diag(d2)) %*% t(BB)))[l.ij.mat]
  })

  BOOTsup <- replicate(M, {
    sqrt(n)*max(abs(((t.hajek[l.ind[ind],] - tbs)/(n*(n-1))) %*% rnorm(n)))
  })
  

  mean(BOOTsup >= sqrt(n)*max(abs(t.diff)))
}
