testOuter2 <- function(Tau.hat,B,i.ind,l.ind){
  
  d2 <- nrow(B)
  BB <- tcrossprod(B)
  
  ind <- which(BB[t(combn(d2,2))] != 1)
  if(length(ind) == 0) ind <- 1:length(l.ind)
  l.ij.mat <- t(combn(d2,2))[ind,]
  
  tau.bar <- mean(Tau.hat[i.ind,i.ind][l.ij.mat])
  t.diff <- Tau.hat[i.ind,i.ind][l.ij.mat] - tau.bar

  tbs <- sapply(T.hajek, function(T.haj){
    mean(T.haj[i.ind,i.ind][l.ij.mat])
  })
  
  BOOTsup <- replicate(M, {
    #sqrt(n)*max(abs(rnorm(n) %*% (t(t.hajek[l.ind[ind],]) - tbs)/(n*(n-1)) - t.diff/(n*(n-1))))
    sqrt(n)*max(abs(rnorm(n) %*% (t(t.hajek[l.ind[ind],]) - tbs)))/(n*(n-1))
  })
  
  mean(BOOTsup >= sqrt(n)*max(abs(t.diff)))
}
