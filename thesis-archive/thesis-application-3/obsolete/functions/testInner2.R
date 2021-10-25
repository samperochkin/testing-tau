testInner2 <- function(dend, node, v, k, Tau.hat, Tau.hajek, M = 5000){
  
  
  ### MESSS
  n <- length(Tau.hajek)

  node.temp <- unbranch(node,k)
  W <- constructW(node.temp)
  Tt1 <- ConstructTt3(node.temp,W,Tau.hat)

  t.diff <- Tt1 - mean(Tt1)
  
  lapply(Tau.hajek, ConstructTt3, node=node.temp,W)
  Tt1 <- ConstructTt3(node,W,Tau.hat)
  
  t.diffs <- sapply(Tau.hajek, function(T.haj){
    tbs <- apply(kks,1,function(kk) mean(T.haj[inds[[kk[1]]],inds[[kk[2]]]]))
    tbs - mean(tbs)
  })
  
  
  
  Tt0 <- ConstructTt(node.temp,node.temp,W,Tau.hat[])
  tt0 <- c(Tt0 - rowMeans(Tt0))
  
  
  Tt.hajek <- lapply(Tau.hajek, ConstructTt, node=node,dend.comp=dend.comp,W=W)
  tt.hajek <- sapply(Tt.hajek, function(Tt){
    c(Tt - rowMeans(Tt))/(n*(n-1)) - tt0/n
  })
  
  boot <- replicate(M, {
    sqrt(n)*max(abs(tt.hajek %*% rnorm(n)))
  })
  
  mean(boot > sqrt(n)*max(abs(tt0)))
  
  
  
  n <- length(Tau.hajek)
  
  node.temp <- unbranch(node,k)
  inds <- lapply(node.temp, get_leaves_attr, attribute="label")
  
  kks <- t(combn(length(inds),2))
  tbs <- apply(kks,1,function(kk) mean(Tau.hat[inds[[kk[1]]],inds[[kk[2]]]]))
  t.diff <- tbs - mean(tbs)
  
  t.diffs <- sapply(Tau.hajek, function(T.haj){
    tbs <- apply(kks,1,function(kk) mean(T.haj[inds[[kk[1]]],inds[[kk[2]]]]))
    tbs - mean(tbs)
  })
  
  BOOTsup <- replicate(M, {
    sqrt(n)*max(abs((t.diffs/(n*(n-1)) - t.diff/n) %*% rnorm(n)))
  })
  
  mean(BOOTsup > sqrt(n)*max(abs(t.diff)))
}
