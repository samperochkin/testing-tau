testInner <- function(node, k, Tau.hat, Tau.hajek, M = 2000){
  
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
