testOuter <- function(dend, v, Tau.hat, Tau.hajek, M = 5000){
  
  if(length(v) == 1) return(1)
  n <- length(Tau.hajek)
  
  node <- getSubDend(dend,v)
  dend.star <- validify(dend)
  dend.comp <- prune(dend.star, leaves = as.character(unlist(node)), reindex_dend = F)
  
  W <- constructW(dend.star)
  Tt0 <- ConstructTt(node,dend.comp,W,Tau.hat)
  tt0 <- c(Tt0 - rowMeans(Tt0))

  
  Tt.hajek <- lapply(Tau.hajek, ConstructTt, node=node,dend.comp=dend.comp,W=W)
  tt.hajek <- sapply(Tt.hajek, function(Tt){
    c(Tt - rowMeans(Tt))/(n*(n-1)) - tt0/n
  })
  
  boot <- replicate(M, {
    sqrt(n)*max(abs(tt.hajek %*% rnorm(n)))
  })
  
  mean(boot > sqrt(n)*max(abs(tt0)))
}
