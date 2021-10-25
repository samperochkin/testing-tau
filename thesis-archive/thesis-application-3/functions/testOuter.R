testOuter <- function(dend, v, Tau.hat, Tau.hajek, M = 5000){
  
  if(length(v) == 1) return(1)
  n <- length(Tau.hajek)
  
  node <- getSubDend(dend,v)
  dend.star <- validify(dend)
  T.dag <- getTauDag(dend.star,Tau.hat)
  
  gs <- sapply(dend.star, function(nn){labels(nn)[1]})
  ks <- which(gs %in% labels(node))
  Tt0 <- T.dag[-ks,ks,drop=F]
  
  tt0 <- c(Tt0 - rowMeans(Tt0))

  
  Tts <- lapply(Tau.hajek, function(Th) getTauDag(dend.star,Th)[-ks,ks,drop=F])
  tt.hajek <- sapply(Tts, function(Tt){
    c(Tt - rowMeans(Tt))/(n*(n-1)) - tt0/n
  })
  
  boot <- replicate(M, {
    sqrt(n)*max(abs(tt.hajek %*% rnorm(n)))
  })
  
  mean(boot > sqrt(n)*max(abs(tt0)))
}
