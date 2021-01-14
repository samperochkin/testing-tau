testInner <- function(node, k, Tau.hat, Tau.hajek, M = 5000){
  
  if(length(v) == 1) return(1)
  n <- length(Tau.hajek)
  
  node.star <- unbranch(node,k)
  T.dag <- getTauDag(node.star,Tau.hat)
  
  rs <- t(combn(length(node.star),2))
  
  tt0 <- c(T.dag[rs])
  tt0 <- tt0 - mean(tt0)
  
  tts <- lapply(Tau.hajek, function(Th) getTauDag(node.star,Th)[rs])
  tt.hajek <- sapply(tts, function(tt){
    (tt - mean(tt))/(n*(n-1)) - tt0/n
  })
  
  boot <- replicate(M, {
    sqrt(n)*max(abs(tt.hajek %*% rnorm(n)))
  })
  
  mean(boot > sqrt(n)*max(abs(tt0)))
}
