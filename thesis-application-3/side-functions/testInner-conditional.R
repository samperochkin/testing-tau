testInner <- function(node, k, Tau.hat, Tau.hajek, M = 2000){
  
  n <- length(Tau.hajek)
  node.star <- unbranch(node,k)
  attr(node.star, "delta") <- 1
  
  W0 <- constructW(node.star)
  Tt0 <- ConstructTt2(node.star,W0,Tau.hat)
  W1 <- constructW(node)
  Tt1 <- ConstructTt2(node,W1,Tau.hat,k)
  tt0 <- Tt0 - Tt1
  
  Tt.hajek0 <- lapply(Tau.hajek, ConstructTt2, node=node,W=W0)
  Tt.hajek1 <- lapply(Tau.hajek, ConstructTt2, node=node,W=W1,k=k)
  tt.hajek <- sapply(seq_along(Tt.hajek0), function(s){
    (Tt.hajek0[[s]] - Tt.hajek1[[s]])/(n*(n-1)) - tt0/n
  })
  
  boot <- replicate(M, {
    sqrt(n)*max(abs(tt.hajek %*% rnorm(n)))
  })
  
  mean(boot > sqrt(n)*max(abs(tt0)))
}
