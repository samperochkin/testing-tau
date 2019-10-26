testOuter <- function(Tau.hat, ind, M = 2000){
  Th <- Tau.hat[-ind,ind,drop=F]
  Tb <- rowMeans(Th)
  t.diff <- c(Th - Tb)
  
  t.diffs <- sapply(T.hajek, function(T.haj){
    Th <- T.haj[-ind,ind,drop=F]
    Tb <- rowMeans(Th)
    c(Th - Tb)
  })
  
  BOOTsup <- replicate(M, {
    sqrt(n)*max(abs((t.diffs/(n*(n-1)) - t.diff/n) %*% rnorm(n)))
  })

  mean(BOOTsup > sqrt(n)*max(abs(t.diff)))
}
