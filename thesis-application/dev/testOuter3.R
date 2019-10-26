testOuter <- function(Tau.hat, child, M = 2000){
  
  if(is.leaf(child)) return(1)
  
  ind.list <- lapply(child, get_leaves_attr, "label")
  ind <- unlist(ind.list)
  
  Th <- as.matrix(sapply(ind.list, function(ii){
    # rowMeans(Tau.hat[-ind,ii,drop=F])
    rowMeans(Tau.hat[-ii,ii,drop=F])
  }))
  Tb <- rowMeans(Th)
  
  t.diff <- c(Th - Tb)
  
  t.diffs <- sapply(T.hajek, function(T.haj){
    Th <- as.matrix(sapply(ind.list, function(ii){
      rowMeans(T.haj[-ind,ii,drop=F])
    }))
    Tb <- rowMeans(Th)
    c(Th - Tb)
  })
  
  BOOTsup <- replicate(M, {
    sqrt(n)*max(abs((t.diffs/(n*(n-1)) - t.diff/n) %*% rnorm(n)))
  })
  
  mean(BOOTsup > sqrt(n)*max(abs(t.diff)))
}
