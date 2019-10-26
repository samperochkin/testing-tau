testOuter <- function(node, Tau.hat, dend){
  
  if(is.leaf(node)) return(1)
  
  ind.list <- lapply(node, get_leaves_attr, attribute = "label")
  ind <- unlist(ind.list)
    
  ids <- (1:d)[-ind]
  
  vec.address <- getAddresses(dend)
  valids <- lapply(rev(vec.address[-1]), function(v){
    node <- dend[[v[-1]]]
    if(identical(attr(node,"valid"),F)) unlist(node)
  })
  valids <- valids[!sapply(valids, is.null)]

  Th <- Tau.hat[-ind,ind]
  ind2 <- ind
  
  for(ii in valids){
    if(any(ii %in% ind)){
      kk <- which(ii %in% ind2)
      Th[,kk[1]] <- rowMeans(Th[,kk])
      Th <- Th[,-kk[-1],drop=F]
      ids <- ind2[-kk[-1]]  
    }else{
      kk <- which(ii %in% ids)
      Th[kk[1],] <- colMeans(Th[kk,,drop=F])
      Th <- Th[-kk[-1],,drop=F]
      ids <- ids[-kk[-1]]  
    }
  }
  
  t.diff <- c(Th - rowMeans(Th))
  
  t.diffs <- sapply(T.hajek, function(Th){
    ids <- (1:d)[-ind]
    Th <- Th[-ind,ind]
    ind2 <- ind
    for(ii in valids){
      if(any(ii %in% ind)){
        kk <- which(ii %in% ind2)
        Th[,kk[1]] <- rowMeans(Th[,kk,drop=F])
        Th <- Th[,-kk[-1],drop=F]
        ids <- ind2[-kk[-1]]  
      }else{
        kk <- which(ii %in% ids)
        Th[kk[1],] <- colMeans(Th[kk,,drop=F])
        Th <- Th[-kk[-1],,drop=F]
        ids <- ids[-kk[-1]]  
      }
    }
    c(Th - rowMeans(Th))
  })
  
  BOOTsup <- replicate(M, {
    sqrt(n)*max(abs((t.diffs/(n*(n-1)) - t.diff/n) %*% rnorm(n)))
  })
  
  mean(BOOTsup > sqrt(n)*max(abs(t.diff)))
}
