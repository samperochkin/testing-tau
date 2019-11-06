tauCalculator <- function(dend, Tau.hat, oorder = F){
  
  d <- ncol(Tau.hat) 
  
  # find all addresses
  vec.address <- getAddresses(dend)
  
  # compute weight matrix
  W <- matrix(1,d,d)
  for(v in vec.address){
    node <- getSubDend(dend,v)
    ind <- unlist(node)
    W[ind,-ind] <- W[ind,-ind] * length(node)
  }
  W <- W*t(W)
  
  
  # loop over all addresses
  for(v in rev(vec.address)){
    
    print(v)
    
    node <- getSubDend(dend,v)
    if(is.leaf(node)) next

    len <- length(node)
    
    kk.mat <- t(combn(len,2))
    vals <- apply(kk.mat, 1, function(kk){
      ind1 <- unlist(node[[kk[1]]])
      ind2 <- unlist(node[[kk[2]]])
      
      sum(Tau.hat[ind1,ind2]*W[ind1,ind2])/sum(W[ind1,ind2])
    })
    

    Tt <- diag(len)
    Tt[t(combn(len,2))] <- Tt[t(combn(len,2))[,2:1]] <- vals
    
    tt <- mean(valz)
    
    if(oorder){
      oo <- hclust(as.dist(1-Tt), "single")$order
      Tt <- Tt[oo,oo]
    }
    
    attr(node, "Tt") <- Tt 
    attr(node, "tt") <- tt 
    
    dend <- assignSubDend(node, dend, v)
    
  }

  return(dend)
}
