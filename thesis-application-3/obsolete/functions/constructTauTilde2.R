constructTauTilde2 <- function(dend, Tau.hat, return.single.values = F){
  d <- ncol(Tau.hat)
  
  vec.address <- getAddresses(dend)
  W <- matrix(1,d,d)
  
  for(v in vec.address){
    node <- getSubDend(dend,v)
    ind <- unlist(node)
    W[ind,-ind] <- W[ind,-ind] * length(node)
  }
  W <- W*t(W)
  
  
  Tau.tilde <- matrix(0,d,d)
  valz <- numeric(0)
  
  for(v in vec.address){
    
    node <- getSubDend(dend,v)
    if(is.leaf(node)){
      valz <- c(valz,1)
      next
    } 
    
    
    kk.mat <- t(combn(length(node),2))
    vals <- apply(kk.mat, 1, function(kk){
      ind1 <- unlist(node[[kk[1]]])
      ind2 <- unlist(node[[kk[2]]])
      
      sum(Tau.hat[ind1,ind2]*W[ind1,ind2])/sum(W[ind1,ind2])
    })
    
    if(identical(attr(node,"delta"),1)) vals <- rep(mean(vals),length(vals))
    if(return.single.values) valz <- c(valz,mean(vals))
    
    for(k in 1:nrow(kk.mat)){
      kk <- kk.mat[k,]
      
      ind1 <- unlist(node[[kk[1]]])
      ind2 <- unlist(node[[kk[2]]])
      
      Tau.tilde[ind1,ind2] <- vals[k]
    }
  }
  Tau.tilde <- Tau.tilde + t(Tau.tilde) + diag(d)
  
  if(return.single.values) return(valz)
  return(Tau.tilde)
}
