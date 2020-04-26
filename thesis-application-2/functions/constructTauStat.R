constructTauBar <- function(dend0, vec.address0 = NULL, Tau.hat, only.bar = F){
  d <- ncol(Tau.hat)
  
  if(is.null(vec.address0)) vec.address0 <- getAddresses(dend0)
  W <- matrix(1,d,d)
  
  for(v in vec.address0){
    node <- getSubDend(dend0,v)
    ind <- unlist(node)
    W[ind,-ind] <- W[ind,-ind] * length(node)
  }
  W <- W*t(W)
  
  valz <- numeric(0)
  
  for(v in vec.address0){

    node <- getSubDend(dend0,v)
    if(is.leaf(node)) next

    kk.mat <- t(combn(length(node),2))
    vals <- do.call("rbind", lapply(1:nrow(kk.mat), function(k){
      kk <- kk.mat[k,]
      ind1 <- unlist(node[[kk[1]]])
      ind2 <- unlist(node[[kk[2]]])
      
      if(length(ind1) + length(ind2) == 2) return(NULL)
      
      cbind(c(Tau.hat[ind1,ind2]),sum(Tau.hat[ind1,ind2]*W[ind1,ind2])/sum(W[ind1,ind2]))
    }))
    
    if(is.null(vals)) next
    
    if(attr(node,"delta") == 0){
      valz <- rbind(valz,vals)
    }else if(attr(node,"delta") == 1){
      vals[,2] <- mean(unique(vals[,2])) 
      valz <- rbind(valz,vals)
    }else{
      # break
      print(paste0("This is a warning : invalid delta for node v = ", v))
    }
  }
  
  if(only.bar) valz <- valz[,2]

  return(valz)
}
