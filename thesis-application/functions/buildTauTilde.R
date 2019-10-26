buildTauTilde <- function(dend, Tau.hat){
  
  vec.address <- getAddresses(dend)
  Tau.tilde <- Tau.hat
  
  for(v in vec.address){

    if(length(v) == 1){
      node <- dend
    }else{
      node <- dend[[v[-1]]]
    }
    
    if(is.leaf(node)) next
    
    B <- attr(node,"B")
    type <- attr(node,"type")
    BB <- tcrossprod(B)
    
    Tb <- (BB %*% (Tau.hat - diag(d)) %*% t(BB)) / (BB %*% (1 - diag(d)) %*% t(BB))
    if(type == 1){
      
      Tau.tilde[BB == 0 & !(is.nan(Tb))] <- mean(Tb[BB == 0  & !(is.nan(Tb))])
    }else{
      Tau.tilde[BB == 0 & !(is.nan(Tb))] <- Tb[BB == 0  & !(is.nan(Tb))]
    }
    diag(Tau.tilde) <- 1
  }
  Tau.tilde
}
