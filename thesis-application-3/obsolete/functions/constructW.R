constructW <- function(dend){
  d <- attr(dend,"member")
  
  vec.address2 <- getAddresses(dend)
  W <- matrix(1,d,d)
  
  for(w in vec.address2){
    node <- getSubDend(dend,w)
    ind <- rank(unlist(node))
    W[ind,-ind] <- W[ind,-ind] * length(node)
  }
  W <- W*t(W)
  diag(W) <- 0
  
  return(W)
}
