getTauDag <- function(node, Tau.hat){
  
  adz <- getAddresses(node)
  ids <- as.numeric(labels(node))
  T0 <- Tau.hat
  
  count <- 0
  for(v in rev(adz[-1])){
    node2 <- getSubDend(node,v)
    if(is.leaf(node2)) next
    if(identical(v,0)) break
    else{
      Gs <- lapply(node2, function(nn) as.numeric(labels(nn)))
      tt <- rowMeans(sapply(Gs, function(G) T0[,G[1]]))
      T0[,unlist(Gs)] <- tt
      T0[unlist(Gs),] <- rep(tt,each=length(unlist(Gs)))
    }
  }
  Gs <- sapply(node, function(nn) as.numeric(labels(nn))[1])
  T0[Gs,Gs]
}
