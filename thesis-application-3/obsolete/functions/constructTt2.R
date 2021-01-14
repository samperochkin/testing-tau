ConstructTt2 <- function(node.star,W0,Tau.hat,k=NULL){
  
  mem <- unlist(node.star)
  Th <- Tau.hat[mem,mem]
  
  labels(node.star) <- seq_along(mem)
  kk.mat <- t(combn(seq_along(node.star),2))

  vals <- sapply(1:nrow(kk.mat), function(kk){
    kk <- kk.mat[kk,]
    G1 <- get_leaves_attr(node.star[[kk[1]]],"label")
    G2 <- get_leaves_attr(node.star[[kk[2]]],"label")
    sum(Th[G1,G2]*W0[G1,G2])/sum(W0[G1,G2])
  })
  
  if(attr(node.star,"delta") == 1) vals <- mean(vals)
  
  if(!is.null(k)){
    node.star <- node.star[[k]]
    kk.mat <- t(combn(seq_along(node.star),2))
    vals <- c(vals,sapply(1:nrow(kk.mat), function(kk){
      kk <- kk.mat[kk,]
      G1 <- get_leaves_attr(node.star[[kk[1]]],"label")
      G2 <- get_leaves_attr(node.star[[kk[2]]],"label")
      sum(Th[G1,G2]*W0[G1,G2])/sum(W0[G1,G2])
    }))
  }
    
  return(vals)
}

