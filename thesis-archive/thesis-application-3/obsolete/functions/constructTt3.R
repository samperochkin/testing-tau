ConstructTt3 <- function(node,W,Tau.hat){
  
  kk.mat <- t(combn(length(node),2))
  
  matrix(apply(kk.mat, 1, function(kk){
    G1 <- unlist(node[kk[1]])
    G2 <- unlist(node[kk[2]])
    sum(Tau.hat[G1,G2]*W[G1,G2])/sum(W[G1,G2])
  }), length(dend.comp), length(node))
}
