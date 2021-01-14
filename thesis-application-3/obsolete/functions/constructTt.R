ConstructTt <- function(node,dend.comp,W,Tau.hat){
  
  kk.mat <- as.matrix(expand.grid(seq_along(dend.comp),seq_along(node)))
  
  matrix(apply(kk.mat, 1, function(kk){
    G1 <- unlist(dend.comp[kk[1]])
    G2 <- unlist(node[kk[2]])
    sum(Tau.hat[G1,G2]*W[G1,G2])/sum(W[G1,G2])
  }), length(dend.comp), length(node))
}
