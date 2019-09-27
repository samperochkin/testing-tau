computeTh2 <- function(X,dd){
  
  d <- ncol(X)
  Gs <- matrix(1:d,dd)
  K <- ncol(Gs)
  
  ij.mat <- t(combn(dd,2))
  r.mat <- matrix(0,dd,dd)
  r.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:nrow(ij.mat)
  B <- Matrix(0, nrow = nrow(ij.mat), ncol = dd, sparse = T)
  for(i in 1:dd){
    B[r.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  
  ss <- sapply(1:K, function(i){
    S <- computeTh(X[,Gs[,i]])
    c(sum(diag(S)),sum(S[which(BtB == 1)]),sum(S[which(BtB == 0)]),
      nrow(S), sum(BtB == 1), sum(BtB == 0))
  })
  
  
  ij.mat <- t(combn(2*dd,2))
  r.mat <- matrix(0,2*dd,2*dd)
  r.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:nrow(ij.mat)
  B <- Matrix(0, nrow = nrow(ij.mat), ncol = 2*dd, sparse = T)
  for(i in 1:(2*dd)){
    B[r.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  
  sss <- apply(t(combn(K,2)), 1, function(ij){
    # print(ij)
    S <- computeTh(X[,c(Gs[,ij[1]],Gs[,ij[2]])])
    
    c(sum(diag(S)),sum(S[which(BtB == 1)]),sum(S[which(BtB == 0)]),
      nrow(S), sum(BtB == 1), sum(BtB == 0)) -
      (ss[,ij[1]] + ss[,ij[2]])
  })
  
  rss <- rowSums(ss)
  rsss <- rowSums(sss)
  
  (rss[1:3]+rsss[1:3])/(rss[4:6]+rsss[4:6])
} 


  