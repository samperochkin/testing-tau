computeTh3 <- function(X,dd,K){
  
  d <- ncol(X)
  Gs <- replicate(K, sample(d,dd))

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
  
  rss <- rowSums(ss)
  rss[1:3]/rss[4:6]
} 


  