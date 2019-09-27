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
    S <- computeThChunks(X,Gs[,i],Gs[,i])
    
    c(sum(diag(S)),sum(S[which(BtB == 1)]),sum(S[which(BtB == 0)]),
      nrow(S), sum(BtB == 1), sum(BtB == 0))
  })
  
  
  if(dd != d){
    ij.mat <- as.matrix(expand.grid(1:dd,1:dd))
    r.mat <- matrix(1:nrow(ij.mat),dd,dd)
    B <- Matrix(0, nrow = nrow(ij.mat), ncol = 2*dd, sparse = T)
    for(i in 1:dd){
      B[r.mat[i,],i] <- 1
      B[r.mat[,i],i+dd] <- 1
    }
    BtB <- B %*% t(B)
    
    sss <- apply(t(combn(K,2)), 1, function(ij){
      # print(ij)
      S <- computeThChunks(X,Gs[,ij[1]],Gs[,ij[2]])
      
      c(sum(diag(S)),sum(S[which(BtB == 1)]),sum(S[which(BtB == 0)]),
        nrow(S), sum(BtB == 1), sum(BtB == 0))
    })
    
  }else{
    sss <- matrix(0,6,1)
  }  

  
  rss <- rowSums(ss)
  rsss <- rowSums(sss)
  
  (rss[1:3]+rsss[1:3])/(rss[4:6]+rsss[4:6])
} 


  