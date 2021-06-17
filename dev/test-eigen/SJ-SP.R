computeSJ <- function(X){
  
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  
  Tau.hat <- cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  Tn <- sapply(1:n, function(r){
    H1 <- t(X[r,] < t(X[-r,]))
    H1 <- Reduce("+",lapply(1:nrow(H1), function(k){
      4*(outer(H1[k,],H1[k,],"=="))[ij.mat]
    }))
  })
  
  Sj <- cov(t(Tn))/(n*(n-1))
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  sj <- rep(NA,3)
  for(i in 1:3){
    sj[i] <- mean(Sj[which(BtB == i-1)])
  }
  
  sj
}




computeSP <- function(X){
  
  d <- ncol(X)
  p <- d*(d-1)/2
  n <- nrow(X)
  
  ij.mat <- t(combn(d,2))
  
  Sp <- buildSigma(computeTh(X),tau.hat,n,F)
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  sp <- rep(NA,3)
  for(i in 1:3){
    sp[i] <- mean(Sp[which(BtB == i-1)])
  }
  
  sp
}

computeSJ2 <- function(X){
  Tn <- lapply(1:n, function(r){
    H1 <- t(X[r,] < t(X[-r,]))
    H1 <- Reduce("+",lapply(1:nrow(H1), function(k){
      4*(outer(H1[k,],H1[k,],"=="))
    }))
  })
  
  Tcs <- sapply(Tn, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(Tn, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  sigma <- c(
    mean(apply(Ths, 1, var))/(n*(n-1)),
    mean(apply(Tcs, 1, var))/(n*(n-1)),
    var(Tbs)/(n*(n-1))  
  )
  
  rev(c(sigma %*% matrix(c(1,0,0,
                       -1/(d-2),(d-1)/(d-2),0,
                       1/(p-2*d+3),-2*(d-1)/(p-2*d+3),p/(p-2*d+3)),3,3)))
  
}



sj <- computeSJ(X)
sj2 <- computeSJ2(X)
sp <- computeSP(X)

sj
sj2
n*sp

c(1/(p-2*d+3),-2*(d-1)/(p-2*d+3),p/(p-2*d+3))

