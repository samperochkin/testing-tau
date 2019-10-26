n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p


# computation of estimates ------------------------------------------------
Tau.hat <- cor.fk(X)
t.hat <- Tau.hat[ij.mat]

# components of Hajek projection
T.hajek <- lapply(1:n, function(r){
  v <- t(X[r,] < t(X[-r,]))
  Reduce("+",lapply(1:nrow(v), function(k){
    4*(outer(v[k,],v[k,],"=="))
  }))
})
t.hajek <- sapply(T.hajek, function(tt){
  tt[ij.mat]
})

