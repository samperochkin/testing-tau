jackknifeVariance <- function(X){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  
  Tau.hat <- pcaPP::cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  
  ij.mat <- t(combn(d,2))
  
  Ths <- lapply(1:n, function(r){
    Y <- X[r,] < t(X[-r,])
    Th <- 2*Reduce("+",lapply(1:ncol(Y), function(r) (outer(Y[,r],Y[,r],"-"))==0)) - (n-1)
    (Tau.hat*(n*(n-1))/2 - Th)/((n-1)*(n-2)/2) 
  })

  Tcs <- sapply(Ths, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(Ths, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  s2 <- mean(apply(Ths, 1, var))*(n-1)^2
  s1 <- mean(apply(Tcs, 1, var))*(n-1)^2
  s0 <- var(Tbs)*(n-1)^2
  
  c(s2,s1,s0)
}
