performTestsLowBoot <- function(X, M){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))

  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  tau.bar <- mean(tau.hat)
  
  Ths <- computeHajekProjection(X,ij.mat)
  Tbs <- colMeans(Ths)
  
  TT <- (Ths - matrix(Tbs,p,n,byrow=T))/sqrt(n-1) - (tau.hat-tau.bar)/sqrt(n)
  boot <- replicate(M, {
    zz <- (TT %*% rnorm(n))
    
    c(euc=crossprod(zz),
      supI=max(abs(zz)))
  })
  
  lo <- c(n*crossprod(tau.hat-tau.bar),sqrt(n)*max(abs(tau.hat-tau.bar)))
  pv <- c(mean(boot[1,] > lo[1]),
          mean(boot[2,] > lo[2]))
    
  data.table(S = rep("I",2),
             Sh = rep("ShJ",2),
             norm = c("Euclidean","Supremum"),
             loss = lo,
             pvalue_method = rep("bootstrap",2),
             pvalue = pv,
             isShPd = rep(NA,2))
  
}


