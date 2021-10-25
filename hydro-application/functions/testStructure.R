testStructure <- function(Y,B,ij.mat,R){
  
  n <- nrow(Y)
  d <- ncol(Y)
  p <- choose(d,2)
  
  Th <- pcaPP::cor.fk(Y)
  th <- Th[ij.mat]
  
  th.hajek <- sapply(1:n, function(r){
    V <- t(Y[r,] < t(Y[-r,]))
    V <- Reduce("+",lapply(1:nrow(V), function(k){
      4*(outer(V[k,],V[k,],"=="))
    }))
    V[ij.mat]
  })
  
  G <- B %*% MASS::ginv(B)
  tt <- G %*% th
  
  euc <- c(n*crossprod(th - tt))

  sup <- sqrt(n)*max(abs(th - tt))

  tc <- c(th - tt)
  tc.hajek <- th.hajek - G %*% th.hajek

  BOOT <- replicate(R, {
    ss <- (tc.hajek/(n*(n-1)) - tc/n) %*% rnorm(n)

    c(
      n*crossprod(ss),
      sqrt(n)*max(abs(ss))
    )
  })
  
  c(
    euc_boot = mean(BOOT[1,] > euc),
    sup_boot = mean(BOOT[2,] > sup)
  )
}
