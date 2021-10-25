testStructure <- function(Y,B,ij.mat){
  
  n <- nrow(Y)
  d <- ncol(Y)
  p <- choose(d,2)
  
  Th <- pcaPP::cor.fk(Y)
  
  
  keep <- which(rowSums(BBp > 0) > 1) # kely relevant rows
  ij.mat <- ij.mat[keep,]
  p <- nrow(ij.mat) # re-define p
  B <- B[keep,]
  B <- B[,colSums(B) > 0]
  
  th <- Th[ij.mat]
  
  th.hajek <- sapply(1:n, function(r){
    V <- t(Y[r,] < t(Y[-r,]))
    V <- Reduce("+",lapply(1:nrow(V), function(k){
      4*(outer(V[k,],V[k,],"=="))
    }))
    V[ij.mat]
  })
  
  ShJ <- var(t(th.hajek))/(n*(n-1))
  # ShJ <- 4*tcrossprod(th.hajek)/(n*(n-1))^2
  ShJi <- MASS::ginv(ShJ)
  ShJi2 <- MASS::ginv(Re(expm::sqrtm(ShJ)))
  # eigJ <- eigen(ShJ)
  
  # if(!matrixcalc::is.positive.definite(ShJ)){
  #   print('The Jackknife estimate of Sigma is not positive-definite, Matrix::nearPD was used')
  #   ShJ <- as.matrix(Matrix::nearPD(ShJ)$mat)
  #   # ShJ <- eigJ$vec %*% diag(eigJ$val*(eigJ$val > .001) + .001*(eigJ$val < .001)) %*% t(eigJ$vec)
  #   eigJ <- eigen(ShJ)
  # }
  
  # ShJ2 <- eigJ$vec %*% sqrt(diag(eigJ$val)) %*% t(eigJ$vec)
  # ShJ2 <- eigJ$vec %*% sqrt(diag(eigJ$val*(eigJ$val>0))) %*% t(eigJ$vec)
  
  G1 <- B %*% MASS::ginv(B)
  # G2 <- (ShJ2 %*% B) %*% MASS::ginv(ShJ2 %*% B)
  G2 <- B %*% solve(t(B) %*% ShJi %*% B) %*% t(B) %*% ShJi
  
  tt1 <- G1 %*% th
  tt2 <- G2 %*% th
  
  euc1 <- c(n*crossprod(th - tt1))
  # euc2 <- c(n*crossprod(th - tt2))
  euc2 <- n*mahalanobis(th,tt2,ShJi,T)
  
  sup1 <- sqrt(n)*max(abs(th - tt1))
  # sup2 <- sqrt(n)*max(abs(th - tt2))
  sup2 <- sqrt(n)*max(abs(ShJi2 %*% (th - tt2)))
  
  tc1 <- c(th - tt1)
  tc1.hajek <- th.hajek - G1 %*% th.hajek
  
  tc2 <- c(th - tt2)
  tc2.hajek <- th.hajek - G2 %*% th.hajek
  
  M <- 5000
  BOOT <- replicate(M, {
    ss1 <- (tc1.hajek/(n*(n-1)) - tc1/n) %*% rnorm(n)
    ss2 <- (tc2.hajek/(n*(n-1)) - tc2/n) %*% rnorm(n)
    
    c(
      n*crossprod(ss1),
      sqrt(n)*max(abs(ss1))
      # ,
      # n*crossprod(ss2),
      # sqrt(n)*max(abs(ss2))
      ,
      n*mahalanobis(c(ss2),F,ShJi,T)
      ,
      sqrt(n)*max(abs(ShJi2 %*% ss2))
    )
  })
  
  c(
    euc_boot = mean(BOOT[1,] > euc1),
    sup_boot = mean(BOOT[2,] > sup1)
    ,
    euc2_boot = mean(BOOT[3,] > euc2)
    ,
    sup2_boot = mean(BOOT[4,] > sup2)
  )
}
