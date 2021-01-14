res <- replicate(50, {
  print("hey")
  
  n <- 100
  d <- 15
  tau <- .15
  dtau <- 0
  dtau_type <- "none"
  distribution <- "normal"
  
  X <- generateData(n,d,tau,dtau,dtau_type,distribution)
  
  Tau.hat <- cor.fk(X)
  ij.mat <- t(combn(d,2))
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  t.star <- (d-1)/(d-2)*(t.col[ij.mat[,1]] + t.col[ij.mat[,2]]) - d/(d-2)*t.bar
  p <- d*(d-1)/2
  
  
  
  
  #1
  Ths <- lapply(1:n, function(r){
    Y <- X[r,] < t(X[-r,])
    Th <- 2*Reduce("+",lapply(1:ncol(Y), function(r) (outer(Y[,r],Y[,r],"-"))==0)) - (n-1)
    (Tau.hat*(n*(n-1))/2 - Th)/((n-1)*(n-2)/2) 
  })
  
  Tcs <- sapply(Ths, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(Ths, function(Th) Th[t(combn(d,2))])
  Tbs <- apply(Ths, 2, mean)
  
  
  s2 <- mean(apply(Ths, 1, var))*(n-1)^2
  s1 <- mean(apply(Tcs, 1, var))*(n-1)^2
  s0 <- var(Tbs)*(n-1)^2
  
  sigma1 <- c(s2,s1,s0)
  
  
  
  #2
  print("ho")
  T.hajek <- lapply(1:n, function(r){
    v <- t(X[r,] < t(X[-r,]))
    v <- Reduce("+", lapply(1:nrow(v), function(k){
      4*(outer(v[k,],v[k,],"=="))
    }))
  })
  
  Tcs <- sapply(T.hajek, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(T.hajek, function(Th) Th[t(combn(d,2))])
  Tbs <- apply(Ths, 2, mean)
  
  sigma2 <- c(
    mean(apply(Ths, 1, var))/(n*(n-1)),
    mean(apply(Tcs, 1, var))/(n*(n-1)),
    var(Tbs)/(n*(n-1))  
  )
  
  delta1 <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma1
  delta2 <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma2
  

    
  maha_p <- c(n*crossprod(t.hat-t.star))/delta1[1]
  maha_d <- (d-1)^2/(d-2)*c(n*crossprod(t.col-t.bar))/delta1[2]
  maha <- maha_d + maha_p
  pc1 <- 1-pchisq(maha,p-1)
  
  maha_p <- c(n*crossprod(t.hat-t.star))/delta2[1]
  maha_d <- (d-1)^2/(d-2)*c(n*crossprod(t.col-t.bar))/delta2[2]
  maha <- maha_d + maha_p
  pc2 <- 1-pchisq(maha,p-1)
  
  
  
  sigma1 <- c(sigma1 %*% matrix(c(1,0,0,
                                  -1/(d-2),(d-1)/(d-2),0,
                                  1/(p-2*d+3),-2*(d-1)/(p-2*d+3),p/(p-2*d+3)),3,3))
  sigma2 <- c(sigma2 %*% matrix(c(1,0,0,
                                  -1/(d-2),(d-1)/(d-2),0,
                                  1/(p-2*d+3),-2*(d-1)/(p-2*d+3),p/(p-2*d+3)),3,3))
  
  delta1 <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma1
  delta2 <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma2
  
  maha_p <- c(n*crossprod(t.hat-t.star))/delta1[1]
  maha_d <- (d-1)^2/(d-2)*c(n*crossprod(t.col-t.bar))/delta1[2]
  maha <- maha_d + maha_p
  pc3 <- 1-pchisq(maha,p-1)
  
  maha_p <- c(n*crossprod(t.hat-t.star))/delta2[1]
  maha_d <- (d-1)^2/(d-2)*c(n*crossprod(t.col-t.bar))/delta2[2]
  maha <- maha_d + maha_p
  pc4 <- 1-pchisq(maha,p-1)
  

  
  
    
  c(pc1,pc2,pc3,pc4)
})


plot(seq(0,1,length.out=ncol(res)),sort(res[1,]),type="l", col="blue")
lines(seq(0,1,length.out=ncol(res)),sort(res[2,]), col="orange")
lines(seq(0,1,length.out=ncol(res)),sort(res[3,]), col="green")
lines(seq(0,1,length.out=ncol(res)),sort(res[4,]), col="red")
abline(a=0,b=1)

rowMeans(res < .05)
