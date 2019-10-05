testEquiRankJackknife <- function(X, M = 2000){
  
  # tiime <- Sys.time()
  # setup -------------------------------------------------------------------
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p
  
  
  # computation of estimates ------------------------------------------------
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  t.star <- (d-1)/(d-2)*(t.col[ij.mat[,1]] + t.col[ij.mat[,2]]) - d/(d-2)*t.bar

  # components of Hajek projection
  T.hajek <- lapply(1:n, function(r){
    v <- t(X[r,] < t(X[-r,]))
    v <- rowSums(apply(v, 1, function(v){
      4*(outer(v,v,"=="))[ij.mat]
    }))
    V <- diag(d)
    V[ij.mat] <- V[ij.mat[,2:1]] <- v #- mean(v)# - t.cen/n
    V
  })
  
  Tcs <- sapply(T.hajek, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(T.hajek, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  sigma <- c(
    mean(apply(Ths, 1, var))/(n*(n-1)),
    mean(apply(Tcs, 1, var))/(n*(n-1)),
    var(Tbs)/(n*(n-1))  
  )
  
  # sigma <- c(sigma %*% matrix(c(1,0,0,
  #                    -1/(d-2),(d-1)/(d-2),0,
  #                    1/(p-2*d+3),-2*(d-1)/(p-2*d+3),p/(p-2*d+3)),3,3))

  delta <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma
  
  euc <- c(n*crossprod(t.hat-t.bar))
  maha_p <- c(n*crossprod(t.hat-t.star))/delta[1]
  maha_d <- (d-1)^2/(d-2)*c(n*crossprod(t.col-t.bar))/delta[2]
  maha <- maha_d + maha_p
  
  sup <- sqrt(n)*max(abs(t.hat-t.bar))
  sup_p <- sqrt(n)*max(abs(t.hat-t.star))
  sup_d <- sqrt(n)*max(abs(t.col-t.bar))
  sups <- sqrt(n)*max(abs((t.hat-t.star) * sqrt(1/delta[1]) + (t.star-t.bar) * sqrt(1/delta[2])))


  #### p-values
  MCeuc <- delta[1]*rchisq(M, p-d) + delta[2]*rchisq(M, d-1)
  
  MCsup <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(sigma[2]-sigma[3]))
    Z2 <- rnorm(p,0,sqrt(delta[1]))
    Z.hat <- Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
    Z.bar <- mean(Z.hat)
    Z.col <- sapply(1:d, function(i) mean(Z.hat[ij.l.mat[,i]]))
    Z.star <- (d-1)/(d-2)*(Z.col[ij.mat[,1]] + Z.col[ij.mat[,2]]) - d/(d-2)*Z.bar
    
    sapply(list(Z.hat - Z.bar,
                Z.hat - Z.star,
                Z.col - Z.bar),function(z) max(abs(z)))
  })
  
  MCsups <- replicate(M, {z <- rnorm(p); max(abs(z-mean(z)))})
  # print(difftime(Sys.time(),tiime))
  
  
  # tiime <- Sys.time()
  # Boot
  BOOTsup <- replicate(M, {
    sqrt(n)*max(abs(rnorm(n) %*% (t(Ths) - Tbs)/(n*(n-1))))
  })
  # print(difftime(Sys.time(),tiime))
  
  pvals <- c(
    maha = pchisq(maha,p-1,lower.tail = F),
    maha_d = pchisq(maha_d,d-1,lower.tail = F),
    maha_p = pchisq(maha_p,p-d,lower.tail = F),
    euc = mean(MCeuc > euc),
    
    sups = mean(MCsups > sups),
    sup_d = mean(MCsup[3,] > sup_d),
    sup_p = mean(MCsup[2,] > sup_p),
    sup = mean(MCsup[1,] > sup),
    
    sup_boot = mean(BOOTsup > sup)
  )
  
  if(sigma[2] < sigma[3] | any(sigma < 0)){
    
    if(sigma[2] < sigma[3]) sigma[2:3] <- mean(sigma[2:3])
    sigma <- pmax(sigma,0)
    delta <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma
    
    euc <- c(n*crossprod(t.hat-t.bar))
    maha_p <- c(n*crossprod(t.hat-t.star))/delta[1]
    maha_d <- (d-1)^2/(d-2)*c(n*crossprod(t.col-t.bar))/delta[2]
    maha <- maha_d + maha_p
    
    sup <- sqrt(n)*max(abs(t.hat-t.bar))
    sup_p <- sqrt(n)*max(abs(t.hat-t.star))
    sup_d <- sqrt(n)*max(abs(t.col-t.bar))
    sups <- sqrt(n)*max(abs((t.hat-t.star) * sqrt(1/delta[1]) + (t.star-t.bar) * sqrt(1/delta[2])))
    
    
    #### p-values
    MCeuc <- delta[1]*rchisq(M, p-d) + delta[2]*rchisq(M, d-1)
    
    MCsup <- replicate(M, {
      Z1 <- rnorm(d,0,sqrt(sigma[2]-sigma[3]))
      Z2 <- rnorm(p,0,sqrt(delta[1]))
      Z.hat <- Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
      Z.bar <- mean(Z.hat)
      Z.col <- sapply(1:d, function(i) mean(Z.hat[ij.l.mat[,i]]))
      Z.star <- (d-1)/(d-2)*(Z.col[ij.mat[,1]] + Z.col[ij.mat[,2]]) - d/(d-2)*Z.bar
      
      sapply(list(Z.hat - Z.bar,
                  Z.hat - Z.star,
                  Z.col - Z.bar),function(z) max(abs(z)))
    })
    
    MCsups <- replicate(M, {z <- rnorm(p); max(abs(z-mean(z)))})
    # print(difftime(Sys.time(),tiime))
    
    
    # tiime <- Sys.time()
    # Boot
    BOOTsup <- replicate(M, {
      sqrt(n)*max(abs(rnorm(n) %*% (t(Ths) - Tbs)/(n*(n-1))))
    })
    
    pvals.equal <- c(
      maha = pchisq(maha,p-1,lower.tail = F),
      maha_d = pchisq(maha_d,d-1,lower.tail = F),
      maha_p = pchisq(maha_p,p-d,lower.tail = F),
      euc = mean(MCeuc > euc),
      
      sups = mean(MCsups > sups),
      sup_d = mean(MCsup[3,] > sup_d),
      sup_p = mean(MCsup[2,] > sup_p),
      sup = mean(MCsup[1,] > sup),
      
      sup_boot = mean(BOOTsup > sup)
    )
    
  }else{
    pvals.equal <- pvals
  }
  
  rbind(pvals,pvals.equal)
}  



