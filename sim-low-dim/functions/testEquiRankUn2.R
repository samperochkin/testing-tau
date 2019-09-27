testEquiRankUn2 <- function(X){

  # setup -------------------------------------------------------------------
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  l.ij.mat <- t(combn(d,2))
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
  

# computation of estimates ------------------------------------------------
  Tau.hat <- cor.fk(X)
  tau.hat <- Tau.hat[l.ij.mat]

  Sh <- buildSigma(computeTh(X),tau.hat,n,F) 
  Shi <- solve(Sh)

  B <- rep(1,p)
  G <- B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shi
  tau.tilde <- c(G %*% tau.hat)

  # G <- colSums(Shi)/sum(Shi)
  # tau.tilde <- rep(G %*% tau.hat,p)
  # tau.tilde
  
  # lo_maha <- mahalanobis(tau.hat,tau.bar,Shi,T)
  # lo_maha3 <- mahalanobis(c(tau.hat %*% (diag(p)-G)),F,Shi,T)
  # lo_maha2 <- mahalanobis(c(tau.hat %*% (G-1/p)),F,Shi,T)
  
  lo_euc <- c(n*crossprod(tau.hat-tau.tilde))
  # lo_euc3 <- c(n*crossprod(c(tau.hat %*% (diag(p)-G))))
  # lo_euc2 <- c(n*crossprod(c(tau.hat %*% (G-1/p))))
  
  lo_sup <- sqrt(n)*max(abs(tau.hat-tau.tilde))
  # lo_sup3 <- sqrt(n)*max(abs(tau.hat %*% (diag(p)-G)))
  # lo_sup2 <- sqrt(n)*max(abs(tau.hat %*% (G-1/p)))
  
  # cShi <- chol(Shi) # cholesky for sqrt of Shi
  # lo_supS <- max(abs(cShi %*% (tau.hat-tau.tilde)))
  # lo_supS3 <- max(abs(cShi %*% ((diag(p)-G) %*% tau.hat)))
  # lo_supS2 <- max(abs(cShi %*% ((G-1/p) %*% tau.hat)))
  
  
  loss <- c(#lo_maha, lo_maha2, lo_maha3,
            lo_euc, #lo_euc2, lo_euc3,
            lo_sup#, lo_sup2, lo_sup3,
            #lo_supS, lo_supS2, lo_supS3
            )

  
# computation of p-values -------------------------------------------------
  
  # delta2 <- sigma2 + (d-4)*sigma1 - (d-3)*sigma0
  # delta3 <- sigma2 - 2*sigma1 + sigma0
  # delta <- c(delta2,delta3)
  
  # mahas
  # al_maha <- pchisq(lo_maha, p-1, lower.tail = F)
  # al_maha2 <- pchisq(lo_maha2, d-1, lower.tail = F)
  # al_maha3 <- pchisq(lo_maha3, p-d, lower.tail = F)
  
  # euc -- Monte Carlo 
  
  lam <- eigen(sqrt(n)*(diag(p) - G) %*% Sh %*% (diag(p) - G))$val
  mc <- matrix(rchisq(p*10000,df=1),10000,p) %*% lam*n
  
  al_euc <- mean(mc > lo_euc)
  # al_euc2 <- mean(mc[1,] > lo_euc2)
  # al_euc3 <- mean(mc[2,] > lo_euc3)
  
  # supS 
  # mc <- replicate(10000, {
  #   z <- rnorm(p)
  #   z.bar <- mean(z)
  #   z.star <- G %*% z
  #   
  #   c(max(abs(z-z.bar)),
  #     max(abs(z.star-z.bar)),
  #     max(abs(z-z.star)))
  # })
  # 
  # al_supS <- mean(mc[1,] > lo_supS)
  # al_supS2 <- mean(mc[2,] > lo_supS2)
  # al_supS3 <- mean(mc[3,] > lo_supS3)

  # sup -- could be done much more efficiently
  z <- sqrt(n)*rmvnorm(10000,rep(0,p),Sh) # weak part -- can use latent factor representation
  z.bar <- c(z %*% G)
  # z.star <- z %*% G
  
  mc <- apply(z-z.bar, 1, function(v) max(abs(v)))
  # mc2 <- apply(z.star-z.bar, 1, function(v) max(abs(v)))
  # mc3 <- apply(z-z.star, 1, function(v) max(abs(v)))
  
  al_sup <- mean(mc > lo_sup)
  # al_sup2 <- mean(mc2 > lo_sup2)
  # al_sup3 <- mean(mc3 > lo_sup3)
  
  # vector of p-values
  pval <- c(#al_maha,al_maha2,al_maha3,
            al_euc,#al_euc2,al_euc3,
            al_sup#,al_sup2,al_sup3,
            #al_supS,al_supS2,al_supS3
            )
  pval
  
  test_type <- c(#"maha","maha2","maha3",
            "euc",#"euc2","euc3",
            "sup"#,"sup2","sup3",
            #"supS","supS2","supS3"
            )
  
  data.table(test_type = test_type,
             loss = loss,
             pval = pval)
}
