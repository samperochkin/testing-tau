performTestsAlternativeOracle <- function(X, epsilon, tau, M, dtau_type){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  l.mat <- matrix(0,d,d)
  l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  # Hajek projection
  HP <- computeHajekProjection(X,ij.mat)
  Sigma.hats <- computeSigmaJackknife(HP, ij.mat, T, l.mat)
  
  B <- rep(1,p)
  IBB <- diag(p) - matrix(1/p,p,p)

  tt <- sqrt(n)*c(IBB %*% tau.hat)
  loE <- c(crossprod(tt))
  loM <- max(abs(tt))
  

  do.call(rbind, lapply(names(Sigma.hats), function(nn){
    
    resTable <- data.table(S = rep(c("I","Sh"), each=2),
                           Sh = nn,
                           norm = rep(c("Euclidean","Supremum"), times=2),
                           loss = as.numeric(NA),
                           pvalue_method = rep("Monte Carlo",4),
                           pvalue = as.numeric(NA),
                           isShPd = as.logical(NA),
                           isShPsd = as.logical(NA),
                           rankSh = as.integer(NA))
    
    resTable[1:2, "loss" := c(loE,loM)]
    Sh <- Sigma.hats[[nn]]
    
    try(eig <- eigen(Sh, symmetric = T),silent=T)
    if(exists("eig")){
      
      resTable$isShPd <- !any(eig$values < 1e-6)
      resTable$rankSh <- as.integer(sum(eig$values > 1e-6))
      resTable$isShPsd <- !any(eig$values < -(1e-6))
      
      keep <- which(eig$values > 1e-6)
      Sh2 <- eig$vectors[,keep] %*% diag(sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
      Shi <- eig$vectors[,keep] %*% diag(1/eig$values[keep]) %*% t(eig$vectors[,keep])
      Shi2 <- eig$vectors[,keep] %*% diag(1/sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
      
      # S = I ------------------------------------------------
      SI.star2 <- IBB %*% Sh2
  
      # note that we use the true value of tau/beta to compute the bias term
      # We will however estimate Sigma...
      if(dtau_type == "single"){
        epsilon.vec <- epsilon*tau/p * c(p-1,rep(-1,p-1))
      }
      if(dtau_type == "column"){
        s1 <- p; s2 <- d-1
        # epsilon <- -epsilon for the column departure
        epsilon.vec <- -epsilon*tau/p^2 *
          c(rep( (d-1)*2*(s1-s2) + (p-d+1)*(s1-2*s2), d-1),
            rep( (d-1)*(s1-2*s2) + (p-d+1)*(-2)*s1, p-d+1))
      }
      
      #### E -- S=I -- MC
      pv <- performMCAlternative(loE,SI.star2,"Euclidean",M,epsilon.vec,F)
      resTable[1, "pvalue" := pv]
      
      #### M -- S=I -- MC
      pv <- performMCAlternative(loM,SI.star2,"Supremum",M,epsilon.vec,F)
      resTable[2, "pvalue" := pv]
      # -------------------------------------------------------
      
      
      # S = Sh ------------------------------------------------
      tt2 <- sqrt(n)*c(IG %*% tau.hat)
      loE2 <- c(crossprod(tt2))
      loM2 <- max(abs(tt2))
      
      G <- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
      IG <- diag(p) - G
      SSh.star <- Shi2 %*% IG %*% Sh2
      
      # bias term
      if(dtau_type == "single"){
        s1 <- sum(Shi)/n; s2 <- sum(Shi[,1])/n
        P <- matrix(0,d,d)
        P[1,1] <- 2*(s1 - s2)
        P[-1,1] <- P[1,-1] <- s1 - 2*s2
        P[-1,-1] <- -2*s2
        P <- epsilon/s1^2 * P %*% Shi/n
      }
      if(dtau_type == "column"){
        s1 <- sum(Shi)/n; s2 <- sum(Shi[,1:(d-1)])/n
        P <- matrix(0,d,d)
        P[1:(d-1),1:(d-1)] <- 2*(s1 - s2)
        P[-(1:(d-1)),(1:(d-1))] <- P[(1:(d-1)),-(1:(d-1))] <- s1 - 2*s2
        P[-(1:(d-1)),-(1:(d-1))] <- -2*s2
        # epsilon <- -epsilon for the column departure
        P <- -epsilon/s1^2 * P %*% Shi/n
      }
      
      # same here: we use tau directly, but Sh is estimated
      epsilon.vec2 <- tau * Shi2/sqrt(n) %*% rowSums(P)
      
      #### E -- S=Sh -- MC
      pv <- performMCAlternative(loE2,SI.star2,"Euclidean",M,epsilon.vec2,F)
      resTable[3, "pvalue" := pv]
      
      #### M -- S=Sh -- MC
      pv <- performMCAlternative(loM2,SI.star2,"Supremum",M,epsilon.vec2,F)
      resTable[4, "pvalue" := pv]
    }
    
    return(resTable)
  }))
}


