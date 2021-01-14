performTestsLow3 <- function(X, M){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  l.mat <- matrix(0,d,d)
  l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  HP <- computeHajekProjection(X,ij.mat)
  Sigma.hats <- c(computeSigmaPlugin(X, ij.mat, tau.hat, F),
                  computeSigmaJackknife(HP, ij.mat, F))
  
  B <- rep(1,p)
  IBB <- diag(p) - matrix(1/p,p,p)
  
  tt <- sqrt(n)*c(IBB %*% tau.hat)
  loE <- c(crossprod(tt))
  loM <- max(abs(tt))
  
  
  do.call(rbind, lapply(names(Sigma.hats), function(nn){
    
    resTable <- data.table(S = rep(c("Sh"), each=4),
                           Sh = c(rep(nn,2),rep(paste0(nn,"near"),2)),
                           norm = rep(c("Euclidean","Supremum"), times=2),
                           loss = as.numeric(NA),
                           pvalue_method = rep("Monte Carlo",4),
                           pvalue = as.numeric(NA),
                           isShPd = as.logical(NA),
                           isShPsd = as.logical(NA),
                           rankSh = as.integer(NA))
    
    Sh <- Sigma.hats[[nn]]
    
    try(eig <- eigen(Sh, symmetric = T),silent=T)
    if(exists("eig")){
      
      resTable$isShPd <- !any(eig$values < 1e-6)
      resTable$rankSh <- as.integer(sum(eig$values > 1e-6))
      resTable$isShPsd <- !any(eig$values < -(1e-6))
      
      Rh <- as.matrix(Matrix::nearPD(Sh)$mat)

      keep <- which(eig$values > 1e-6)
      Sh <- eig$vectors[,keep] %*% diag(eig$values[keep]) %*% t(eig$vectors[,keep])
      Sh2 <- eig$vectors[,keep] %*% diag(sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
      Shi <- eig$vectors[,keep] %*% diag(1/eig$values[keep]) %*% t(eig$vectors[,keep])
      Shi2 <- eig$vectors[,keep] %*% diag(1/sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
      
      eig2 <- eigen(Rh, symmetric = T)
      Rh2 <- eig2$vectors %*% diag(sqrt(eig2$values)) %*% t(eig2$vectors)
      Rhi <- eig2$vectors %*% diag(1/eig2$values) %*% t(eig2$vectors)
      Rhi2 <- eig2$vectors %*% diag(1/sqrt(eig2$values)) %*% t(eig2$vectors)
      
      # SI.star2 <- IBB %*% Sh2 
      # RI.star2 <- IBB %*% Rh2
      # loE <- n*c(crossprod(IBB %*% tau.hat))
      # loM <- sqrt(n)*max(abs(IBB %*% tau.hat))
      # resTable[, "loss" := c(loE,loM,loE,loM)]
      
      IGS <- diag(p)- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
      IGR <- diag(p)- matrix(colSums(Rhi),p,p,byrow=T)/sum(Rhi)
      SI.star2 <- Shi2 %*% IGS %*% Sh2
      RI.star2 <- Rhi2 %*% IGR %*% Rh2
      loES <- n*c(crossprod(Shi2 %*% IGS %*% tau.hat))
      loMS <- sqrt(n)*max(abs(Shi2 %*% IGS %*% tau.hat))
      loER <- n*c(crossprod(Rhi2 %*% IGR %*% tau.hat))
      loMR <- sqrt(n)*max(abs(Rhi2 %*% IGR %*% tau.hat))
      resTable[, "loss" := c(loES,loMS,loER,loMR)]
      
      #### E -- S=I -- MC
      # pv <- performMC2(loES,SI.star2,"Euclidean",M,F)
      pv <- pchisq(loES,sum(eig$val > 1e-6)-1,lower.tail = F)
      resTable[1, "pvalue" := pv]
      
      #### M -- S=I -- MC
      pv <- performMC2(loMS,SI.star2,"Supremum",M,F)
      resTable[2, "pvalue" := pv]
      
      #### E -- S=I -- MC
      # pv <- performMC2(loER,RI.star2,"Euclidean",M,F)
      pv <- pchisq(loER,sum(eig2$val > 1e-6)-1,lower.tail = F)
      resTable[3, "pvalue" := pv]
      
      #### M -- S=I -- MC
      pv <- performMC2(loMR,RI.star2,"Supremum",M,F)
      resTable[4, "pvalue" := pv]
    }
    
    return(resTable)
  }))
}


