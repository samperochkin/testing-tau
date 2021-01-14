blockTest <- function(X,B,ij.mat,M){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  HP <- computeHajekProjection(X,ij.mat)
  Sigma.hats <- c(computeSigmaPlugin(X, ij.mat, tau.hat, F),
                  computeSigmaJackknife(HP, ij.mat, F))
  
  IBB <- diag(p) - B %*% MASS::ginv(B)
  tt <- sqrt(n)*c(IBB %*% tau.hat)
  loE <- c(crossprod(tt))
  loM <- max(abs(tt))
  
  do.call(rbind, lapply(names(Sigma.hats), function(nn){
    
    resTable <- data.table(S = rep(c("Sh","I"), each=2),
                           Sh = nn,
                           norm = rep(c("Euclidean","Supremum"), times=2),
                           loss = as.numeric(NA),
                           pvalue_method = c("Direct",rep("Monte Carlo",3)),
                           pvalue = as.numeric(NA),
                           isShPd = as.logical(NA),
                           isShPsd = as.logical(NA),
                           rankSh = as.integer(NA))
    
    resTable[3:4, "loss" := c(loE,loM)]
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
      
      SI.star2 <- IBB %*% Sh2 
      IG <- diag(p)- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
      SSh.star <- Shi2 %*% IG %*% Sh2
      
      
      #### E -- S=I -- MC
      pv <- performMC2(loE,SI.star2,"Euclidean",M,F)
      resTable[3, "pvalue" := pv]
      
      #### M -- S=I -- MC
      pv <- performMC2(loM,SI.star2,"Supremum",M,F)
      resTable[4, "pvalue" := pv]
      
      
      tt <- sqrt(n)*c(Shi2 %*% IG %*% tau.hat)
      
      #### E -- S=Sh -- Direct approximation
      lo <- c(crossprod(tt))
      pv <- pchisq(lo,resTable$rankSh[1]-1,lower.tail=F)
      resTable[1, c("loss","pvalue") := list(lo,pv)]
      
      #### M -- S=Sh -- MC
      lo <- max(abs(tt))
      pv <- performMC2(lo,SSh.star,"Supremum",M,F)
      resTable[2, c("loss","pvalue") := list(lo,pv)]
    }
    
    return(resTable)
  }))
}
