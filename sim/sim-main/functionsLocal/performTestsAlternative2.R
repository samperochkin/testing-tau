performTestsAlternative2 <- function(X, epsilon, epsilon.vec, M){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  l.mat <- matrix(0,d,d)
  l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  HP <- computeHajekProjection(X,ij.mat)
  # Sigma.hats <- c(computeSigmaPlugin(X, ij.mat, tau.hat, T, l.mat),
  #                 computeSigmaJackknife(HP, ij.mat, T, l.mat))
  # Sigma.hats <- c(computeSigmaPlugin(X, ij.mat, tau.hat),
  #                 computeSigmaJackknife(HP, ij.mat))
  Sigma.hats <- computeSigmaJackknife(HP, ij.mat, T, l.mat)
  
  B <- rep(1,p)
  Bn <- B
  Bn[1] <- B[1] + epsilon/sqrt(n)
  IBB <- diag(p) - matrix(1/p,p,p)
  IBBn <- diag(p) - tcrossprod(Bn,Bn)/c(crossprod(Bn,Bn))
  
  tt <- sqrt(n)*c(IBB %*% tau.hat)
  loE <- c(crossprod(tt))
  loM <- max(abs(tt))
  
  
  do.call(rbind, lapply(names(Sigma.hats), function(nn){
    
    resTable <- data.table(S = rep("I", 2),
                           Sh = nn,
                           norm = c("Euclidean","Supremum"),
                           loss = as.numeric(NA),
                           pvalue_method = rep("Monte Carlo",2),
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
      
      # SI.star2 <- IBB %*% Sh2
      SI.star2 <- IBBn %*% Sh2
      IG <- diag(p)- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
      SSh.star <- Shi2 %*% IG %*% Sh2
      
      
      #### E -- S=I -- MC
      pv <- performMCAlternative(loE,SI.star2,"Euclidean",M,epsilon.vec,F)
      resTable[1, "pvalue" := pv]
      
      #### M -- S=I -- MC
      pv <- performMCAlternative(loM,SI.star2,"Supremum",M,epsilon.vec,F)
      resTable[2, "pvalue" := pv]
      
    }
    
    return(resTable)
  }))
}


