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
  
      if(dtau_type == "single"){
        dep_set <- 1
        S <- Si <- Si2 <- diag(p)
        ep <- rep(0,p)
        ep[dep_set] <- 1
      }
      if(dtau_type == "column"){
        dep_set <- d:p
        S <- Si <- Si2 <- diag(p)
        ep <- rep(0,p)
        ep[dep_set] <- 1
      }
      a <- sum(Si)
      b <- sum(Si[,dep_set])
      epsilon.vec <- epsilon * c(Si2 %*% (-b/a + ep))
      
      #### E -- S=I -- MC
      pv <- performMCAlternative(loE,SI.star2,"Euclidean",M,epsilon.vec,F)
      resTable[1, "pvalue" := pv]
      
      #### M -- S=I -- MC
      pv <- performMCAlternative(loM,SI.star2,"Supremum",M,epsilon.vec,F)
      resTable[2, "pvalue" := pv]
      # -------------------------------------------------------
      
      
      # S = Sh ------------------------------------------------
      G <- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
      IG <- diag(p) - G

      tt2 <- sqrt(n)*c(Shi2 %*% IG %*% tau.hat)
      loE2 <- c(crossprod(tt2))
      loM2 <- max(abs(tt2))
      resTable[3:4, "loss" := c(loE2,loM2)]
      
      SSh.star <- Shi2 %*% IG %*% Sh2

      # bias term
      if(dtau_type == "single"){
        dep_set <- 1
        Si <- Shi
        Si2 <- Shi2
        ep <- rep(0,p)
        ep[dep_set] <- 1
      }
      if(dtau_type == "column"){
        dep_set <- d:p
        Si <- Shi
        Si2 <- Shi2
        ep <- rep(0,p)
        ep[dep_set] <- 1
      }
      a <- sum(Si)
      b <- sum(Si[,dep_set])
      epsilon.vec2 <- epsilon * c(Si2 %*% (-b/a + ep))
      
      #### E -- S=Sh -- MC
      pv <- performMCAlternative(loE2,SSh.star,"Euclidean",M,epsilon.vec2,F)
      resTable[3, "pvalue" := pv]
      
      #### M -- S=Sh -- MC
      pv <- performMCAlternative(loM2,SSh.star,"Supremum",M,epsilon.vec2,F)
      resTable[4, "pvalue" := pv]
    }
    
    return(resTable)
  }))
}


