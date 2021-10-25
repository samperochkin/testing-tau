# pre-processed data
Y <- readRDS("data/Y.rds")
clus <- readRDS("data/clus.rds")
sapply(list.files("C:/Users/Samuel/Gits/testing-tau/sim-main/functionsLowBlock",full.names=T), source, local=environment())

library(data.table)
res <- performTestsLowBlock(Y,clus,10000)
max(res$pvalue)


X <- Y
M <- 10000
# function(X, clus, M){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  K <- length(unique(clus))
  l <- apply(matrix(clus[ij.mat],ncol=2),1,sort)
  l <- (l[1,]-1)*K - choose(l[1,]-1,2)  + (l[2,]-l[1,]+1)
  B <- matrix(0,p,max(l))
  B[cbind(1:p,l)] <- 1
  if(any(colSums(B) == 0)) B <- B[,-which(colSums(B) == 0)]
  IBB <- diag(p) - tcrossprod(B)/rowSums(tcrossprod(B))
  
  HP <- computeHajekProjection(X,ij.mat)
  Sigma.hats <- c(computeSigmaPlugin(X, ij.mat, tau.hat, T, clus),
                  computeSigmaJackknife(HP, ij.mat, T, clus))
  
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
      IG <- diag(p) - B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shi
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
      pv <- pchisq(lo,resTable$rankSh[1]-ncol(B),lower.tail=F)
      resTable[1, c("loss","pvalue") := list(lo,pv)]
      
      #### M -- S=Sh -- MC
      lo <- max(abs(tt))
      pv <- performMC2(lo,SSh.star,"Supremum",M,F)
      resTable[2, c("loss","pvalue") := list(lo,pv)]
    }
    
    return(resTable)
  }))
# }
