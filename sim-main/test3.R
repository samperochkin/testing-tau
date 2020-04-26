
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunLow.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
# packages
library(mvtnorm)
library(data.table)
sapply(list.files("functionsLow/", full.names = T), source, local = environment())

# simuls
n = c(50)
d = c(25)
tau = c(0)
dtau = c(0)
dtau_type <- "none"
distribution = c("normal")
M <- 5000
p <- d*(d-1)/2
ij.mat <- t(combn(d,2))
l.mat <- matrix(0,d,d)
l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p
B <- rep(1,p)
IBB <- diag(p) - matrix(1/p,p,p)

se <- 0
while(T){
  se <- se+1
  set.seed(se)
  X <- generateData(n,d,tau,dtau,dtau_type,distribution)
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  HP <- computeHajekProjection(X,ij.mat)
  Sigma.hats <- c(computeSigmaPlugin(X, ij.mat, tau.hat, T, l.mat),
                  computeSigmaJackknife(HP, ij.mat, T, l.mat))
  
  for(nn in names(Sigma.hats)){
    Sh <- Sigma.hats[[nn]]
    
    isPD <- matrixcalc::is.positive.definite(Sh)
    isPSD <- matrixcalc::is.positive.semi.definite(Sh)
    
    if(isPSD) try(eig <- eigen(Sh, symmetric = T),silent=T)
    
    if(exists("eig")){
      keep <- which(eig$values > 1e-8)
      Shi <- eig$vectors[,keep] %*% diag(1/eig$values[keep]) %*% t(eig$vectors[,keep])
      Shi2 <- eig$vectors[,keep] %*% diag(1/sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
      
      IG <- diag(p)- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
      SSh.star <- diag(p)-tcrossprod(rowSums(Shi2))/sum(Shi)
      
      SI.star <- IBB %*% Sh %*% IBB
      SI.star <- (SI.star + t(SI.star))/2
    }
    
    if(exists("eig")){
      tt <- sqrt(n)*c(Shi2 %*% IG %*% tau.hat)
      
      #### E -- S=Sh -- Direct approximation
      lo <- c(crossprod(tt))
      pchisq(lo,p-1,lower.tail=F)
      lo <- max(abs(tt))
      performMC(lo,SSh.star,"Supremum",M)
    }
    
    tt <- sqrt(n)*c(IBB %*% tau.hat)
    loE <- c(crossprod(tt))
    loM <- max(abs(tt))

    if(exists("eig")){
      performMC(loE,SI.star,"Euclidean",M)
      performMC(loM,SI.star,"Supremum",M)
    }
    
  }
}
difftime(Sys.time(),tiime)
