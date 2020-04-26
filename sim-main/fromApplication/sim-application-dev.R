n <- 65
Tau <- readRDS("fromApplication/Tt.rds")
d <- nrow(Tau)
p <- d*(d-1)/2

clus <- readRDS("fromApplication/clus.rds")
ij.mat <- cbind(unlist(sapply(1:d, function(i) seq(i)[-i])),
                unlist(sapply(2:d, function(i) rep(i,i-1))))
K <- length(unique(clus))
ds <- table(clus)
rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                unlist(sapply(2:K, function(i) rep(i,i-1))))
ps <- tcrossprod(ds)[rs.mat] # block sizes
L <- p - sum(ps) + nrow(rs.mat)
ij.rs.mat <- matrix(clus[ij.mat], ncol=2)
B <- t(apply(ij.rs.mat,1,function(rs){
  v <- rep(0,L)
  if(rs[1] == rs[2]) return(v)
  v[cumsum(c(0,K:2 - 1))[rs[1]] + (rs[2] - rs[1])] <- 1
  v
}))
B[cbind(which(rowSums(B)==0),(nrow(rs.mat)+1):L)] <- 1
colSums(B)


blockTest <- function(X,B,M){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  
  HP <- computeHajekProjection(X,ij.mat)
  Sigma.hats <- c(computeSigmaPlugin(X, ij.mat, tau.hat, F),
                  computeSigmaJackknife(HP, ij.mat, F))
  IBB <- diag(p) - B %*% MASS::ginv(B)
  
  do.call(rbind, lapply(names(Sigma.hats), function(nn){
    
    Sh <- Sigma.hats[[nn]]
    
    isPD <- matrixcalc::is.positive.definite(Sh)
    isPSD <- matrixcalc::is.positive.semi.definite(Sh)
    
    if(isPSD){
      eig <- eigen(Sh)
      keep <- which(eig$values > 1e-8)
      Shi <- eig$vectors[,keep] %*% diag(1/eig$values[keep]) %*% t(eig$vectors[,keep])
      Shi2 <- eig$vectors[,keep] %*% diag(1/sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
      
      IG <- diag(p)- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
      SSh.star <- diag(p)-tcrossprod(rowSums(Shi2))/sum(Shi)
      
      SI.star <- IBB %*% Sh %*% IBB
      SI.star <- (SI.star + t(SI.star))/2
      eig <- eigen(SI.star)
      keep <- which(eig$values > 1e-8)
      SI.star2 <- eig$vectors[,keep] %*% diag(sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
    }
    
    resTable <- data.table(S = rep(c("Sh","I"), each=2),
                           Sh = nn,
                           norm = rep(c("Euclidean","Supremum"), times=2),
                           loss = as.numeric(NA),
                           pvalue_method = c("Direct",rep("Monte Carlo",3)),
                           pvalue = as.numeric(NA),
                           isShPd = isPD)
    
    
    if(isPSD){
      tt <- sqrt(n)*c(Shi2 %*% IG %*% tau.hat)
      
      #### E -- S=Sh -- Direct approximation
      lo <- c(crossprod(tt))
      pv <- pchisq(lo,p-1,lower.tail=F)
      resTable[1, c("loss","pvalue") := list(lo,pv)]
      
      #### M -- S=Sh -- MC
      lo <- max(abs(tt))
      pv <- performMC(lo,SSh.star,"Supremum",M)
      resTable[2, c("loss","pvalue") := list(lo,pv)]
    }
    
    tt <- sqrt(n)*c(IBB %*% tau.hat)
    loE <- c(crossprod(tt))
    loM <- max(abs(tt))
    resTable[3:4, "loss" := c(loE,loM)]
    
    if(isPSD){
      #### E -- S=I -- 
      pv <- performMC(loE,SI.star,"Euclidean",M)
      resTable[3, "pvalue" := pv]
      
      #### M -- S=I -- MC
      pv <- performMC(loM,SI.star,"Supremum",M)
      resTable[4, "pvalue" := pv]
    }
    
    return(resTable)
  }))
}

N <- 500
res <- rbindlist(lapply(1:N, function(dummy){
  X <- rmvnorm(n,rep(0,d),sin(Tau*pi/2))
  blockTest(X,5000)
}))

plot((1:N)/(N+1),sort(res[S == "I" & Sh == "ShJ" & norm == "Euclidean"]$pvalue), type="l", col = "blue")
lines((1:N)/(N+1),sort(res[S == "I" & Sh == "ShJ" & norm == "Supremum"]$pvalue), type="l", col = "red")
abline(a=0,b=1)

