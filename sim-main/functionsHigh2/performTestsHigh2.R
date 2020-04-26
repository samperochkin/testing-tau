performTestsHigh2 <- function(X, M){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  l.mat <- matrix(0,d,d)
  l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  tau.col <- (colSums(Tau.hat)-1)/(d-1)
  tau.bar <- mean(tau.col)
  tau.star <- (d-1)/(d-2)*(tau.col[ij.mat[,1]] + tau.col[ij.mat[,2]]) - d/(d-2)*tau.bar
  
  # components of Hajek projection
  HP <- computeHajekProjection(X)
  
  Tcs <- sapply(HP, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(HP, function(Th) Th[ij.mat])
  Tbs <- colMeans(Ths)
  
  sigma.hat <- computeSigma3Jackknife(Ths,Tcs,Tbs)
  delta.hat <- c(rbind(c(p-2*d+3,2*(d-2),1),c(3-d,d-4,1),c(1,-2,1)) %*% sigma.hat)
  
  s_info <- ""
  if(sigma.hat[1] < 0) s_info <- paste0(s_info,"0")
  if(sigma.hat[2] < 0) s_info <- paste0(s_info,"1")
  if(sigma.hat[2] < sigma.hat[1]) s_info <- paste0(s_info,"+")
  
  resTable <- data.table(S = c("Sh","Sh-p","Sh-d","I","Sh","Sh-p","Sh-d","I"),
                         Sh = rep("SbJ",8),
                         norm = rep(c("Euclidean","Supremum"), each=4),
                         loss = as.numeric(NA),
                         pvalue_method = c(rep("Direct",3),"Monte Carlo (chi)",rep("Monte Carlo",4)),
                         pvalue = as.numeric(NA),
                         isShPd = rep(all(delta.hat > 0),8),
                         s_info = rep(s_info,8))
  
  
  
  #### losses
  
  euc <- c(n*crossprod(tau.hat-tau.bar))
  maha_p <- c(n*crossprod(tau.hat-tau.star))/delta.hat[3]
  maha_d <- (d-1)^2/(d-2)*c(n*crossprod(tau.col-tau.bar))/delta.hat[2]
  maha <- maha_d + maha_p  
  
  supI <- sqrt(n)*max(abs(tau.hat-tau.bar))
  if(delta.hat[3] > 0){
    supS_p <- sqrt(n)*max(abs(tau.hat-tau.star))/sqrt(delta.hat[3])
  }else{
    supS_p <- NA
  }
  if(delta.hat[2] > 0){
    supS_d <- sqrt(n)*max(abs(tau.star-tau.bar))/sqrt(delta.hat[2])
  }else{
    supS_d <- NA
  }
  if(all(delta.hat[2:3]>0)){
    supS <- sqrt(n)*max(abs((tau.star-tau.bar)/sqrt(delta.hat[2])+
                              (tau.hat-tau.star)/sqrt(delta.hat[3])))
  }else{
    supS <- NA
  }
  
  resTable$loss <- c(maha,maha_p,maha_d,euc,supS,supS_p,supS_d,supI)
  
  
  #### pvalues 
  
  resTable[c(1:3),]$pvalue <- c(pchisq(maha,p-1,lower.tail = F),
                                pchisq(maha_p,p-d,lower.tail = F),
                                pchisq(maha_d,d-1,lower.tail = F))
  
  resTable[4,]$pvalue <- mean(delta.hat[3]*rchisq(M,p-d) + delta.hat[2]*rchisq(M,d-1) > euc)
  
  
  Z <- matrix(rnorm(M*p),M,p)
  Zc <- sapply(1:d, function(k) rowMeans(Z[,l.mat[k,-k]]))
  Zb <- rowMeans(Z)
  
  Zp <- sqrt(delta.hat[3])*
    (Z - (d-1)/(d-2)*(Zc[,ij.mat[,1]] + Zc[,ij.mat[,2]]) + d/(d-2)*Zb)
  Zd <- sqrt(delta.hat[2])*
    ((d-1)/(d-2)*(Zc[,ij.mat[,1]] + Zc[,ij.mat[,2]]) - 2*(d-1)/(d-2)*Zb)
  Z <- Zp + Zd
  
  rm(Zc,Zb)
  
  resTable[5,]$pvalue <- mean(apply(matrix(rnorm(p*M),p,M), 2, function(z) max(abs(z-mean(z)))) > supS)
  resTable[6,]$pvalue <- mean(apply(abs(Zp)/sqrt(delta.hat[3]), 1, max) > supS_p)
  resTable[7,]$pvalue <- mean(apply(abs(Zd)/sqrt(delta.hat[2]), 1, max) > supS_d)
  resTable[8,]$pvalue <- mean(apply(abs(Z), 1, max) > supI)

  return(resTable)
}


