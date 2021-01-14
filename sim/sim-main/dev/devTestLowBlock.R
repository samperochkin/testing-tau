library(mvtnorm)
sapply(list.files("sim-main/functionsLowBlock/", full.names = T), source, local=environment())
clus <- c(sapply(1:4, rep, times = 3))

res <- do.call("rbind",lapply(1:500,function(dummy){
  X <- generateData(n=250, d=length(clus), tau=.25, dtau=0, dtau_type = "none", distribution = "normal")
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  
  ij.mat <- t(combn(d,2))
  l.mat <- matrix(0,d,d)
  l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p
  
  Tau.hat <- pcaPP::cor.fk(X)
  tau.hat <- Tau.hat[ij.mat]
  tau.bar <- mean(tau.col)

  # components of Hajek projection
  HP <- computeHajekProjection(X)
  
  Ths <- sapply(HP, function(Th) Th[ij.mat])
  Tbs <- colMeans(Ths)
  
  ShJs <- computeSigmaJackknife(HP,ij.mat)
  
  sigma.hat <- computeSigma3Jackknife(Ths,Tcs,Tbs)
  delta.hat <- c(rbind(c(p-2*d+3,2*(d-2),1),c(3-d,d-4,1),c(1,-2,1)) %*% sigma.hat)
  
  resTable <- data.table(S = c("Sh","Sh-p","Sh-d","I","Sh","Sh-p","Sh-d","I",rep("I",2)),
                         Sh = c(rep("SbJ",8),rep("ShJ",2)),
                         norm = c(rep(c("Euclidean","Supremum"), each=4),c("Euclidean","Supremum")),
                         loss = as.numeric(NA),
                         pvalue_method = c(rep("Direct",3),"Monte Carlo (chi)",rep("Monte Carlo",4),rep("bootstrap",2)),
                         pvalue = as.numeric(NA),
                         isShPd = c(rep(all(delta.hat > 0),8),rep(NA,2)))
  
  
  
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
  
  resTable$loss <- c(maha,maha_p,maha_d,euc,supS,supS_p,supS_d,supI,euc,supI)
  
  
  #### pvalues 
  
  resTable[c(1:3),]$pvalue <- c(pchisq(maha,p-1,lower.tail = T),
                                pchisq(maha_p,p-d,lower.tail = T),
                                pchisq(maha_d,d-1,lower.tail = T))
  
  resTable[4,]$pvalue <- mean(delta.hat[3]*rchisq(M,p-d) + delta.hat[2]*rchisq(M,d-1) > euc)
  
  if(sigma.hat[2]-sigma.hat[1] > 0){
    Z1 <- replicate(M, {rnorm(d,0,sqrt(sigma.hat[2]-sigma.hat[1]))})
    Z2 <- replicate(M, {rnorm(p,0,sqrt(delta.hat[3]))})
    Z.hat <- Z2 + Z1[ij.mat[,1],] + Z1[ij.mat[,2],]
    rm(Z1,Z2)
    
    Z.bar <- colMeans(Z.hat)
    Z.col <- t(sapply(1:d, function(i) colMeans(Z.hat[l.mat[,i],])))
    Z.star <- t(t((d-1)/(d-2)*(Z.col[ij.mat[,1],] + Z.col[ij.mat[,2],])) - d/(d-2)*Z.bar)
    
    resTable[5,]$pvalue <- mean(apply(matrix(rnorm(p*M),p,M), 2, function(z) max(abs(z-mean(z)))) > supS)
    resTable[6,]$pvalue <- mean(apply(abs(Z.hat - Z.star), 2, max) > supS_p)
    resTable[7,]$pvalue <- mean(apply(abs(t(Z.star) - Z.bar), 1, max) > supS_d)
    resTable[8,]$pvalue <- mean(apply(abs(t(Z.hat) - Z.bar), 1, max) > supI)
  }
  
  TT <- (Ths - matrix(Tbs,p,n,byrow=T))/sqrt(n-1) - (tau.hat-tau.bar)/sqrt(n)
  boot <- replicate(M, {
    zz <- (TT %*% rnorm(n))
    
    c(euc=crossprod(zz),
      supI=max(abs(zz)))
  })
  
  resTable[9:10,]$pvalue <- c(mean(boot[1,] > euc),
                              mean(boot[2,] > supI))
  resTable
}))


library(ggplot2)
ggplot(res, aes(x=pvalue)) +
  geom_histogram(breaks=seq(0,1,.1)) + facet_grid(norm~Sh+S, scales="free")

# hist(res[S == "Sh-p" & Sh == "SbJ" & norm == "Supremum",loss],breaks=100)
# hist(res[S == "Sh-p" & Sh == "SbJ" & norm == "Supremum",pvalue],breaks=100)
# lo <- res[S == "Sh-p" & Sh == "SbJ" & norm == "Supremum",]$loss
# hist(pchisq(lo*sqrt(n),p-d),breaks=100)
