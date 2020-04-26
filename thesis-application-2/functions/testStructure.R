testStructure <- function(dend0, Tau.hat, Tau.hajek, M = 5000){
  n <- length(Tau.hajek)
  
  vec.address0 <- getAddresses(dend0)
  
  tau.stat <- constructTauBar(dend0,vec.address0,Tau.hat)
  tau.stat <- tau.stat[,1]-tau.stat[,2]
  
  tau.stat.hajek <- lapply(Tau.hajek, constructTauBar, dend0=dend0, vec.address0=vec.address0)
  tau.stat.hajek <- sapply(tau.stat.hajek, function(th) th[,1] - th[,2])
  
  boot <- replicate(M, {
    sqrt(n)*max(abs((tau.stat.hajek/(n*(n-1)) - tau.stat/n) %*% rnorm(n)))
  })
  
  mean(boot > sqrt(n)*max(abs(tau.stat)))
}
