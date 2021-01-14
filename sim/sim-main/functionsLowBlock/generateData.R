generateData <- function(n, d, design, dtau, dtau_type, distribution){
  
  if(is.null(dtau_type)) dtau_type <- "none"
  clus <- designToClus(d,design)
  
  # generate data
  if(distribution == "normal"){
    ij <- t(combn(d,2))
    kk <- matrix(clus[ij],ncol=2)
    g <- abs(kk[,1]-kk[,2])
    Sig <- diag(d)
    Sig[ij] <- Sig[ij[,2:1]] <- sin((.4 - .15*g)*pi/2)
    
    if(dtau_type == "single") Sig[cbind(c(1,2),c(2,1))] <- sin((.4 + dtau)*pi/2)
    
    X <- rmvnorm(n,rep(0,d),Sig)
    
  }
  return(X)
}