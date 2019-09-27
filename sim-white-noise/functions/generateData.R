generateData <- function(n, d, tau, distribution){
  
  # generate data
  if(distribution == "normal"){
    
    Sig <- diag(d) + (1-diag(d))*sin(tau*pi/2)
    ij <- t(combn(d,2))
    Sig[rbind(ij,ij[,2:1])] <- Sig[ij] + runif(nrow(ij),-.1,.1)
    
    Sig <- as.matrix(nearPD(Sig, ensureSymmetry = F)$mat)
    # image(Sig, zlim = c(-1,1))
    # is.positive.definite(Sig)
    
    X <- rmvnorm(n,rep(0,d),Sig)
    
  }
  return(X)
}