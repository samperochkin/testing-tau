# Hajek projection
gFun <- function(X, distribution, tau){
  
  if(is.vector(X)) X <- matrix(X,nrow=1)

  d <- ncol(X)
  ij.mat <- t(combn(d,2)) # ************************ MAKE SURE THAT's WHAT's used.
  
  # NOTE THAT X MUST HAVE VARIANCE = 1 (generate it accordingly)
  if(distribution == "normal"){
    Sig <- diag(2) + (1-diag(2))*sin(tau*pi/2)
    return(apply(ij.mat, 1, function(ij){
      # 2*(apply(X[,ij],1,function(x) pmvnorm(upper=x, corr=Sig)) + apply(X[,ij],1,function(x) pmvnorm(lower=x, corr=Sig))) - 1 - tau
      4*apply(X[,ij,drop=F],1,function(x) pmvnorm(upper=x, corr=Sig)) - 2*rowSums(pnorm(X[,ij,drop=F])) + 1 - tau
    }))
  } 
  
  if(distribution == "t4"){
    Sig <- diag(2) + (1-diag(2))*sin(tau*pi/2)
    return(apply(ij.mat, 1, function(ij){
      # 2*(apply(X[,ij],1,function(x) pmvt(upper=x, corr=Sig, df=4)) + apply(X[,ij],1,function(x) pmvt(lower=x, corr=Sig, df=4))) - 1 - tau
      4*apply(X[,ij],1,function(x) pmvt(upper=x, corr=Sig, df=4)) - 2*rowSums(pt(X[,ij],df=4)) + 1 - tau
    }))
  }
  
  if(distribution == "clayton"){
    claytonCop <- claytonCopula(tau2theta(tau,3))
    return(apply(ij.mat, 1, function(ij){
      4*pCopula(X[,ij,drop=F], claytonCop) - 2*rowSums(X[,ij,drop=F]) + 1 - tau
    }))
  }
  
  if(distribution == "gumbel"){
    gumbelCop <- gumbelCopula(tau2theta(tau,1))
    return(apply(ij.mat, 1, function(ij){
      4*pCopula(X[,ij,drop=F], gumbelCop) - 2*rowSums(X[,ij,drop=F]) + 1 - tau
    }))
  }
}