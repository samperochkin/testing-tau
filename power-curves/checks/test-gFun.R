library(magrittr)
library(mvtnorm)

N <- 1000
d <- 5
tau <- .5
distribution <- "normal"

source("sim/sim-main/functionsLow2/generateData.R")
X <- generateData(n=N, d=d, tau=tau, dtau=0, dtau_type="none", distribution=distribution)

source("sim/sim-main/powerCurves/functions/gFun.R")
gs <- gFun(X, distribution, tau)


if(is.vector(X)) X <- matrix(X,nrow=1)
d <- ncol(X)
ij.mat <- t(combn(d,2)) # ************************ MAKE SURE THAT's WHAT's used.
Sig <- diag(2) + (1-diag(2))*sin(tau*pi/2)
apply(ij.mat, 1, function(ij){
  4*apply(X[,ij,drop=F],1,function(x) pmvnorm(upper=x, corr=Sig))
}) %>% system.time()
apply(ij.mat, 1, function(ij){
  2*rowSums(pnorm(X[,ij,drop=F])) + 1 - tau
}) %>% system.time()


library(copula)

ij <- c(1,2)
apply(X[,ij,drop=F],1,function(x) pmvnorm(upper=x, corr=Sig)) %>% system.time()
i.lower <- rep.int(-Inf, 2)
apply(X[,ij], 1, function(x)
  pmvt(lower = i.lower, upper = x, sigma=Sig, df=Inf)
) %>% system.time()
pmvnorm(upper=t(X[,ij]), corr=Sig)




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

  
apply(ij.mat, 1, function(ij){
  4*apply(X[,ij,drop=F],1,function(x) pmvnorm(upper=x, corr=Sig))
}) %>% system.time()

4*apply(do.call("rbind",lapply(1:nrow(ij.mat), function(k) X[,ij.mat[k,]])),1,function(x) pmvnorm(upper=x, corr=Sig)) %>% system.time()

mapply(, 1, function(ij){
  4*apply(X[,ij,drop=F],1,function(x) pmvnorm(upper=x, corr=Sig))
}) %>% system.time()
