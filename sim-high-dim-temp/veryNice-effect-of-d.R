library(HAC)
library(mvtnorm)
library(pcaPP)
library(Matrix)

source("functions/generateData.R")
source("functions/buildSigma.R")
source("functions/computeTh.R")
source("functions/computeTh4.R")
source("functions/computeTh5.R")



pvals <- sapply(seq(5,40,5), function(d){
  replicate(750, {
    # print("hey")
    X <- generateData(n = 25,
                      d = d,
                      tau = .3,
                      dtau = 0,
                      #dtau_type = "column",
                      distribution = "joe")
    
    n <- nrow(X)
    d <- ncol(X)
    p <- d*(d-1)/2
    l.ij.mat <- t(combn(d,2))
    ij.l.mat <- matrix(0,d,d)
    ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
    
    Tau.hat <- cor.fk(X)
    tau.hat <- Tau.hat[l.ij.mat]
    tau.bar <- mean(tau.hat)
    
    B <- Matrix(0, nrow = p, ncol = d, sparse = T)
    for(i in 1:d){
      B[ij.l.mat[i,-i],i] <- 1
    }
    tb <- as.vector(B %*% (solve(crossprod(B)) %*% crossprod(B,tau.hat)))
    
    Tb <- diag(d)
    Tb[rbind(l.ij.mat,l.ij.mat[,2:1])] <- tb
    # image(t((Tau.hat - Tb)[d:1,]))
    # image(t((Tb - tau.bar)[d:1,]))
    
    s2 <- n*crossprod(tau.hat - tb)/(p-d)
    s1 <- n*crossprod(tb-tau.bar)/(d*(d-1))
    
    # ss <- c(s2+2*s1,s1)
    # Sh <- Th - 2*(2*n - 3) / (n*(n - 1)) * (tau.bar + 1)^2
    # Shh <- Matrix(data=0,nrow=p,ncol=p)
    # for(i in 1:2){
    #   Shh[which(BtB == 3-i)] <- ss[i]
    # }
    # # Shh <- Shh + sigma[3]
    # 
    # 
    # mahalanobis(tau.hat,rep(tau.bar,p),Sh)
    # mahalanobis(tau.hat,rep(tau.bar,p),Shh)
    
    
    # MONTE CARLO : very efficient.
    MC <- replicate(500, {
      Z1 <- rnorm(d,0,sqrt(s1))
      Z2 <- rnorm(p,0,sqrt(s2))
      
      Z <- Z2 + Z1[l.ij.mat[,1]] + Z1[l.ij.mat[,2]]
      
      max(abs(Z - mean(Z)))
    })
    
    mean(MC > sqrt(n)*max(abs(tau.hat-tau.bar)))
  })
})



dim(pvals)

pr <- c(.05,.25,.5,.75,.95)
q <- apply(pvals, 2, quantile, probs = pr)
q


plot(q[1,], ylim=c(0,1), type= "l")
apply(q[-1,], 1, lines)

abline(h=pr, col="green")

