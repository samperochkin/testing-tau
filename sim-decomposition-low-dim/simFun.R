simFun <- function(n=100, d=10, k=NULL, tau=.5, dtau=0, distribution="normal", num_sim = 250, filename = "res", clus = NULL){
  #################
  #### SETUP 1 #################################################
  #################
  
  # packages
  require(parallel)
  require(data.table)
  require(mvtnorm)
  require(copula)
  require(HAC)
  require(pcaPP)

  # If parameter k is set, compute n or d as the proper ratio
  if(!is.null(k)){
    if(is.null(d)){
      d <- n/k
    }else{
      n <- k/d
    }
  }
  
  
  # simulation grid
  sim.grid <- as.data.table(
    expand.grid(
      n = n,
      d = d,
      tau = tau,
      dtau = dtau,
      distribution = distribution,
      num_sim = 1:num_sim
      )
    )
  
  # add unique ids
  sim.grid$ID <- 1:nrow(sim.grid)
  
  
  #################
  #### SETUP 2 #################################################
  #################
  
  # for parallel stuff
  
  if(is.null(clus)){
    clus <- makeCluster(detectCores()-1) 
  }else{
    clus <- makeCluster(clus)
  }

  clusterEvalQ(clus, expr={
    library(data.table)
    library(mvtnorm)
    library(copula)
    library(HAC)
    library(pcaPP)
    # library(matrixcalc)
    source("functions/computeTh.R")
    source("functions/buildSigma.R")
  })
  clusterExport(clus, varlist=c("sim.grid"))
  
  
  
  #################
  #### SIMULS #################################################
  #################
  
  res <- rbindlist(parLapply(clus, sample(nrow(sim.grid)), function(r){
    
    # assign parameters
    n <- sim.grid[r,]$n
    d <- sim.grid[r,]$d
    p <- d*(d-1)/2
    
    l.ij.mat <- t(combn(d,2))
    ij.l.mat <- matrix(0,d,d)
    ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
    
    tau <- sim.grid[r,]$tau
    dtau <- sim.grid[r,]$dtau
    
    
    # generate data
    if(sim.grid[r,]$distribution == "normal"){
      
      Sig <- diag(d) + (1-diag(d))*sin(tau*pi/2)
      Sig[cbind(c(1,2),c(2,1))] <- sin((tau + dtau)*pi/2)
      X <- rmvnorm(n,rep(0,d),Sig)
      
    }else if(sim.grid[r,]$distribution == "joe"){
      
      if(dtau == 0){
        X <- rHAC(n, hac(8,c(as.list(as.character(1:d)),tau2theta(tau,8))))
      }else{
        tree <- c(list(list(1,2,tau2theta(tau+dtau,8))),as.list(c(3:d,tau2theta(tau,8))))
        X <- rHAC(n, hac(8,tree))
      }

    }
    

    # compute estimates
    Tau.hat <- cor.fk(X)
    tau.hat <- Tau.hat[l.ij.mat]
    tau.bar <- mean(tau.hat)
    
    Sh <- buildSigma(computeTh(X),rep(tau.bar,p),n,F) 
    
    kl.mat <- as.matrix(expand.grid(1:nrow(l.ij.mat),1:nrow(l.ij.mat)))
    type <- apply(kl.mat, 1, function(kl) 4-length(unique(c(l.ij.mat[kl,]))))
    for(i in 1:3){
      Sh[kl.mat[type==(i-1),]] <- mean(Sh[kl.mat[type==(i-1),]])
    }

    
    # special: compute decomposition 
    B <- matrix(0,p,d)
    for(i in 1:d){
      B[ij.l.mat[i,-i],i] <- 1
    }
    G <- B %*% solve(tcrossprod(t(B))) %*% t(B)
    
    # compute losses
    maha <- mahalanobis(tau.hat,tau.bar,Sh)
    maha3 <- mahalanobis(c(tau.hat %*% (diag(p)-G)),F,Sh)
    maha2 <- mahalanobis(c(tau.hat %*% (G-1/p)),F,Sh)
    
    euc <- crossprod(tau.hat-tau.bar)
    euc3 <- crossprod(c(tau.hat %*% (diag(p)-G)))
    euc2 <- crossprod(c(tau.hat %*% (G-1/p)))
    
    res.grid <- sim.grid[r,]
    res.grid[, ':='(maha = maha, maha2 = maha2, maha3 = maha3,
                    euc = euc, euc2 = euc2, euc3 = euc3,
                    sigma2 = Sh[1,1], sigma1 = Sh[1,2], sigma0 = Sh[1,p])]
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}